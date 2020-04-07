##### US DATA COLLECTOR ########################################################

# This script collects, updates and harmonises data on the US economy from
# a wide variety of sources. To be selfcontained and reused within other
# scripts, it also contains all functions and flags subsequently used

# Written by Emanuele Franceschi - 07/04/2020 v1


##### I - create directories ###################################################
working_directory <- getwd()
temp_dir <- 'downloaded_files'
data_dir <- 'processed_data'
graphs_dir <- 'plots'

temp_dir <- file.path(working_directory, temp_dir)
data_dir <- file.path(working_directory, data_dir)
graphs_dir <- file.path(working_directory, graphs_dir)

options(warn=-1) # turns off warnings momentarily
dir.create(temp_dir)
dir.create(data_dir)
dir.create(graphs_dir)
options(warn=0) # turns warnings back on

##### II - Custom functions ####################################################

# install/load packages
instant_pkgs <- function(pkgs) { 
  ## Function loading or installing packages in
  ## current R instance.
  ## Developed by Jaime M. Montana Doncel - V1
  
  
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  if (length(pkgs_miss) == 0) {
    message("\n ...Packages were already installed!\n")
  }
  
  # install packages not already loaded:
  pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
  if (length(pkgs_miss) > 0) {
    install.packages(pkgs_miss)
  }
  
  # load packages not already loaded:
  attached <- search()
  attached_pkgs <- attached[grepl("package", attached)]
  need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
  
  if (length(need_to_attach) > 0) {
    for (i in 1:length(need_to_attach))  suppressPackageStartupMessages(library(need_to_attach[i], character.only = TRUE))
  }
  
  if (length(need_to_attach) == 0) {
    message("\n ...Packages were already loaded!\n")
  }
}

##### Packages Loader #####
# fill pkgs with names of the packages to install
instant_pkgs(
			c(
		  'glue', 
          'lazyeval',
          'quantreg', 
          'tidyverse',
          'devtools',
          'tseries',
          'stargazer',
          'xts',
          'MASS',
          'car',
          'rvest',
          'mFilter',
          'fredr',
          'readr',
          'quantmod',
          'devtools',
          'lubridate',
          'readxl',
          'tbl2xts',
          'tictoc',
          'httr'
          )
		)

subfilter <- function(df){
  # function to convert a df with multiple observations per unit
  # of time in a df with one observation per unit of time,
  # namely the last one among those previously present
  
  
  indice <- as.character(unique(df$date))
  len <- length(indice)
  outp <- matrix(NA, ncol=ncol(df), nrow=len)
  outp <- data.frame(outp)
  names(outp) <- names(df)
  for (i in 1:len){
    supp <- indice[i]
    ram <- subset(df, date==supp)
    outp[i,] <- ram[nrow(ram),]
    outp[i,1] <- indice[i]
  }
  return(outp)
}


subfilter.mean <- function(df){
  # function to convert a df with multiple observations per unit
  # of time in a df with one observation per unit of time,
  # namely the mean of those previously present
  
  
  indice <- as.character(unique(df$date))
  len <- length(indice)
  outp <- matrix(NA, ncol=ncol(df), nrow=len)
  outp <- data.frame(outp)
  names(outp) <- names(df)
  for (i in 1:len){
    supp <- indice[i]
    ram <- subset(df, date==supp)
    outp[i,] <- c(0, as.numeric(apply(ram[,-1], 2, mean)))
  }
  outp[,1] <- indice
  return(outp)
}


trendev<-function(mat){
  # for multiple observation in particular shape, this function
  # estimates a quadratic trend on the available series and consider
  # the deviation from the trend in the last observation. This deviation
  # is put into another time series. The purpose of this function is to
  # extract real time output gap from Philadelphia dataset.
  
  
  matdat<-mat[,2:ncol(mat)]
  temp<-1:nrow(mat)
  temp2<-temp^2
  regr<-function(x){
    dta<-data.frame(x, temp, temp2)
    names(dta)<-c('x', 'temp', 'temp2')
    model<-lm(x~temp+temp2, data=dta)
    GAPS<-(model$residuals/(x-model$residuals))
    gaps<-as.matrix(na.omit(GAPS))
    gap<-gaps[nrow(gaps)]
    return(gap)
  }
  outcome<-apply(matdat, 2, regr)
  outcome<-as.matrix(outcome)
  return(outcome*100)
}

spf_funct <-  function(filnam, typs, ahead=1) {
  # this function imports the files, reformats,
  # renames, saves in raw format and produces
  # aggregate statistics in XTS format
  
  # read in xlsx files and reshape w\ spread
  # this block selects one quarter ahead forecasts
  # but adjusting 'ahead' parameter below one can
  # extract other values
  
  # ad-hoc function inconsistent w/ external use
  # typs is one of CPI, CORECPI, PCE, COREPCE
  
  
  # 'ahead' allows to select the horizon of 
  # forecasts one wishes to extract:
  # -1 for previous quarter estimates
  # 0 for nowcast
  # 1 for one quarter ahead -- default
  # 2 for two quarters ahead
  # 3 for three quarters ahead
  # 4 for one year ahead
  
  typ=tolower(typs)
  
  colu=c(rep('numeric',3),  # picks year, quarter, ID
         rep('skip', 2+ahead),	 # skips industry
         'numeric',				 # moving target picking 'ahead' horizon
         rep('skip', 7-ahead)	 # skips the rest
  )
  
  df=read_excel(file.path(temp_dir,filnam), 
                na='#N/A', col_types=colu) %>%
    spread(ID, paste0(typs,ahead+2)) %>% 
    ts(start=c(1968, 4), frequency=4) %>%
    as.xts()
  
  pst=paste0(typ,'_')
  if (ahead==-1){
    pst=paste0(pst,'b1')
  } 	else {
    pst=paste0(pst,'h') %>% paste0(ahead)
  }
  
  names(df)=c('year', 'quarter', paste(pst, (1:(ncol(df)-2)), sep='_'))
  
  df$year <- df$quarter <- NULL
  
  # saving in txt csv format the raw data
  write.zoo(df, file.path(data_dir, paste(paste0('SPF_IND_',pst),'txt', sep='.')), sep=';', row.names=F, index.name='time')
  
  
  iqr <- apply(df, 1, IQR, na.rm=TRUE) %>% ts(start=c(1968, 4), frequency=4) %>% as.xts()
  stand<-apply(df, 1, var, na.rm=T) %>% sqrt()%>% ts(start=c(1968, 4), frequency=4) %>% as.xts()
  mean<-apply(df, 1, mean, na.rm=T)%>% ts(start=c(1968, 4), frequency=4) %>% as.xts()
  mean[is.nan(mean)] <- NA
  
  lab <- paste0('spf_', pst)
  
  df_stat=merge(iqr, stand, mean)
  names(df_stat)=paste(lab, c('iqr', 'sd', 'mean'), sep='_')
  
  
  return(df_stat)
}


hamil_filter <- function(tseries, log=FALSE, p = 4, h = 8){
  
  # test code 
  # 
  # tseries <- c(rep(NA, 2), rnorm(200, 3, 3), rep(NA, 8))
  # 
  # tseries <- ts(tseries, frequency = 4, start = c(1900, 01))
  # 
  # h <- 8
  # p <- 4
  
  
  # R implementation of Hamilton's replacement
  # for time series filtering, to use for the same
  # purposes of Hodrick-Prescott Filter.
  #
  # Reference: James Hamilton, "Why you should never use the Hodrick-Prescott Filter", 2017, NBER Working Paper
  
  # ts: the time series to filter out of the trend
  # p : the number of lags to include 
  # h : the forward term
  
  # the model to estimate is then:
  # y_{t+h} = \alpha + \beta_1 y_{t} + \beta_2 y_{t-1} + ... + \beta_p y_{t-p}
  #
  # and this function will output the residuals of this regression
  
  
  ##### Libraries #####
  if (!require(xts)){install.packages('xts')}
  
  library(xts)
  
  if (log) tseries <- log(tseries)
  
  if (class(tseries)[1] %in% c('zoo', 'ts', 'xts')){
    #### Prepping data ####
    
    # keep the time index
    time_ind <- time(tseries)
    
    # get rid of leading and trailing NA's
    ts <- na.trim(tseries)
    
    time_ind_trim <- time(ts)
    
    # count remaining NA, barf in case
    nas_count <- sum(is.na(ts))
    
    if (nas_count>=1) stop('NAs in the series!') 
    if (length(ts)<= h+p) stop('Too few observations: you might want to decrease p and h.')
    
    
    # lag data
    lagged_ts <- embed(ts, dimension = h+p-1)
    lagged_ts <- as.data.frame(lagged_ts)
    names(lagged_ts) <- paste0('x', 1:(h+p-1))
    
    # dump useless cols
    lagged_ts <- lagged_ts[,-(2:(h-1))]
    
    #### Running lm's ####
    model <- lm(lagged_ts)
    residuals <- resid(model)*100/model$model$x1
    
    
    #### date up correctly residuals ####
    residuals <- xts(residuals, order.by = as.Date(time_ind_trim)[-(1:(p+h-2))])
    
    return(residuals)
  }else{warning('Provide a time series object!')}
}




##### III - Actual data collection #############################################

# pick ahead to set how many quarters ahead 
# to consider for SPF forecasts:
# -1 for previous quarter estimates
# 0 for nowcast
# 1 for one quarter ahead -- default
# 2 for two quarters ahead
# 3 for three quarters ahead
# 4 for one year ahead
ahead <- 1

#### FEDERAL INTEREST RATE ####

fredr_set_key('5d4b5f1e6667727ee4ea90affbad1e6a')
# key for the FRED API

ffr <- fredr_series_observations(series_id='FEDFUNDS', frequency='m') %>% tbl_xts()
ffr <- as.xts(aggregate(ffr, as.yearqtr(as.yearmon(time(ffr))), last))
# aggregates up to quarters picking quarter's last month value

ffrb <- lag(ffr)

ffrate <- merge(ffr, ffrb)

#### INFLATION FORECASTS & REVISED ####

# downloads the big xlsx Greenbook file in
# a specifically created folder
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/greenbook-data/documentation/gbweb_row_format.xls?la=en',
              file.path(temp_dir,'Greenbook_allvar_row.xlsx'), mode='wb',
              extra='--no-check-certificate',
              quiet = T)

### !!! EMAIL TO FED TO REQUIRE UPDATE !!!

# reads the single interesting sheets and
# imports them in df format

classi <- c('text', rep('numeric', 14), 'text')

cpi_greenbook <- read_excel(file.path(temp_dir,'Greenbook_allvar_row.xlsx'), 
                            sheet='gPCPI', col_types=classi, na='#N/D')
core_greenbook <- read_excel(file.path(temp_dir,'Greenbook_allvar_row.xlsx'),
                             sheet='gPCPIX', col_types=classi, na='#N/D')
deflator_greenbook <- read_excel(file.path(temp_dir,'Greenbook_allvar_row.xlsx'),
                                 sheet='gPGDP', col_types=classi, na='#N/D')

# # replace NAs
# cpi_greenbook[cpi_greenbook=='NaN'] <- NA
# core_greenbook[core_greenbook=='NaN'] <- NA
# deflator_greenbook[deflator_greenbook=='NaN'] <- NA

# drop useless columns
cpi_greenbook <- cpi_greenbook[,-c(2:5, 16, 15)]
core_greenbook <- core_greenbook[,-c(2:5, 16, 15)]
deflator_greenbook <- deflator_greenbook[,-c(2:5, 16, 15)]

# name columns
names(cpi_greenbook) <- c('date', 'cpit', paste(rep('cpit', 7), 1:8, sep=''))
names(core_greenbook) <- c('date', 'coret', paste(rep('coret', 7), 1:8, sep=''))
names(deflator_greenbook) <- c('date', 'deflt', paste(rep('deflt', 7), 1:8, sep=''))


# drop useless observations via subfilter fncts
cpi <- subfilter(cpi_greenbook)
cpi.mean <- subfilter.mean(cpi_greenbook)

core <- subfilter(core_greenbook)
core.mean <- subfilter.mean(core_greenbook)

defl <- subfilter(deflator_greenbook)
defl.mean <- subfilter.mean(deflator_greenbook)


# time series conversion

cpi <- as.xts(ts(cpi, start=c(1967, 1), frequency = 4))
cpi$date <- NULL

core <- as.xts(ts(core, start=c(1967, 1), frequency = 4))
core$date <- NULL

defl <- as.xts(ts(defl, start=c(1967, 1), frequency = 4))
defl$date <- NULL

rates <- merge(ffrate, cpi, core, defl)

# same, but for mean series

cpi.mean <- as.xts(ts(cpi.mean, start=c(1967, 1), frequency = 4))
cpi.mean$date <- NULL

core.mean <- as.xts(ts(core.mean, start=c(1967, 1), frequency = 4))
core.mean$date <- NULL

defl.mean <- as.xts(ts(defl.mean, start=c(1967, 1), frequency = 4))
defl.mean$date <- NULL

rates.mean <- merge(ffrate, cpi.mean, core.mean, defl.mean)

# Section to get historical, revised inflation TS

rev_hist <- merge(

          # Consumer Price Index for All Urban Consumers: All Items 
          rev_pci = fredr_series_observations(series_id='CPIAUCSL', 
                                          frequency='q', 
                                        aggregation_method='eop',
                                        units='cca') %>% tbl_xts(), 
          
          # Consumer Price Index for All Urban Consumers: All Items Less Food and Energy
          rev_pci_fe  = fredr_series_observations(series_id='CPILFESL', 
                                            frequency='q', 
                                            aggregation_method='eop',
                                            units='cca') %>% tbl_xts(),
          
          # Gross Domestic Product: Implicit Price Deflator
          rev_defl = fredr_series_observations(series_id='GDPDEF', 
                                          frequency='q', 
                                         aggregation_method='eop',
                                         units='cca') %>% tbl_xts(),
          
          # Personal Consumption Expenditures including Food and Energy
          rev_pce  = fredr_series_observations(series_id='PCEPI', 
                                             frequency='q', 
                                         aggregation_method='eop',
                                         units='cca') %>% tbl_xts(),
          
          # Personal Consumption Expenditures Excluding Food and Energy
          rev_pce_fe  = fredr_series_observations(series_id='PCEPILFE', 
                                            frequency='q', 
                                            aggregation_method='eop',
                                            units='cca') %>% tbl_xts()
) 
# renames variables
names(rev_hist) <-  c('rev_cpi', 'rev_cpi_fe', 'rev_defl',
                      'rev_pce', 'rev_pce_fe')


## UNEMPLOYMENT METRICS ####

claims <- fredr_series_observations(series_id='ICSA', 
                                    frequency='q', 
                                    aggregation_method='sum') %>% 
                                      tbl_xts()
# initial claims, number

natural_unemp_short <- fredr_series_observations(series_id='NROUST', 
                                                 frequency='q') %>% 
                                      tbl_xts()
# natural employment on the short run

natural_unemp_long <- fredr_series_observations(series_id='NROU', 
                                                frequency='q') %>% 
                                      tbl_xts()
# longer term natural unemployment rate

current_unemp <- fredr_series_observations(series_id='UNRATE', 
                                           frequency='q') %>% 
                                      tbl_xts()
# current unemployment rate

tot_emp <- fredr_series_observations(series_id='PAYEMS', 
                                     frequency='q') %>% 
                                      tbl_xts() %>% 
                                        `*`(.,1000)
# total employed, thousands

## Unemployment manipulation

short_long_diff <- natural_unemp_short - natural_unemp_long
layoffs <- 100*claims/tot_emp
employment_fluct <- current_unemp - natural_unemp_long

## merging

unemployment <- merge(layoffs, employment_fluct, current_unemp, short_long_diff)
names(unemployment) <- c('layoffs', 'employment_fluct', 'unempl_rate', 'unemp_sh_lng')


#### OUTPUT GAPS ####
# expost gap

capacity <- fredr_series_observations(series_id='GDPPOT', frequency='q') %>% tbl_xts()
# real installed capacity, 2009 chained dollars

actual <- fredr_series_observations(series_id='GDPC1', frequency='q') %>% tbl_xts()
# actual gdp

gap_expost <- (actual-capacity)*100/capacity 

# real time gap

download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/real-time-data/data-files/files/xlsx/routputqvqd.xlsx?la=en',
              file.path(temp_dir,'PhilFed_realtime_realgdp.xlsx'), mode='wb',
              extra='--no-check-certificate',
              quiet = T)

gdp_waves <- read_excel(file.path(temp_dir,'PhilFed_realtime_realgdp.xlsx'), 
                        sheet='ROUTPUT', na='#N/A')
cols <- ncol(gdp_waves)

options(warn=-1) # line below produces more than 50 warnings as it produces NAs, which I want
y_real_gap <- as.xts(ts(trendev(gdp_waves), start=c(1965, 4), frequency = 4))

gap_output <- merge(y_real_gap, gap_expost)
names(gap_output) <- c('realtime_gap', 'expost_gap')
                      # philly and st louis gaps, respectively
options(warn=0) # reactivates warnings

cfnai <- fredr_series_observations(series_id = 'CFNAI', frequency = 'q') %>% tbl_xts()

##### Consumption #####
# work in progress
real_cons_exp <-  fredr_series_observations(series_id = 'PCECC96', frequency = 'q') %>% tbl_xts()
real_cons_exp_g <- diff(log(real_cons_exp))*100
real_cons_exp_filtered <- hamil_filter(real_cons_exp)

cons_durables <- fredr_series_observations(series_id = 'PCEDG', frequency = 'q') %>% tbl_xts()
cons_durables_g <- diff(log(cons_durables))*100
cons_durables_filtered <- hamil_filter(cons_durables)

cons_nondurables <- fredr_series_observations(series_id = 'PCEND', frequency = 'q') %>% tbl_xts()
cons_nondurables_g <- diff(log(cons_nondurables))*100
cons_nondurables_filtered <- hamil_filter(cons_nondurables)

cons_serv <- fredr_series_observations(series_id = 'PCESV', frequency = 'q') %>% tbl_xts()
cons_serv_g <- diff(log(cons_serv))*100
cons_serv_filtered <- hamil_filter(cons_serv)

cons_FE <- fredr_series_observations(series_id = 'DPCCRC1M027SBEA', frequency = 'q') %>% tbl_xts()
cons_FE_g <- diff(log(cons_FE))*100
cons_FE_filtered <- hamil_filter(cons_FE)

cons_gvt <- fredr_series_observations(series_id = 'GCEC1', frequency = 'q') %>% tbl_xts()
cons_gvt_g <- diff(log(cons_gvt))*100
cons_gvt_filtered <- hamil_filter(cons_gvt)

cons_defense <- fredr_series_observations(series_id = 'FDEFX', frequency = 'q') %>% tbl_xts()
cons_defense_g <- diff(log(cons_defense))*100
cons_defense_filtered <- hamil_filter(cons_defense)


consumption <- merge(real_cons_exp,
                     real_cons_exp_g,
                     real_cons_exp_filtered,
                     cons_durables,
                     cons_durables_g,
                     cons_durables_filtered,
                     cons_nondurables,
                     cons_nondurables_g,
                     cons_nondurables_filtered,
                     cons_serv,
                     cons_serv_g,
                     cons_serv_filtered,
                     cons_FE,
                     cons_FE_g,
                     cons_FE_filtered,
                     cons_gvt,
                     cons_gvt_g,
                     cons_gvt_filtered,
                     cons_defense,
                     cons_defense_g,
                     cons_defense_filtered)

names(consumption) <- c('real_c',
                        'real_c_g',
                        'real_c_filtered',
                        'c_durab',
                        'c_durab_g',
                        'c_durab_filtered',
                        'c_nondurab',
                        'c_nondurab_g',
                        'c_nondurab_filtered',
                        'c_serv',
                        'c_serv_g',
                        'c_serv_filtered',
                        'c_noFE',
                        'c_noFE_g',
                        'c_noFE_filtered',
                        'c_gvt',
                        'c_gvt_g',
                        'c_gvt_filtered',
                        'c_defense',
                        'c_defense_g',
                        'c_defense_filtered'
)
# mnemonics <- c("GPDI",
#                "GPDIC1",
#                "PNFI",
#                "PRFI",
#                "DRSFRMACBS",
#                "EMRATIO",
#                "PAYEMS",
#                "CES0500000003",
#                "CE16OV",
#                "LREM25TTUSQ156S",
#                "CES9091000001",
#                "LNS12300060",
#                "U6RATE",
#                "UEMPMEAN",
#                "AWHNONAG",
#                "AWHMAN",
#                "CES0600000007",
#                "CES4300000007",
#                "CEU4200000007",
#                "CES4200000007",
#                "CEU3100000007"
#                )
# 
# descr <- c('priv_inv',
#            'real_priv_inv',
#            'nonresidential_fixed_inv',
#            'residential_fixed_inv',
#            'deliquency_rate',
#            'active_ratio',
#            'payroll',
#            'priv_hourly_wage',
#            'civil_empl',
#            'emp_rate_prime',
#            'tot_unempl',
#            'mean_unempl_duration',
#            'week_hours_tot',
#            'week_hours_manufacturing',
#            'week_hours_goods',
#            'week_hours_logistics',
#            'week_hours_trade',
#            'week_hours_trade_seasonadj',
#            'week_hours_durable'
#            )
# 
# fredrlist <- data.frame(mnem = mnemonics,
#                         descr = descr)

###### List of additional series ########
# mnemonics	desc
# PCECC96	Real personal consumption expenditures
# PCEDG	personal consumption expenditures, durable goods
# PCND	personal consumption expenditures, non durable goods
# PCESV	personal consumption expenditures, services
# DPCCRC1M027SBEA	personal consumption expenditures ex food and energy
# GCEC1	Real Government Consumption Expenditures and Gross Investment
# FDEFX	Federal Government: National Defense Consumption Expenditures and Gross Investment
# 
# GPDI	Gross Private Domestic Investment
# GPDIC1	Real Gross Private Domestic Investment
# PNFI	Private Nonresidential Fixed Investment
# PRFI	Private Residential Fixed Investment
# DRSFRMACBS	Delinquency Rate on Single-Family Residential Mortgages, Booked in Domestic Offices, All Commercial Banks
# EMRATIO	Civilian Employment-Population Ratio
# PAYEMS	All Employees: Total Nonfarm Payrolls
# CES0500000003	Average Hourly Earnings of All Employees: Total Private
# CE16OV	Civilian Employment Level
# LREM25TTUSQ156S	Employment Rate: Aged 25-54: All Persons for the United States
# CES9091000001	All Employees: Government: Federal
# LNS12300060	Employment Population Ratio: 25 - 54 years
# U6RATE	Total unemployed, plus all marginally attached workers plus total employed part time for economic reasons
# UEMPMEAN	Average (Mean) Duration of Unemployment
# AWHNONAG	Average Weekly Hours of Production and Nonsupervisory Employees: Total private
# AWHMAN	Average Weekly Hours of Production and Nonsupervisory Employees: Manufacturing
# CES0600000007	Average Weekly Hours of Production and Nonsupervisory Employees: Goods-Producing 
# CES4300000007	Average Weekly Hours of Production and Nonsupervisory Employees: Transportation and Warehousing
# CEU4200000007	Average Weekly Hours of Production and Nonsupervisory Employees: Retail Trade
# CES4200000007	Average Weekly Hours of Production and Nonsupervisory Employees: Retail Trade  seasonally adj
# CEU3100000007	Average Weekly Hours of Production and Nonsupervisory Employees: Durable Goods

# OIL prices
# WTISPLC	Spot Crude Oil Price: West Texas Intermediate
# DCOILBRENTEU	Crude Oil Prices: Brent - Europe





##### SPREADS ####

## BAA 10Y bonds        !!! - DISCONTINUED BY FRED - !!!
spread_baa <- fredr_series_observations(series_id='BAA10Y', 
                                        frequency='q', 
                                        aggregation_method = 'eop') %>% tbl_xts()

## BAA 20+ year bonds rate
baa <- fredr_series_observations(series_id = 'BAA', frequency
                                 = 'q', 
                                 aggregation_method = 'eop') %>% tbl_xts()

## AAA 20+ year bonds rate
aaa <- fredr_series_observations(series_id = 'AAA', frequency
                                 = 'q', 
                                 aggregation_method = 'eop') %>% tbl_xts()

## 3 months Tbill rate
tbill_rate_3m <- fredr_series_observations(series_id='TB3MS',
                                           frequency='q', 
                                           aggregation_method = 'eop') %>% tbl_xts()

## 1 year
tbill_rate_1y <- fredr_series_observations(series_id='DGS1', 
                                           frequency='q', 
                                           aggregation_method = 'eop') %>% tbl_xts()

## 10 years 
tbill_rate_10y <- fredr_series_observations(series_id='DGS10',
                                            frequency='q', 
                                            aggregation_method = 'eop') %>% tbl_xts()




## spread btw 3m tbill and FFR
tbill3_ffr <- fredr_series_observations(series_id='TB3SMFFM', 
                                        aggregation_method = 'eop',
                                        frequency = 'q') %>% tbl_xts()

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)# disables disclaimer about version update
# downloads daily prices time series through new Yahoo! API
# to be fixed sooner than later
sp_ret <- getSymbols(src='yahoo', Symbols='^GSPC',
                     from='1950-01-03',
                     to=format(Sys.Date()-1, '%Y-%m-%d'),
                     auto.assign = F)

# aggregating up to quarterly data
# through monthly upscaling
sp_ret <- to.monthly(sp_ret)

# adapts the order of the observations                        <- code for old API
# sp_ret <- sp_ret[order(-1:-nrow(sp_ret)),]
# sp_ret <- data.frame(sp_ret$sp_ret.Close)
# sp_ret <- as.xts(ts(sp_ret[1:(nrow(sp_ret)-1),], start=c(1950, 01), frequency=12))

sp_ret <- diff(log(sp_ret$sp_ret.Close))*100
sp_ret <- as.xts(aggregate(sp_ret, as.yearqtr(as.yearmon(time(sp_ret))), mean))

# one_year <- fredr_series_observations(series_id='DGS1', frequency='q') %>% tbl_xts()
# spread_sp <- (sp_ret - one_year)
spread_sp_3m <- sp_ret - tbill_rate_3m

# replicate spread from moody's
spread_baa_long <- baa - tbill_rate_10y

# quality spread
spread_baa_aaa <- baa - aaa

# corp vs treas spread
spread_aaa <- aaa - tbill_rate_10y

spreads <- merge(spread_baa, spread_sp_3m, 
                 tbill3_ffr, tbill_rate_3m,
                 tbill_rate_1y, tbill_rate_10y,
                 spread_baa_long, spread_baa_aaa,
                 spread_aaa, aaa, baa)
names(spreads) <- c('spread_baa', 'spread_sp_3m',
                    'tbill3_ffr', 'tbill_rate_3m',
                    'tbill_rate_1y', 'tbill_rate_10y',
                    'spread_baa_long', 'spread_baa_aaa',
                    'spread_aaa', 'aaa', 'baa')

options("getSymbols.warning4.0"=T) # activates disclaimer v0.4

#### Additional variables #####

#### DEFICIT as % OF GDP

surplus_season <- fredr_series_observations(series_id='M318501Q027NBEA', frequency='q') %>% tbl_xts()

inizio <- as.Date(min(time(surplus_season)), format='%Y-%m-%d')
fine <- as.Date(max(time(surplus_season)), format='%Y-%m-%d')

gdp <- fredr_series_observations(series_id='GDP', frequency='q',
                           observation_start= inizio,
                           observation_end= fine) %>% tbl_xts()

# to deseasonalize one needs to get back to ts format

# surplus_ratio <- 100*surplus/gdp
surplus.ts <- ts((100*surplus_season/gdp), frequency=4,
                       start=c(year(inizio), quarter(inizio)),
                       end=  c(year(fine), quarter(fine)))

surplus_gdp <- decompose(surplus.ts)$x - decompose(surplus.ts)$seasonal
surplus_gdp <- as.xts(surplus_gdp)
names(surplus_gdp) <- 'surplus_gdp'
 
#### DEBT 
# debt to gdp series
debt_gdp <- fredr_series_observations(series_id='GFDEGDQ188S', frequency='q') %>% tbl_xts()

# debt level, millions of $
debt_lev <- fredr_series_observations(series_id='GFDEBTN', frequency='q') %>% tbl_xts()

# debt growth rate to proxy deficit
debt_g <- diff(log(debt_lev))*100


# debt held by FED, billions of $
debt_fed <- fredr_series_observations(series_id = 'FDHBFRBN', frequency='q') %>% tbl_xts()



# percentage of debt held by FED
debt_fed_share <- 100*(debt_fed*1000/debt_lev)


fiscal <- merge(surplus_gdp, debt_g, debt_gdp, debt_fed, debt_fed_share)
names(fiscal) <- c('surplus_gdp', 'debt_growth', 'debt_gdp', 'debt_fed', 'debt_fed_share')

#### MONEY AGGREGATES 

base <- fredr_series_observations(series_id='BOGMBASE', frequency='q') %>% tbl_xts() %>% `/`(.,1000)
m1 <- fredr_series_observations(series_id='M1SL', frequency='q') %>% tbl_xts()
m2 <- fredr_series_observations(series_id='M2SL', frequency='q') %>% tbl_xts()
m3 <- fredr_series_observations(series_id = 'MABMM301USM189S', frequency = 'q') %>% tbl_xts()

money <- merge(base, m1, m2, m3)
names(money) <- c('base', 'm1', 'm2', 'm3')

# monetary aggregates growth rates
money_g <- diff(log(money))*100
names(money_g) <- c('base_g', 'm1_g', 'm2_g', 'm3_g')

money <-  merge(money, money_g)


#### SPF DATA ####

# automate download of the xlsx file, import, run statistics and merge

# download CPI inflation rate raw file for individuals in the SPF
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_cpi.xlsx?la=en',
              file.path(temp_dir,'spf_ind_cpi_rate.xlsx'), mode='wb',
              extra='--no-check-certificate',
              quiet = T)

# download CORE CPI inflation rate raw file for individuals in the SPF
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_corecpi.xlsx?la=en',
              file.path(temp_dir,'spf_ind_corecpi_rate.xlsx'), mode='wb',
              extra='--no-check-certificate',
              quiet = T)

# download PCE inflation rate raw file for individuals in the SPF
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_pce.xlsx?la=en',
              file.path(temp_dir,'spf_ind_pce_rate.xlsx'), mode='wb',
              extra='--no-check-certificate',
              quiet = T)

# download CORE PCE inflation rate file for individuals in the SPF
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_corepce.xlsx?la=en',
              file.path(temp_dir,'spf_ind_corepce_rate.xlsx'), mode='wb',
              extra='--no-check-certificate',
              quiet = T)



spf_cpi <- spf_funct('spf_ind_cpi_rate.xlsx', 'CPI',
                     ahead=ahead)
spf_corecpi <- spf_funct('spf_ind_corecpi_rate.xlsx', 'CORECPI',
                         ahead=ahead)
spf_pce <- spf_funct('spf_ind_pce_rate.xlsx','PCE',
                     ahead=ahead)
spf_corepce <- spf_funct('spf_ind_corepce_rate.xlsx', 'COREPCE',
                         ahead=ahead)

spf <- merge(spf_cpi,spf_corecpi,spf_pce, spf_corepce)



##### Wu-Xia Shadow FFR #####

# This shadow rate ends in 2015 when the ZLB period was terminated.
# using this shadow rate allows the inclusion of 
# non-Taylor rule interventions of the CB in the 
# quantity of money in the policy rate path

download.file(#url = 'http://faculty.chicagobooth.edu/jing.wu/research/data/policyrate.xls', old link
              url = 'https://drive.google.com/uc?export=download&id=1Z0UdvdPhXaIQNVXTZzYtvxDXwxA6YD2e',
              destfile = file.path(temp_dir, 'wuxia_dwnl.xls'),
              mode = 'wb',
              quiet = T)

shffr <- read_excel(path = file.path(temp_dir,'wuxia_dwnl.xls'),
                    col_names = c('date', 'shffr'),
                    sheet = 'Sheet1')

# the above lines download the xls file and dataframe it

#### conversion to xts ####

shffr <- shffr$shffr
shffr <- as.xts(ts(shffr, frequency = 12, start=c(1960, 1)))
shffr <- aggregate(shffr, as.yearqtr(as.yearmon(time(shffr))), xts::last) %>% as.xts()


# stitching
shffr_ext <- ffr
shffr_ext["2007/2015"] <- shffr["2007/2015"]

shffr <- merge(shffr_ext, lag(shffr_ext, 1))
names(shffr) <- c('shffr', 'shffrb')



##### Krippner's shadow rate ###################################################

# download xlsx file
download.file(url = 'https://www.rbnz.govt.nz/-/media/ReserveBank/Files/Publications/Research/additional-research/leo-krippner/us-ea-jp-uk-ssrs-monthly-update.xlsx?la=en&revision=a1d77cad-e95c-47cb-b15d-73b5a6d719fa',
              destfile = file.path(temp_dir, "krippner_dwnl.xlsx"),
              mode = "wb",
              quiet = T)

# read in US data only
kripp_ffr <- read_excel(path = file.path(temp_dir, 'krippner_dwnl.xlsx'),
                        sheet = 2,
                        range = "A7:B303", 
                        col_names = c('date', 'kripp_shffr'),
                        col_types = c('date', 'numeric')) %>% 
  xts(x = .$kripp_shffr, order.by = as.Date(.$date))

kripp_ffr <- aggregate(kripp_ffr, as.yearqtr(as.yearmon(time(kripp_ffr))), xts::last) %>% xts()



# stitching
kripp_ext <- ffr
kripp_ext["1995/2019-08"] <- kripp_ffr

kripp_ffr <- merge(kripp_ext, lag(kripp_ext, 1))
names(kripp_ffr) <- c("kripp_shffr", "kripp_shffrb")


##### Un. of Michigan Surveys of Consumers #######

# Glossary:
# perch is PERcentage CHange; 
# SOC(M) is Survey Of Consumers (Michigan);
# g is for Growth rate
# ind is for INDex
# diff is for DIFFerence, levels when not otherwise stated
# create list with requests to send
# to the UoM website

req_fields <- list(#'User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.120 Mobile Safari/537.36',
  table = '32',
  year = '1960',
  qorm = "Q",
  order = 'asc',
  format = "View Below")

# consumers' expected inflation, 1 year ahead
socm_1y_inflation <- POST(url = 'https://data.sca.isr.umich.edu/data-archive/mine.php',
                          body = req_fields,
                          encode = 'form' ) %>% 
                          content('parsed', encoding = 'UTF-8') %>%
                          rvest::html_table() %>% 
                          .[[2]] %>% 
                          dplyr::select(X13, X15) %>% 
                          .[-1,] %>%   
                          rename(socm_1y_inflation_mean = X13,
                                 socm_1y_inflation_sd = X15) %>% 
                          ts(start = c(1960, 1), frequency = 4) %>%
                          xts(order.by = time(.) %>% as.Date())



# consumers' expected inflation, 5 years ahead
req_fields[['table']] <- '33'

socm_5y_inflation <- POST(url = 'https://data.sca.isr.umich.edu/data-archive/mine.php',
                          body = req_fields,
                          encode = 'form' 
                          ) %>% 
                            content('parsed', encoding = 'UTF-8') %>%
                            rvest::html_table() %>% 
                            .[[2]] %>% 
                            dplyr::select(X13, X15) %>% 
                            .[-1,] %>% 
                            rename(socm_5y_inflation_mean = X13,
                                   socm_5y_inflation_sd = X15) %>% 
                            ts(start = c(1960, 1), frequency = 4) %>%
                            xts(order.by = time(.) %>% as.Date())
                            


# consumer confidence indexes + transformations
req_fields[['table']] <- '5'

socm_indexes <- POST(url = 'https://data.sca.isr.umich.edu/data-archive/mine.php',
                     body = req_fields,
                     encode = 'form' 
                      ) %>% 
                        content('parsed', encoding = 'UTF-8') %>%
                        rvest::html_table() %>% 
                        .[[2]] %>% 
                        dplyr::select(X8, X9) %>% 
                        .[-1,] %>%
                        rename(socm_actual_ind = X8,
                               socm_expected_ind = X9) %>% 
                        
                        apply(2, as.numeric) %>% as.data.frame() %>% 
                        
                        # create deviations from expected index
                        # percentage deviations from previous level
                        # of the indexes, actual and expected
                        mutate(soc_diff_ind = socm_actual_ind - socm_expected_ind,
                               socm_perch_actual_ind = 100*diff(log(socm_actual_ind) %>% rbind(NA)),
                               socm_perch_expected_ind = 100*diff(log(socm_expected_ind)%>% rbind(NA))) %>% 
                        ts(start = c(1960, 1), frequency = 4) %>%
                        xts(order.by = time(.) %>% as.Date())


# merge all datafiles together
SOC_Michigan <- merge(socm_1y_inflation,
                      socm_5y_inflation,
                      socm_indexes)


##### EPU indexes #####
# nb: the US EPU contains data from SPF
# that we already have: using EPU + SPF
# together is not advised, use rather 
# categorical series with the exclusion of
# economic forecasters disagreement

# download excel files
download.file(url = 'http://www.policyuncertainty.com/media/US_Policy_Uncertainty_Data.xlsx',
              destfile = file.path(temp_dir, 'EPU_aggregate.xlsx'), 
              mode = 'wb',
              quiet = T)

download.file(url = 'http://www.policyuncertainty.com/media/Categorical_EPU_Data.xlsx',
              destfile = file.path(temp_dir, 'EPU_categories.xlsx'), 
              mode = 'wb',
              quiet = T)

# import data, drop last row
epu_aggregate <- read_excel(path = file.path(temp_dir, 'EPU_aggregate.xlsx'),
                            col_names = T, 
                            sheet = 1,
                            col_types = 'numeric'
                            )
epu_aggregate <- epu_aggregate[-nrow(epu_aggregate),]

epu_aggregate_comp <- read_excel(path = file.path(temp_dir, 'EPU_aggregate.xlsx'),
                                 col_names = T, 
                                 sheet = 2, 
                                 col_types = 'numeric'
                                 )
epu_aggregate_comp <- epu_aggregate_comp[-nrow(epu_aggregate_comp),]

epu_cat <- read_excel(path = file.path(temp_dir, 'EPU_categories.xlsx'),
                      col_names = T,
                      sheet = 1,
                      col_types = c('date', rep('numeric', 12))
                      )
epu_cat <- epu_cat[-c((nrow(epu_cat)-1),nrow(epu_cat)),]

# convert data to xts
epu_aggregate_ts <- ts(data = epu_aggregate, 
                       start=c(epu_aggregate$Year[1], 
                               epu_aggregate$Month[1]),
                       frequency = 12) %>% 
                    aggregate(FUN = mean, nfrequency = 4) %>%
                    xts(x = ., order.by = as.Date(time(.)))



epu_aggregate_comp_ts <- ts(data = epu_aggregate_comp, 
                            start=c(epu_aggregate_comp$Year[1], 
                                    epu_aggregate_comp$Month[1]),
                            frequency = 12) %>% 
                          aggregate(FUN = mean, nfrequency = 4) %>%
                          xts(x = ., order.by = as.Date(time(.)))

epu_cat_ts <- ts(data = epu_cat,
                 start = c(1985, 01),
                 frequency = 12) %>% 
              aggregate(FUN = mean, nfrequency = 4) %>% 
              xts(x = ., order.by = as.Date(time(.)))
# epu_cat_ts <- xts(x = epu_cat, frequency = 12, order.by = epu_cat$Date)

epu_aggregate_ts$Month <- epu_aggregate_ts$Year <- NULL
epu_aggregate_comp_ts$Month <- epu_aggregate_comp_ts$Year <- epu_aggregate_comp_ts$News_Based_Policy_Uncert_Index <- NULL
epu_cat_ts$Date <- NULL

names(epu_aggregate_ts) <- c('epu_baseline_index', 'epu_news_index')
names(epu_aggregate_comp_ts) <- c('epu_public_purchases', 'epu_cpi_spf_disagr', 'epu_tax_expir')
names(epu_cat_ts) <- c('epu_econpol', 'epu_monpol', 'epu_fiscal', 'epu_taxes', 
                       'epu_gvt_spending', 'epu_healthcare', 'epu_natsec',
                       'epu_entitle', 'epu_regul', 'epu_finregul', 'epu_tradepol',
                       'epu_debt_curr')

epu <- merge(epu_aggregate_ts,
             epu_aggregate_comp_ts,
             epu_cat_ts)

#### Merge to dataset ####

db_US <- merge(rates, 
               rev_hist,
               unemployment,
               gap_output,
               cfnai,
               spreads,
               consumption,
               money,
               fiscal,
               spf,
               shffr,
               kripp_ffr,
               SOC_Michigan,
               epu)

#### Quarterly dummies to account for seasonality

dums <- xts(x = data.frame(q1 = rep(c(1,0,0,0), floor(length(index(db_US)))/4),
                           q2 = rep(c(0,1,0,0), floor(length(index(db_US)))/4),
                           q3 = rep(c(0,0,1,0), floor(length(index(db_US)))/4)),
            order.by = index(db_US)
)

db_US <- merge(db_US, dums)

# date harmonisation

db_US <- db_US %>% 
  tbl2xts::xts_tbl() %>% 
  dplyr::mutate(date = as.Date(index(db_US))) %>% 
  tbl2xts::tbl_xts()

# write to disk
write.zoo(x=db_US, 
          file=file.path(data_dir, 'US_data.txt'), 
          sep=';', 
          row.names=F, 
          index.name='date')



## housekeeping
rm(ffr, classi, core_greenbook, cpi_greenbook, deflator_greenbook,
cpi, core, defl, cpi.mean, core.mean, defl.mean,
claims, natural_unemp_long, natural_unemp_short,
current_unemp, tot_emp, layoffs, employment_fluct,
cols, gdp_waves, rates, ffrate, unemployment, gap_output,
spreads, sp_ret, spread_baa, spread_sp_3m,
tbill_rate_3m, tbill_rate_10y, tbill_rate_1y,ffrb,
spread_baa_long, spread_baa_aaa, spread_aaa, aaa, baa,
actual, capacity, y_real_gap, gap_expost, rates.mean,
base, m1, m2, m3, money, money_g, gdp,
inizio, fine, surplus.ts, debt_fed,
debt_fed_share, debt_g, debt_gdp, debt_lev, fiscal,
surplus_gdp, surplus_season, spf, spf_corecpi,
spf_corepce, spf_cpi, spf_pce, rev_hist,
tbill3_ffr, shffr, cfnai,
socm_indexes, req_fields,
socm_1y_inflation, socm_5y_inflation, SOC_Michigan,
short_long_diff, epu_aggregate, epu_aggregate_comp,
epu_aggregate_comp_ts, epu_aggregate_ts,
epu_cat, epu_cat_ts, epu, real_cons_exp,
real_cons_exp_g, real_cons_exp_filtered,
cons_durables,cons_durables_g,cons_durables_filtered,
cons_nondurables,cons_nondurables_g,
cons_nondurables_filtered,cons_serv,
cons_serv_g,cons_serv_filtered,
cons_FE,cons_FE_g,cons_FE_filtered,
cons_gvt,cons_gvt_g,cons_gvt_filtered,
cons_defense,cons_defense_g,cons_defense_filtered,
kripp_ffr, kripp_ext, shffr_ext,
dums,
consumption)
