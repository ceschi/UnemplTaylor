#### Second venue of research
#### using unemployment metrics to proxy output gap ####

# side goal: integrate smoothly this with previous codes
# for US (Trulyfinal.R) and for the panel building

# This code collects and scraps core data for the analysis
# and puts it in xts format

if (flag___singular == 1){
  
  cat('Single file execution')
  source('directories.R')
  source('functs.R')
  
  # selector for the lead in SPF
  ahead <- 1
}


#### Scraping US data ####


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
              file.path(temp_dir,'Greenbook_allvar_row.xls'), mode='wb',
              extra='--no-check-certificate')

# reads the single interesting sheets and
# imports them in df format

classi <- c('text', rep('numeric', 14), 'text')

cpi_greenbook <- read_excel(file.path(temp_dir,'Greenbook_allvar_row.xls'), 
                            sheet='gPCPI', col_types=classi, na='#N/D')
core_greenbook <- read_excel(file.path(temp_dir,'Greenbook_allvar_row.xls'),
                             sheet='gPCPIX', col_types=classi, na='#N/D')
deflator_greenbook <- read_excel(file.path(temp_dir,'Greenbook_allvar_row.xls'),
                                 sheet='gPGDP', col_types=classi, na='#N/D')

# replace NAs
cpi_greenbook[cpi_greenbook=='NaN'] <- NA
core_greenbook[core_greenbook=='NaN'] <- NA
deflator_greenbook[deflator_greenbook=='NaN'] <- NA

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
                                        units='pc1') %>% tbl_xts(), 
          
          # Consumer Price Index for All Urban Consumers: All Items Less Food and Energy
          rev_pci_fe  = fredr_series_observations(series_id='CPILFESL', 
                                            frequency='q', 
                                            aggregation_method='eop', 
                                            units='pc1') %>% tbl_xts(),
          
          # Gross Domestic Product: Implicit Price Deflator
          rev_defl = fredr_series_observations(series_id='GDPDEF', 
                                          frequency='q', 
                                         aggregation_method='eop', 
                                         units='pc1') %>% tbl_xts(),
          
          # Personal Consumption Expenditures including Food and Energy
          rev_pce  = fredr_series_observations(series_id='PCE', 
                                             frequency='q', 
                                         aggregation_method='eop', 
                                         units='pc1') %>% tbl_xts(),
          
          # Personal Consumption Expenditures Excluding Food and Energy
          rev_pce_fe  = fredr_series_observations(series_id='PCEPILFE', 
                                            frequency='q', 
                                            aggregation_method='eop', 
                                            units='pc1') %>% tbl_xts()
) 
# renames variables
names(rev_hist) <-  c('rev_cpi', 'rev_cpi_fe', 'rev_defl',
                      'rev_pce', 'rev_pce_fe')


## UNEMPLOYMENT METRICS ####

claims <- fredr_series_observations(series_id='ICSA', frequency='q', aggregation_method='sum') %>% tbl_xts()
# initial claims, number

natural_unemp_short <- fredr_series_observations(series_id='NROUST', frequency='q') %>% tbl_xts()
# natural employment on the short run

natural_unemp_long <- fredr_series_observations(series_id='NROU', frequency='q') %>% tbl_xts()
# longer term natural unemployment rate

current_unemp <- fredr_series_observations(series_id='UNRATE', frequency='q') %>% tbl_xts()
# current unemployment rate

tot_emp <- fredr_series_observations(series_id='PAYEMS', frequency='q') %>% tbl_xts() %>% `*`(.,1000)
# total employed, thousands

## Unemployment manipulation

# short_long_diff <- natural_unemp_short - natural_unemp_long
layoffs <- 100*claims/tot_emp
employment_fluct <- current_unemp - natural_unemp_long

## merging

unemployment <- merge(layoffs, employment_fluct)
names(unemployment) <- c('layoffs', 'employment_fluct')


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
              extra='--no-check-certificate')

gdp_waves <- read_excel(file.path(temp_dir,'PhilFed_realtime_realgdp.xlsx'), 
                        sheet='ROUTPUT', na='#N/A')
cols <- ncol(gdp_waves)

options(warn=-1) # line below produces more than 50 warnings as it produces NAs, which I want




y_real_gap <- as.xts(ts(trendev(gdp_waves), start=c(1965, 4), frequency = 4))

gap_output <- merge(y_real_gap, gap_expost)
names(gap_output) <- c('realtime_gap', 'expost_gap')
                      # philly and st louis gaps, respectively
options(warn=0) # reactivates warnings


##### SPREADS ####

## BAA 10Y bonds        !!! - DISCONTINUED BY FRED - !!!
spread_baa <- fredr_series_observations(series_id='BAA10Y', frequency='q') %>% tbl_xts()

## 3 months Tbill rate
tbill_rate_3m <- fredr_series_observations(series_id='TB3MS',frequency='q') %>% tbl_xts()

## 1 year
tbill_rate_1y <- fredr_series_observations(series_id='DGS1', frequency='q') %>% tbl_xts()

## 10 years 
tbill_rate_10y <- fredr_series_observations(series_id='DGS10',frequency='q') %>% tbl_xts()


## spread btw 3m tbill and FFR
tbill3_ffr <- fredr_series_observations(series_id='TB3SMFFM', frequency='q', aggregation_method='eop') %>% tbl_xts()



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

spreads <- merge(spread_baa, spread_sp_3m, 
                 tbill3_ffr, tbill_rate_3m,
                 tbill_rate_1y, tbill_rate_10y)
names(spreads) <- c('spread_baa', 'spread_sp_3m',
                    'tbill3_ffr', 'tbill_rate_3m',
                    'tbill_rate_1y', 'tbill_rate_10y')

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

money <- merge(base, m1, m2)
names(money) <- c('base', 'm1', 'm2')

# monetary aggregates growth rates
money_g <- diff(log(money))*100
names(money_g) <- c('base_g', 'm1_g', 'm2_g')


#### SPF DATA ####

# automatize download of the xlsx file, import, run statistics and merge

# download CPI inflation rate raw file for individuals in the SPF
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_cpi.xlsx?la=en',
              file.path(temp_dir,'spf_ind_cpi_rate.xlsx'), mode='wb',
              extra='--no-check-certificate')

# download CORE CPI inflation rate raw file for individuals in the SPF
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_corecpi.xlsx?la=en',
              file.path(temp_dir,'spf_ind_corecpi_rate.xlsx'), mode='wb',
              extra='--no-check-certificate')

# download PCE inflation rate raw file for individuals in the SPF
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_pce.xlsx?la=en',
              file.path(temp_dir,'spf_ind_pce_rate.xlsx'), mode='wb',
              extra='--no-check-certificate')

# download CORE PCE inflation rate file for individuals in the SPF
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/files/individual_corepce.xlsx?la=en',
              file.path(temp_dir,'spf_ind_corepce_rate.xlsx'), mode='wb',
              extra='--no-check-certificate')



spf_cpi <- spf_funct('spf_ind_cpi_rate.xlsx', 'CPI',
                     ahead=ahead)
spf_corecpi <- spf_funct('spf_ind_corecpi_rate.xlsx', 'CORECPI',
                         ahead=ahead)
spf_pce <- spf_funct('spf_ind_pce_rate.xlsx','PCE',
                     ahead=ahead)
spf_corepce <- spf_funct('spf_ind_corepce_rate.xlsx', 'COREPCE',
                         ahead=ahead)

spf <- merge(spf_cpi,spf_corecpi,spf_pce, spf_corepce)



#### Merge to dataset ####

db_US <- merge(rates, rev_hist, unemployment, gap_output, spreads, money, fiscal, spf)
write.zoo(x=db_US, 
          file=file.path(data_dir, 'US_data.txt'), 
          sep=';', 
          row.names=F, 
          index.name='time')





#### Other countries ####

# UK
# Canada
# Norway
# Denmark
# Japan
# Israel
# Mexico
# Finland
# S Korea
# Sweden





## housekeeping
rm(ffr, classi, core_greenbook, cpi_greenbook, deflator_greenbook,
cpi, core, defl, cpi.mean, core.mean, defl.mean,
claims, natural_unemp_long, natural_unemp_short,
current_unemp, tot_emp, layoffs, employment_fluct,
cols, gdp_waves, rates, ffrate, unemployment, gap_output,
spreads, sp_ret, spread_baa, spread_sp_3m,
tbill_rate_3m, tbill_rate_10y, tbill_rate_1y,ffrb,
actual, capacity, y_real_gap, gap_expost, rates.mean,
base, m1, m2, money, money_g, gdp,
inizio, fine, surplus.ts, debt_fed,
debt_fed_share, debt_g, debt_gdp, debt_lev, fiscal,
surplus_gdp, surplus_season, spf, spf_corecpi,
spf_corepce, spf_cpi, spf_pce, rev_hist,
tbill3_ffr)
if (flag___singular == 1) rm(ahead)
