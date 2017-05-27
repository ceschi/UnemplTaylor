#### Second venue of research
#### using unemployment metrics to proxy output gap ####

# side goal: integrate smoothly this with previous codes
# for US (Trulyfinal.R) and for the panel building

# This code collects and scraps core data for the analysis
# and puts it in xts format




#### Scraping US data ####
# #### Getting set up and creating folders ####
# working_directory <- getwd()
# temp_dir <- 'Downloaded files'
# data_dir <- 'Processed data'
# dir.create(file.path(working_directory, temp_dir))
# dir.create(file.path(working_directory, data_dir))


#### FEDERAL INTEREST RATE ####

fredr_key('5d4b5f1e6667727ee4ea90affbad1e6a')
# key for the FRED API

ffr <- as.xts(fredr_series(series_id='FEDFUNDS', frequency='m'))
ffr <- as.xts(aggregate(ffr, as.yearqtr(as.yearmon(time(ffr))), last))
# aggregates up to quarters picking quarter's last month value

ffrb <- lag(ffr)

ffrate <- merge(ffr, ffrb)

#### INFLATION FORECASTS ####

# downloads the big xlsx Greenbook file in
# a specifically created folder
download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/greenbook-data/documentation/gbweb_row_format.xls?la=en',
              file.path(working_directory,temp_dir,'Greenbook_allvar_row.xls'), mode='wb')

# reads the single interesting sheets and
# imports them in df format

classi <- c('character', rep('numeric', 14), 'character')

cpi_greenbook <- read.xlsx2(file.path(working_directory,temp_dir,'Greenbook_allvar_row.xls'), 
                            sheetName='gPCPI', colClasses=classi)
core_greenbook <- read.xlsx2(file.path(working_directory,temp_dir,'Greenbook_allvar_row.xls'),
                             sheetName='gPCPIX', colClasses=classi)
deflator_greenbook <- read.xlsx2(file.path(working_directory,temp_dir,'Greenbook_allvar_row.xls'),
                                 sheetName='gPGDP', colClasses=classi)

# replace NAs
cpi_greenbook[cpi_greenbook=='NaN'] <- NA
core_greenbook[core_greenbook=='NaN'] <- NA
deflator_greenbook[deflator_greenbook=='NaN'] <- NA

# drop useless columns
cpi_greenbook[,c(2:5, 16, 15)] <- NULL
core_greenbook[,c(2:5, 16, 15)] <- NULL
deflator_greenbook[,c(2:5, 16, 15)] <- NULL

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

#### ERROR GENERATING CODE #### 
### commented out bcs it does not merge
### issue with NA's in indexes 


# tsconverter <- function(df, index){
#   temp <- as.Date(as.yearqtr(df$index, format='%Y.%q'))
#   df$index <- temp
#   df <- xts(df, order.by = df$index)
#   # df$index <- NULL
#   return(df)
# }
# 
# cpi$date <- as.Date(as.yearqtr(cpi$date, format='%Y.%q'))
# cpi.mean$date <- as.yearqtr(cpi.mean$date, format='%Y.%q')
# 
# core$date <- as.Date(as.yearqtr(core$date, format='%Y.%q'))
# core.mean$date <- as.yearqtr(core.mean$date, format='%Y.%q')
# 
# defl$date <- as.Date(as.yearqtr(defl$date, format='%Y.%q'))
# defl.mean$date <- as.yearqtr(defl.mean$date, format='%Y.%q')
# 
# cpi <- xts(cpi, order.by=cpi$date)
# cpi.mean <- xts(cpi.mean, order.by=cpi.mean$date)
# # cpi$date <- NULL
# # cpi.mean$date <- NULL
# 
# core <- xts(core, order.by=core$date)
# core.mean <- xts(core.mean, order.by=core.mean$date)
# # core$date <- NULL
# # core.mean$date <- NULL
# 
# defl <- xts(defl, order.by=defl$date)
# defl.mean <- xts(defl.mean, order.by=defl.mean$date)
# # defl$date <- NULL
# # defl.mean$date <- NULL


## UNEMPLOYMENT METRICS ####

claims <- as.xts(fredr_series(series_id='ICSA', frequency='q', aggregation_method='sum'))
# initial claims, number

natural_unemp_short <- as.xts(fredr_series(series_id='NROUST', frequency='q'))
# natural employment on the short run

natural_unemp_long <- as.xts(fredr_series(series_id='NROU', frequency='q'))
# longer term natural unemployment rate

current_unemp <- as.xts(fredr_series(series_id='UNRATE', frequency='q'))
# current unemployment rate

tot_emp <- as.xts(fredr_series(series_id='PAYEMS', frequency='q'))*1000
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

capacity <- as.xts(fredr_series(series_id='GDPPOT', frequency='q'))
# real installed capacity, 2009 chained dollars

actual <- as.xts(fredr_series(series_id='GDPC1', frequency='q'))
# actual gdp

gap_expost <- (actual-capacity)*100/capacity

# real time gap

download.file('https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/real-time-data/data-files/files/xlsx/routputqvqd.xlsx?la=en',
              file.path(working_directory,temp_dir,'PhilFed_realtime_realgdp.xlsx'), mode='wb')

gdp_waves <- read.xlsx2(file.path(working_directory,temp_dir,'PhilFed_realtime_realgdp.xlsx'), 
                        sheetName='ROUTPUT')
cols <- ncol(gdp_waves)

gdp_waves$DATE <- as.character(gdp_waves$DATE)
gdp_waves[, 2:ncol(gdp_waves)] <- lapply(gdp_waves[, 2:ncol(gdp_waves)], function(x) as.numeric(levels(x))[x])

y_real_gap <- as.xts(ts(trendev(gdp_waves), start=c(1965, 4), frequency = 4))

gap_output <- merge(y_real_gap, gap_expost)
names(gap_output) <- c('realtime_gap', 'expost_gap')
                      # philly and st louis gaps, respectively



##### SPREADS ####

## BAA 10Y bonds        !!! - DISCONTINUED BY FRED - !!!
spread_baa <- as.xts(fredr_series(series_id='BAA10Y', frequency='q'))

## 3 months Tbill rate
tbill_rate_3m <- as.xts(fredr_series(series_id='TB3MS',frequency='q'))

## 1 year
tbill_rate_1y <- as.xts(fredr_series(series_id='DGS1', frequency='q'))

## 10 years 
tbill_rate_10y <- as.xts(fredr_series(series_id='DGS10',frequency='q'))


## Scraping Yahoo! Finance

# determine current date, adapt the Yahoo! URL
sp_ret <- read_csv(paste0('http://finance.yahoo.com/table.csv?s=^GSPC&a=0&b=3&c=1950&d=',
                          as.numeric(format(Sys.Date(), '%m')), 
                          '&e=', as.numeric(format(Sys.Date(), '%d')), 
                          '&f=', as.numeric(format(Sys.Date(), '%Y')), 
                          '&g=m&ignore=.csv'),
                   col_names=T, col_types = cols(
                     Date = col_date(format = "%Y-%m-%d"),
                     Open = col_double(),
                     High = col_double(),
                     Low = col_double(),
                     Close = col_double(),
                     Volume = col_double(),
                     `Adj Close` = col_double()
                   ))

sp_ret <- getSymbols(src='google', Symbols='^GSPC',
                     from='1950-01-03',
                     to=format(Sys.Date(), '%Y-%m-%d'))
# adapts the order of the observations
sp_ret <- sp_ret[order(-1:-nrow(sp_ret)),]
sp_ret <- data.frame(sp_ret$Close)
sp_ret <- as.xts(ts(sp_ret[1:(nrow(sp_ret)-1),], start=c(1950, 01), frequency=12))
sp_ret <- diff(log(sp_ret))*100
sp_ret <- as.xts(aggregate(sp_ret, as.yearqtr(as.yearmon(time(sp_ret))), mean))

# one_year <- as.xts(fredr_series(series_id='DGS1', frequency='q'))
# spread_sp <- (sp_ret - one_year)
spread_sp_3m <- sp_ret - tbill_rate_3m

spreads <- merge(spread_baa, spread_sp_3m)
names(spreads) <- c('spread_baa', 'spread_sp_3m')



# #### Additional variables #####
# # 
# # deficit as % of gdp
# surplus <- as.ts(fredr_series(series_id='M318501Q027NBEA', frequency='q'))
# gdp <- as.ts(fredr_series(series_id='GDP', frequency='q', 
#                            observation_start= as.Date(min(time(surplus)), format='%Y-%m-%d'),
#                            observation_end= as.Date(max(time(surplus)), format='%Y-%m-%d')))
# ratio <- 100*surplus/gdp
# 
# sa_surplus <- ratio$x - ratio$seasonal
# plot(sa_surplus)
# 
# # debt lvl
# # money aggregates
# 
# base <- as.xts(fredr_series(series_id='BOGMBASE', frequency='q'))/1000
# m1 <- as.xts(fredr_series(series_id='M1SL', frequency='q'))
# m2 <- as.xts(fredr_series(series_id='M2SL', frequency='q'))
# 
# money <- merge(base, m1, m2)
# names(money) <- c('base', 'm1', 'm2')
# 
# # monetary aggregates growth rates 
# money_g <- diff(log(money))
# names(money_g) <- c('base_g', 'm1_g', 'm2_g')
# 
# # spf data 



#### Merge to dataset ####

db_US <- merge(rates, unemployment, gap_output, spreads, money)
write.table(db_US, file.path(getwd(), data_dir, 'US_data.txt'), sep=';', row.names=F)





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
data_dir, base, m1, m2, money, money_g, gdp, surplus
)