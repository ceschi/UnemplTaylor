#### Recovering data through R from set of websites ####

library(rvest)
library(xts)
library(readr)
library(mFilter)
# library(reshape2)

#### Canada ####

# Bank of Canada data on output, NOT IN REAL TIME
# the purpose is to automate this into another script
# to grab data when estimation is run

# reads the html from provided url
BOC_page <- read_html('http://www.bankofcanada.ca/rates/indicators/capacity-and-inflation-pressures/product-market-definitions/product-market-historical-data/')

# isolates node from the html to parse the data table
ygap_table_boc <- html_nodes(BOC_page, 'table')

# extracts the data to form a data.frame with all data
df_ygap_boc <- html_table(ygap_table_boc, fill=T, header=T)
df_ygap_boc <- data.frame(df_ygap_boc[1])

# selecting relevant data and turning into xts objects
# here one must be careful to sort data correctly
# since they're presented backwards: ordering puts
# observation in the correct order
db_ygap <- df_ygap_boc[order(-1:-nrow(df_ygap_boc)),2:3]

# Creates the time series object w/ assigned beginning
db_ygap <- ts(db_ygap, start=c(1981, 1), frequency=4)
db_ygap <- as.xts(db_ygap)

# re-naming variables
colnames(db_ygap) <- c('ygap_IF', 'ygap_EMV')

# Data about the reference inflation rate for policy

BOC_cpi_page <- read_html('http://www.bankofcanada.ca/rates/indicators/capacity-and-inflation-pressures/inflation/historical-data/')
cpi_table_boc <- html_nodes(BOC_cpi_page, 'table')
df_cpi_boc <- html_table(cpi_table_boc, fill=T, header=T)
df_cpi_boc <- data.frame(df_cpi_boc[1])
db_cpi <- df_cpi_boc[order(-1:-nrow(df_cpi_boc)), 2:3]
db_cpi <- as.xts(ts(db_cpi, start=c(1993, 1), frequency=4))

colnames(db_cpi) <- c('core_cpi', 'cpi_full')

# housekeeping
rm(BOC_cpi_page, cpi_table_boc, df_cpi_boc,
   BOC_page, df_ygap_boc, ygap_table_boc)


# Data about the reference inflation rate for policy, pt 2
# these observation come from Statistics Canada, are monthly
# observation and need to be aggregated up to quarters

BOC_cpi_page2 <- read_html('http://www.bankofcanada.ca/rates/price-indexes/cpi/')
cpi_table_boc2 <- html_nodes(BOC_cpi_page2, 'table')
df_cpi_boc2 <- html_table(cpi_table_boc2, fill=T, header=T)
df_cpi_boc2 <- data.frame(df_cpi_boc2[1])
db_cpi2 <- df_cpi_boc2[order(-1:-nrow(df_cpi_boc2)), 5:6]
db_cpi2 <- db_cpi2[1:(nrow(db_cpi2)-2),]
db_cpi2 <- as.xts(ts(db_cpi2, start=c(2000, 1), frequency=12))

# This last need aggregation:
# each monthly obs is subset in quarters which take
# the mean value over the three obs
db_cpi2 <- aggregate(db_cpi2, as.yearqtr(as.yearmon(time(db_cpi2))), mean)

# housekeeping
rm(BOC_cpi_page2, df_cpi_boc2, cpi_table_boc2)

names(db_cpi2) <- c('cpi_full_yoy', 'core_cpi_yoy')


# BoC key policy rates scraping
# "Bank" goes up to 90's (check 1996) as key policy tool
# then it becomes "overnight target rate" up to present time
# ref. to http://www.bankofcanada.ca/core-functions/monetary-policy/key-interest-rate/
# for chronicle of the shifts

# this URL must change at each iteration,
# so one need to provide Sys.Date in the URL directly
firstpt <- 'http://www5.statcan.gc.ca/cansim/a20?NOSEC=false&VEC=122530&action%3Aa20=Apply&whenConvertingFrequency=USE_CALENDAR_YEAR&eyear='
year <- as.numeric(format(Sys.Date(), '%Y'))
secondpt <- '&exporterId=SERIES_HTML_TIME_AS_ROW&outputFrequency=QUARTERLY_AVERAGE&smonth=1&printerFriendly=true&lang=eng&syear=1935&id=1760043&emonth=12&stByVal=2&accessible=false&retrLang=eng&manipulationOption=DATA_AS_RETRIEVED'

BOC_rate_page <- read_html(paste0(firstpt,year,secondpt))
BOC_tables <- html_nodes(BOC_rate_page, 'table')
df_rates_boc <- html_table(BOC_tables, fill=T, header=T)
df_rates_boc <- data.frame(df_rates_boc)
df_rates_boc <- data.frame(df_rates_boc[1:(nrow(df_rates_boc)-1),2])
names(df_rates_boc) <- 'bank_rate'

# reconversion in numeric - coerces a NA if empty cell
df_rates_boc$bank_rate <- as.numeric(as.character(df_rates_boc$bank_rate))
cat('Produces warnings but for filling empty cells -- if any.')

## here for references
## http://www.bankofcanada.ca/rates/interest-rates/selected-historical-interest-rates/

# going TS
db_bankrate_boc <- as.xts(ts(df_rates_boc, start=c(1935, 1), frequency=4))


# adding the target rate from 2000 on
unopt <- 'http://www5.statcan.gc.ca/cansim/a20?NOSEC=false&VEC=39079.-29&action%3Aa20=Apply&whenConvertingFrequency=USE_CALENDAR_YEAR&eyear='
duept <- '&exporterId=SERIES_HTML_TIME_AS_ROW&outputFrequency=QUARTERLY_AVERAGE&smonth=1&printerFriendly=true&lang=eng&syear=1991&id=1760048&emonth=9&stByVal=2&accessible=false&retrLang=eng&manipulationOption=DATA_AS_RETRIEVED'

# download and data framing
BOC_targ_rate_page <- read_html(paste0(unopt,year,duept))
BOC_targ_tables <- html_nodes(BOC_targ_rate_page, 'table')
df_targ_rates_boc <- html_table(BOC_targ_tables, fill=T, header=T)
df_targ_rates_boc <- data.frame(df_targ_rates_boc)
df_targ_rates_boc <- data.frame(df_targ_rates_boc[1:(nrow(df_targ_rates_boc)-1),2])
names(df_targ_rates_boc) <- 'targ_rate'
df_targ_rates_boc$targ_rate <- as.numeric(as.character(df_targ_rates_boc$targ_rate))
cat('Produces warnings but for filling empty cells -- if any.')

# going TS
db_targ_rate_boc <- as.xts(ts(df_targ_rates_boc, start=c(1991, 1), frequency=4))

db_composed <- rbind(db_bankrate_boc['/1999-12'], db_targ_rate_boc['2000-01/'])
colnames(db_composed) <- 'key_rate'

# Merging in one time series database
# and writing to formatted txt file
db_canada <- merge(db_targ_rate_boc, db_bankrate_boc, db_composed, db_cpi, db_cpi2, db_ygap)

write.zoo(db_canada, file.path(getwd(), 'Data', 'canada_data_1935Q1.txt'), sep=';')

# housekeeping
rm(firstpt, secondpt, BOC_rate_page, BOC_tables, df_rates_boc,
   unopt, duept, BOC_targ_tables, BOC_targ_rate_page, df_targ_rates_boc,
   db_targ_rate_boc, db_cpi, db_ygap, db_composed, db_cpi2, db_bankrate_boc)

cat('Bank of Canada data obtained and saved in \'db_canada\'')




#### UK  ####

# getting rates data from html format
boe_page_rates <- read_html('http://www.bankofengland.co.uk/boeapps/iadb/fromshowcolumns.asp?Travel=NIxIRxSUx&FromSeries=1&ToSeries=50&DAT=ALL&VFD=N&html.x=26&html.y=21&CSVF=TT&C=E32&Filter=N')
boe_table_rates <- html_nodes(boe_page_rates, 'table')
df_rates_boe <- html_table(boe_table_rates, fill=T, header=T)
df_rates_boe <- data.frame(df_rates_boe[2])
df_rates_boe <- data.frame(df_rates_boe[6:nrow(df_rates_boe),2])
names(df_rates_boe) <- 'ref_rate_eng'
df_rates_boe$ref_rate_eng <- as.numeric(as.character(df_rates_boe$ref_rate_eng))

# stocks reference rate in TS w/ quarters
rate_boe <- as.xts(ts(df_rates_boe, frequency=4, start=c(1975, 1)))


# data on inflation, cpi
onu <- 'https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7g7/mm23&series=&fromQuarter=Q1&fromYear=1989&toQuarter=Q4&toYear='
eud <- '&frequency=quarters'
cpi_tab_boe <- read_csv(paste0(onu, year, eud), #year provides current year so to have updated data
                        col_names=c('date', 'cpi'), skip=6)
cpi_boe <- as.xts(ts(cpi_tab_boe$cpi, start=c(1989, 1), frequency=4))

#######################################
### MANAGE TO FIND OUT UNEMPLOYMENT ###
#######################################
### MANAGE TO FIND OUT UNEMPLOYMENT ###
#######################################
### MANAGE TO FIND OUT UNEMPLOYMENT ###
#######################################

# getting series on gdp growth and filtering out
# cyclical component
un <- 'https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ihyq/pgdp&series=&fromQuarter=Q1&fromYear=1955&toQuarter=Q4&toYear='
dos <- '&frequency=quarters'
gdp_tab_uk <- read_delim(paste0(un,year,dos), skip=6, col_names=F, delim=',')

# filtering
ygaps_uk <- hpfilter(gdp_tab_uk[,2], freq=1600)$cycle
ygaps_uk <- as.xts(ts(ygaps_uk, start=c(1955, 2), frequency=4))
colnames(ygaps_uk) <- 'ygap_uk'

# merging series in one dataset
db_uk <- merge(rate_boe, cpi_boe, ygaps_uk)

# housekeeping
rm(onu, eud, cpi_tab_boe, boe_table_rates, boe_page_rates,
   df_rates_boe, un, dos, gdp_tab_uk, ygaps_uk, cpi_boe, rate_boe)

# pulling out to formatted txt
write.zoo(db_uk, file.path(getwd(), 'Data', 'uk_data_1955Q2.txt'), sep=';')

cat('Bank of England and ONS data obtained and saved in \'db_uk\'')




#### Norway ####

cpi_tab_nor <- read_delim('https://www.ssb.no/eksport/tabell.csv?key=277862',
                    col_names=T, delim=';')
cpi_tab_nor[cpi_tab_nor=='.'] <- NA
head(cpi_tab_nor)

df_cpi_nor <- data.frame(cpi_tab_nor[,1], cpi_tab_nor$Mar, 
                         cpi_tab_nor$Jun, cpi_tab_nor$Sep, cpi_tab_nor$Dec)
names(df_cpi_nor) <- c('year', 'mar', 'jun', 'sept', 'dec')

# total failure in the reshaping thing, to rework
# df_cpi_nor2 <- reshape(df_cpi_nor, direction='long', varying=c('mar', 'jun', 'sept', 'dec'), v.names='year')
# head(df_cpi_nor2)


# importing key interest rate data
rate_site_nor <- read_html('http://www.norges-bank.no/en/Statistics/Interest-rates/Key-policy-rate-monthly/')
rate_table_nor <- html_nodes(rate_site_nor, 'table')
df_rates_nor <- html_table(rate_table_nor, fill=T, header=T)
df_rates_nor <- data.frame(df_rates_nor)
df_rates_nor <- df_rates_nor[order(-1:-nrow(df_rates_nor)),]
db_rates_nor <- as.xts(ts(df_rates_nor[, 2:4], start=c(1982, 2), frequency=12))
db_rates_nor <- as.xts(aggregate(db_rates_nor, as.yearqtr(as.yearmon(time(db_rates_nor))), mean))
colnames(db_rates_nor) <- c('pol_rate', 'reserves_rate', 'overnight_rate')

composed_rate_nor <- rbind(as.xts(db_rates_nor$pol_rate['/1993-09']), 
                       as.xts(db_rates_nor$overnight_rate['1993-10/']))
colnames(composed_rate_nor) <- 'composed_rate_nor'

# following the bank policy, after summer '93 the bank
# switched from pol_rate to overnight_rate as main policy tool
db_rates_nor <- merge(db_rates_nor, composed_rate_nor)

# importing GDP growth rate and HP-filter it to 
# obtain cycles. Lambda at 1600
gdp_tab_nor_mess <- read_delim('http://www.ssb.no/en/nasjonalregnskap-og-konjunkturer/tables/_attachment/103320?_ts=156db890408',
                          delim=';', skip=46, col_names=F)
gdp_ser_nor <- data.frame(t(gdp_tab_nor_mess[1:2,-(1:5)]))
names(gdp_ser_nor) <- c('gdp_nor', 'gdp_mainland')
gdp_ser_nor$gdp_nor <- as.numeric(as.character(gdp_ser_nor$gdp_nor))
gdp_ser_nor$gdp_mainland <- as.numeric(as.character(gdp_ser_nor$gdp_mainland))

gdp_ser_nor <- as.xts(ts(gdp_ser_nor, start=c(1978, 1), frequency=4))

# filtering
ygaps_nor <- hpfilter(gdp_ser_nor[1:(nrow(gdp_ser_nor)-1),1], freq=1600)
ygaps_nor_mainland <- hpfilter(gdp_ser_nor[1:(nrow(gdp_ser_nor)-1),2], freq=1600)

ygaps_nor <- as.xts(ts(ygaps_nor$cycle, start=c(1978, 1), frequency=4))
colnames(ygaps_nor) <- 'ygaps_nor'
ygaps_nor_mainland <- as.xts(ts(ygaps_nor_mainland$cycle, start=c(1978, 1), frequency=4))
colnames(ygaps_nor_mainland) <- 'ygaps_nor_mainland'

ygaps_nor <- merge(ygaps_nor, ygaps_nor_mainland)

##############################
# Exchange rate and deviations from unemployment trend
# might also help to proxy for economic activity,
# worth checking is also net business opening
# and layoffs
##############################

# importing exchange rate, but which one is best?

db_nor <- merge(db_rates_nor, ygaps_nor) # missing is the cpi values

# pulling out to formatted txt
write.zoo(db_nor, file.path(getwd(), 'Data', 'nor_data_1978Q1.txt'), sep=';')

cat('Norges Bank and Statistics Norway data obtained and saved in \'db_nor\'')

# housekeeping
rm(cpi_tab_nor, df_cpi_nor, df_cpi_nor2, rate_site_nor,
   rate_table_nor, df_rates_nor, composed_rate_nor, gdp_tab_nor_mess,
   gdp_ser_nor, ygaps_nor_mainland, db_rates_nor, ygaps_nor)




#### Denmark ####





