##### FRED #####################################################################
# miniscript to import and harmonise data from official FRED sources ###########

##### import Q, set up merger with db_US #######################################
# read in the 'raw' file from connection
db_US_q <- read.delim(file = 'https://s3.amazonaws.com/files.fred.stlouisfed.org/fred-md/quarterly/current.csv',
                      header = T, 
                      sep = ',',
                      na.strings = '')

# drop row with metadata
db_US_q <- db_US_q[-(1:2),]

# reformat date
db_US_q$sasdate <- as.Date(as.character(db_US_q$sasdate), 
                           format = '%m/%d/%Y') %>% 
  as.yearqtr() %>% 
  as.Date()

# rename
colnames(db_US_q)[1] <- 'date'

# transform to xts format
db_US_q <- xts::xts(db_US_q, order.by = db_US_q$date)




##### import M, set up xts #####################################################
# read in the 'raw' file from connection
db_US_m <- read.delim(file = 'https://s3.amazonaws.com/files.fred.stlouisfed.org/fred-md/monthly/current.csv',
                      header = T, 
                      sep = ',',
                      na.strings = '')

# drop row with metadata and last with NAs
db_US_m <- db_US_m[-c(1, nrow(db_US_m)),]

# reformat date
db_US_m$sasdate <- as.Date(as.character(db_US_m$sasdate),
                           format = '%m/%d/%Y')%>% 
  as.yearmon() %>% 
  as.Date()

# rename
colnames(db_US_m)[1] <- 'date'

# transform to xts format
db_US_m <- xts::xts(db_US_m, order.by = db_US_m$date)


##### write both to disk #######################################################
write.zoo(x = db_US_q,
          file = file.path(data_dir, 'FRED_QD.csv'),
          col.names = T)


write.zoo(x = db_US_m,
          file = file.path(data_dir, 'FRED_MD.csv'), 
          col.names = T)