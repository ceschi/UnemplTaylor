#### Main file for Taylor rule second branch of rsch ####

#### Second venue of research
#### using unemployment metrics to proxy output gap ####

# side goal: integrate smoothly this with previous codes
# for US (Trulyfinal.R) and for the panel building


#### Flagging

# 0 -- the code runs entirely, all different parts composed
# 1 -- when files are run singularily
flag___singular = 0

# 0 -- the code prints out all graphs
# 1 -- graphs are not printed but only produced and stocked
flag___plot = 1

# 0 -- optimal lags for inflation are off
# 1 -- optimal lags for inflation are on
flag___optilag = 0

# 0 -- MsM estimation is off
# 1 -- MsM estimation is on, 2 states
# 2 -- MsM estimation is on, 3 states
flag___msm = 1


#### Functions #####
source('functs.R', verbose=F, echo=F)

#### Directories
source('directories.R', verbose=F, echo=F)

#### DATA COLLECTION, SCRAPING, MANIPULATION ####
source("USdatacoll.R", verbose=F, echo=F)


#### REGRESSIONS - TR bulk ####

# US Data
source('USreg.R', verbose=F, echo=F)


#### REGRESSIONS - inflation study  ####

source('inflanalysis.R')


##### VISUALIZATION ####
source('visuals.R', verbose=F, echo=F)



# housekeeping
rm(temp_dir, data_dir, graphs_dir, 
   working_directory, flag___singular)