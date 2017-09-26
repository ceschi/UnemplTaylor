#### Main file for Taylor rule second branch of rsch ####

#### Second venue of research
#### using unemployment metrics to proxy output gap ####

# side goal: integrate smoothly this with previous codes
# for US (Trulyfinal.R) and for the panel building


#### Flagging

flag___singular = 1
# 0 -- the code runs entirely, all different parts composed
# 1 -- when files are run singularily

#### Functions #####
source('functs.R', verbose=F, echo=F)

#### Directories
source('directories.R', verbose=F, echo=F)

#### DATA COLLECTION, SCRAPING, MANIPULATION ####

source("USdatacoll.R", verbose=F, echo=F)


##### VISUALIZATION ####

source('visuals.R', verbose=F, echo=F)


#### REGRESSIONS - SIMPLE ONES ####

# US Data
source('USreg.R', verbose=F, echo=F)


#### REGRESSIONS - MORE SOPHISTICATED ####

source('inflation_analysis.R')



# housekeeping
rm(temp_dir, data_dir, graphs_dir, 
   working_directory, flag___singular)