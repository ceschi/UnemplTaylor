#### Main file for Taylor rule second branch of rsch ####

#### Second venue of research
#### using unemployment metrics to proxy output gap ####

# side goal: integrate smoothly this with previous codes
# for US (Trulyfinal.R) and for the panel building

#### Functions #####
source('functs.R')

#### Directories
working_directory <- getwd()
temp_dir <- 'Downloaded files'
data_dir <- 'Processed data'
graphs_dir <- 'Plots'
dir.create(file.path(working_directory, temp_dir))
dir.create(file.path(working_directory, data_dir))
dir.create(file.path(working_directory, graphs_dir))

#### DATA COLLECTION, SCRAPING, MANIPULATION ####

source("USdatacoll.R", verbose=F, echo=F)


##### VISUALIZATION ####

source('visuals.R')


#### REGRESSIONS - SIMPLE ONES ####

# US Data
source('USreg.R')


#### REGRESSIONS - MORE SOPHISTICATED ####

source('inflation_analysis.R')



# housekeeping
rm(temp_dir, data_dir, graphs_dir, 
   working_directory)