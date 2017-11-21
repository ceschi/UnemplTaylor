#### Main file for Taylor rule second branch of rsch ####
#### using unemployment metrics to proxy output gap #####


#### Flagging

# 0 -- the code runs entirely, all different parts composed
# 1 -- when files are run singularily
flag___singular = 0

# 0 -- the code prints out all graphs
# 1 -- graphs are not printed but only produced and stocked
flag___plot = 1

# 0 -- optimal lags for inflation are off
# 1 -- optimal lags for inflation are on
flag___optilag = 1

# 0 -- MsM estimation is off
# 1 -- MsM estimation is on, 2 states
# 2 -- MsM estimation is on, 3 states
flag___msm = 1


#### Functions #####
source('functs.R', verbose=F, echo=F)

#### Directories
source('directories.R', verbose=F, echo=F)

#### DATA COLLECTION, SCRAPING, MANIPULATION ####
# US Data
# pick ahead to set how many quarters ahead 
# to consider for SPF forecasts:
# -1 for previous quarter estimates
# 0 for nowcast
# 1 for one quarter ahead -- default
# 2 for two quarters ahead
# 3 for three quarters ahead
# 4 for one year ahead

ahead <- 1
source("USdatacoll.R", verbose=F, echo=F)


#### REGRESSIONS - TR bulk ####

source('USreg.R', verbose=F, echo=F)




#### Analysis on inflation ####
# AR(k) on several inflation series


# exogenous lag
k=5

# selector for coefficient
# AR(r) will be plotted
# MUST be <k
r=1

# select window width for
# rolling estimates, pick <80
# to get interesting results
wind=58

source('inflanalysis.r')


##### VISUALIZATION ####
source('visuals.R', verbose=F, echo=F)



# housekeeping
rm(temp_dir, data_dir, graphs_dir, 
   working_directory, flag___singular,
   flag___msm, flag___optilag, flag___plot,
   ahead, k, r, wind)