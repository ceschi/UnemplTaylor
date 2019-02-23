  ##### Specifically designed functions ####

# A file to gather all home made functions with relative descriptions


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



rollm <- function(df, formula){
  # function to extract and store coefficients 
  # and double SD in a named row tibble
  
  
  # estimates the linear model
  lmod <- summary(lm(data=df, formula=formula))
  
  # extracts point estimates and 2*SD (+- 95%),
  # put info in named row tibble dropping 
  # intercept info from first column
  cofs <- as.tibble(coefficients(lmod)[2:(lmod %>% coefficients() %>% 
                                            t() %>% ncol()),1] %>% t())
  SD2 <- as.tibble(2*coefficients(lmod)[2:(lmod %>% coefficients() %>% 
                                            t() %>% ncol()),2] %>% t())
  
  # adds suffix for bands
  names(SD2) <- paste0(names(SD2), '.SD2')
  
  # merges in one row with names
  estim <- cbind(cofs, SD2)
  
  # outputs
  return(estim)
}

rolloop <- function(df, window=8, lags=1){
  
  # width of the rolling window
  window <- as.integer(window)
  
  # select lags 
  k <- as.integer(lags)
  
  # lags the time series, names it, cuts out NAs
  df <- df %>% lagger(lag=k, na.cut=T)
  # and creates related formula
  formulae <- formula.maker(df, df %>%  names(.) %>% first())
  
  # computes point estimates and 2SD
  # stocks in a dataframe for convenience
  regs <-rollapply(as.data.frame(df),
                   width=window,
                   by.column = F,
                   FUN=function(x, formula) rollm(df=as.data.frame(x), formula=formulae))
  
  # converts and dates the regressions
  regs <- xts(regs, frequency=4, 
              order.by=index(df)[window:length(index(df))])
  
return(regs)
}


repara <- function(x, rho=4){
  # function to reparametrize once a lm is estimated 
  # having on the 4th place the persistence parameter for FFR
  
  
  params <- coef(summary(x))[,1:2]/(1-coef(x)[rho])
  params[rho,] <- coef(summary(x))[rho, 1:2]
  return(params)
}


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


formula.maker <- function(df, y){
  # provided with a df and a dependent variable name
  # this generates a formula for estimation in R, y is the 
  # dependent variable, all the others are considered
  # independent and explanatory ones
  
  
  fomu <- as.formula(paste(y, 
                           paste(names(df)[names(df)!=y], collapse='+'),
                           # paste(c(0,names(df)[names(df)!=y]), collapse='+'),
                           # this prevents to have a constant but breaks the
                           # functioning of the code 
                           sep='~'))
  attr(fomu, which='.Environment') <- .GlobalEnv
  return(fomu)
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
  
  if (class(tseries) %in% c('zoo', 'ts', 'xts')){
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


fredr_down <- function(mnem, name, freq = 'q'){
  series <- fredr_series_observations(series_id = mnem,
                                      frequency = freq) %>% 
    tbl_xts()
  
  gather_series <- merge(series,
                         diff(log(series))*100,
                         hamil_filter(series))
  
  names(gather_series) <- paste(rep(name, 3), c('', '_g', '_filtered'), sep = '')
  
  return(gather_series)
  
}


##### Packages Loader #####

pkgs <- c('glue', 
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
          'tictoc')
# fill pkgs with names of the packages to install

devtools::install_github('sboysel/fredr')

instant_pkgs(pkgs)


rm(pkgs)
