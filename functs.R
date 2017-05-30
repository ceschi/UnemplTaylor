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
    for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
  }
  
  if (length(need_to_attach) == 0) {
    message("\n ...Packages were already loaded!\n")
  }
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
    #supp <- as.numeric(indice[i])
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
    supp <- as.numeric(indice[i])
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


lagger <- function(series, lag){
  # Takes a time series and creates a matrix with given number
  # of lags, also generating appropriate names
  
  
  matrix <- as.data.frame(matrix(ncol=lag+1, nrow=nrow(series)))
  for (i in 1:lag+1){
    matrix[,i] <- stats::lag(series, k=(i-1))
  }
  names(matrix) <- c(names(series), paste(names(series), 1:lag, sep='.'))
  matrix[, 1] <- series
  return(matrix)
}


formula.maker <- function(df, y){
  # provided with a df and a dependent variable name
  # this generates a formula for estimation in R, y is the 
  # dependent variable, all the others are considered
  # independent and explanatory ones
  
  
  fomu <- as.formula(paste(y, 
                           paste(names(df)[names(df)!=y], collapse='+'),
                           sep='~'))
  attr(fomu, which='.Environment') <- .GlobalEnv
  return(fomu)
}






##### Packages Loader #####

pkgs <- c('vars', 'MSwM','tidyverse',
          'tseries', 'dynlm', 'stargazer',
          'dyn', 'strucchange', 'xts',
          'MASS', 'car', 'ggplot2',
          'mFilter', 'fredr', 'xlsx',
          'dplyr', 'readr', 'quantmod',
          'devtools')
# fill pkgs with names of the packages to install

instant_pkgs(pkgs)

## part needed for handling Yahoo! finance data,
## supposed to change w/ next release of 'quantmod' pkg
## requires installation of 'Rtools' to compile

devtools::install_github("joshuaulrich/quantmod", ref="157_yahoo_502")
library(quantmod)

rm(pkgs)