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



reg_call <- function(m){
  # custom function to extract, print and plot 
  # information on estimates of a particular
  # Taylor rule specification. The latter is selected
  # by specifying m, listed below
  
  # # 1
  # 'Standard TR',
  # # 2
  # 'TR with layoffs replacing output gap',
  # # 3 
  # 'TR and BAA spread',
  # # 4
  # 'TR and 3M spread',
  # # 5
  # 'TR with layoffs and 3M spread',
  # # 6
  # 'TR with layoffs and BAA spread',
  # # 7
  # 'TR with SPF mean expected inflation',
  # # 8
  # 'TR augmented with IQR SPF'
  
  # sink fnct saves in a txt file
  # the output while printing it out
  # on the command line
  
  sink(file=paste0(file.path(graphs_dir, regressions$messages[[m]]), ' regressions results.txt'),
       append=F,
       split=T,
       type='output')
  
  sa_plot <- function(po){
    # custum function to duplicate, save as pdf 
    # and shut second graphic device
    dev.copy(pdf, po, height=8, width=14.6)
    invisible(dev.off())
    
  }
  
  
  # prints the name of the model
  cat(paste0('\n\n', as.character(regressions$messages[[m]]), '\n'))
  
  # prints the estimated formula
  cat('\n')
  print(regressions$formula[[m]])
  cat('\n')
  
  # prints the number of observations used in the model
  cat(paste0('\nModel estimated with ', nobs(regressions$models[[m]]), ' observations\n\n'))
  
  # prints converted parameters + SE
  print(regressions$params[[m]])

  # plots the residuals + SE bands for stability
  print(regressions$plot[[m]])

  # plots cusum stability diagnostics
  plot(regressions$stab$cusum[[m]], alpha=.01, boundary=T)
  sa_plot(paste0(file.path(graphs_dir, regressions$messages[[m]]), ' CUSUM.pdf'))
  
  # plots Fstat stability diagnostics
  plot(regressions$stab$fstat[[m]])
  title(main=paste0(regressions$messages[[m]], ': F-stat stability'),
        sub=paste0('Vertical line indicates date of most likely break: ', 
                   regressions$stab$fstatpoints[[m]]))
  lines(breakpoints(regressions$stab$fstat[[m]]))
  sa_plot(paste0(file.path(graphs_dir, regressions$messages[[m]]), ' F-stat.pdf'))
  
  # prints date of most likely break
  cat(paste0('\nMost likely singular break occurs at ',
             as.character(regressions$stab$fstatpoints[[m]]), '\n'))
  regressions$stab$fstatcandidates[[m]]
  
  # optimal number of segment partition,
  # -1 to account for the 0-breaks case
  fstat_dates <- which(summary(regressions$stab$fstatcandidates[[m]])$RSS[2,]==
                         min(summary(regressions$stab$fstatcandidates[[m]])$RSS[2,]), arr.ind=T)-1
  
  # extracting corresponding nobs and dates
  n_obs <- summary(regressions$stab$fstatcandidates[[m]])$breakpoints[fstat_dates,] %>% na.omit(.)
  multibreaks <- names(regressions$stab$fstatcandidates[[m]]$y)[n_obs] %>% paste(collapse=', ')
  
  # printing optimal segment partition dates
  cat(paste0('while optimal segmentation points to ', length(n_obs), ' breaks, at dates ', multibreaks))
  
  
  # MSwM printig results and plotting
  if (flag___msm!=0){
    cat('\n\nMarkov Switching model estimation with', j, 'states')
    cat('\n')
    cat(summary(regressions$mswm$fit[[m]]))
    cat('\n\nConverted parameters:\n')
    print(regressions$mswm$coefs[[m]])
    
    # fine tuning plots
    par(mar=c(1,1,2.85,1), cex.main=.85)
    plotProb(regressions$mswm$fit[[m]], which=2)
    title(paste0(j, '-state MS regimes for ', regressions$messages[[m]]), line=2.3)
    sa_plot(file.path(graphs_dir,paste0(regressions$messages[[m]], ' ',
                      j,'-state MSM.pdf')))
    
    # silently setting margins to default values
    invisible(dev.off())
  }
  
  # VAR results for TR equation
  # this does ignore all other results
  print(summary(regressions$var$varfit[[m]], equation='ffr'))
  
  # plots and saves IRFs
  plot(regressions$var$varirf[[m]])
  title(paste0(regressions$messages[[m]], ' VAR IRFs, MonPol shock'), line=9.5)
  sa_plot(file.path(graphs_dir, paste0(regressions$messages[[m]], ' VAR model IRFs.pdf')))
  
  # SVAR results restricted to TR
  # thus dropping other eq'ns
  print(summary(regressions$svar$svarfit[[m]], equation='ffr'))
  
  # plots and save SVAR IRFs
  plot(regressions$svar$svarirf[[m]])
  title(paste0(regressions$messages[[m]], ' SVAR IRFs, MonPol shock'), line=9.5)
  sa_plot(file.path(graphs_dir, paste0(regressions$messages[[m]], ' SVAR model IRFs.pdf')))
      
  
  
  # stopping printing
  sink(file=NULL)
}

rollm <- function(df, formula){
  # function to extract and store coefficients 
  # and double SD in a named row tibble
  
  
  # estimates the linear model
  lmod <- summary(lm(data=df, formula=formula))
  
  # extracts point estimates and 2*SD (+- 95%),
  # put info in named row tibble dropping 
  # intercept info from first column
  cofs <- as.tibble(coefficients(lmod)[2:(lmod %>% coefficients() 
                                          %>% t() %>% ncol()),1] %>% t())
  SD2 <- as.tibble(2*coefficients(lmod)[2:(lmod %>% coefficients() 
                                           %>% t() %>% ncol()),2] %>% t())
  
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


lagger <- function(series, lag, na.cut=F){
  # Takes a time series and creates a matrix with given number
  # of lags, also generating appropriate names
  
  
  matrix <- as.data.frame(matrix(ncol=lag+1, nrow=nrow(series)))
  for (i in 1:lag+1){
    matrix[,i] <- stats::lag(series, k=(i-1))
  }
  names(matrix) <- c(names(series), paste(names(series), 1:lag, sep='.'))
  matrix[, 1] <- series
  matrix <- as.xts(matrix, order.by=index(series))
  
  # conditional to remove NAs from output
  if (na.cut){
    matrix <- na.omit(matrix)
  }
  
  # output
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


# modified DF function to extract optimal lags from URCA pkg FAILURE
#########
# urca.df <- function (y, type = c("none", "drift", "trend"), lags = 1, selectlags = c("Fixed", "AIC", "BIC")) 
# {
#   selectlags <- match.arg(selectlags)
#   type <- match.arg(type)
#   if (ncol(as.matrix(y)) > 1) 
#     stop("\ny is not a vector or univariate time series.\n")
#   if (any(is.na(y))) 
#     stop("\nNAs in y.\n")
#   y <- as.vector(y)
#   lag <- as.integer(lags)
#   if (lag < 0) 
#     stop("\nLags must be set to an non negative integer value.\n")
#   CALL <- match.call()
#   DNAME <- deparse(substitute(y))
#   x.name <- deparse(substitute(y))
#   lags <- lags + 1
#   z <- diff(y)
#   n <- length(z)
#   x <- embed(z, lags)
#   z.diff <- x[, 1]
#   z.lag.1 <- y[lags:n]
#   tt <- lags:n
#   
#   if (selectlags != "Fixed") {
#     critRes <- rep(NA, lags)
#     for (i in 2:(lags)) {
#       z.diff.lag = x[, 2:i]
#       if (type == "none") 
#         result <- lm(z.diff ~ z.lag.1 - 1 + z.diff.lag)
#       if (type == "drift") 
#         result <- lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)
#       if (type == "trend") 
#         result <- lm(z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
#       critRes[i] <- AIC(result, k = switch(selectlags, 
#                                            AIC = 2, BIC = log(length(z.diff))))
#     }
#     lags <- optimaxlags <- which.min(critRes)
#     
#   }
#   z.diff.lag = x[, 2:lags]
#   if (type == "none") {
#     result <- lm(z.diff ~ z.lag.1 - 1 + z.diff.lag)
#     tau <- coef(summary(result))[1, 3]
#     teststat <- as.matrix(tau)
#     colnames(teststat) <- "tau1"
#   }
#   if (type == "drift") {
#     result <- lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)
#     tau <- coef(summary(result))[2, 3]
#     phi1.reg <- lm(z.diff ~ -1 + z.diff.lag)
#     phi1 <- anova(phi1.reg, result)$F[2]
#     teststat <- as.matrix(t(c(tau, phi1)))
#     colnames(teststat) <- c("tau2", "phi1")
#   }
#   if (type == "trend") {
#     result <- lm(z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
#     tau <- coef(summary(result))[2, 3]
#     phi2.reg <- lm(z.diff ~ -1 + z.diff.lag)
#     phi3.reg <- lm(z.diff ~ z.diff.lag)
#     phi2 <- anova(phi2.reg, result)$F[2]
#     phi3 <- anova(phi3.reg, result)$F[2]
#     teststat <- as.matrix(t(c(tau, phi2, phi3)))
#     colnames(teststat) <- c("tau3", "phi2", "phi3")
#   }
#   
#   rownames(teststat) <- "statistic"
#   testreg <- summary(result)
#   res <- residuals(testreg)
#   if (n < 25) 
#     rowselec <- 1
#   if (25 <= n & n < 50) 
#     rowselec <- 2
#   if (50 <= n & n < 100) 
#     rowselec <- 3
#   if (100 <= n & n < 250) 
#     rowselec <- 4
#   if (250 <= n & n < 500) 
#     rowselec <- 5
#   if (n >= 500) 
#     rowselec <- 6
#   if (type == "none") {
#     cval.tau1 <- rbind(c(-2.66, -1.95, -1.6), c(-2.62, -1.95, 
#                                                 -1.61), c(-2.6, -1.95, -1.61), c(-2.58, -1.95, -1.62), 
#                        c(-2.58, -1.95, -1.62), c(-2.58, -1.95, -1.62))
#     cvals <- t(cval.tau1[rowselec, ])
#     testnames <- "tau1"
#   }
#   if (type == "drift") {
#     cval.tau2 <- rbind(c(-3.75, -3, -2.63), c(-3.58, -2.93, 
#                                               -2.6), c(-3.51, -2.89, -2.58), c(-3.46, -2.88, -2.57), 
#                        c(-3.44, -2.87, -2.57), c(-3.43, -2.86, -2.57))
#     cval.phi1 <- rbind(c(7.88, 5.18, 4.12), c(7.06, 4.86, 
#                                               3.94), c(6.7, 4.71, 3.86), c(6.52, 4.63, 3.81), 
#                        c(6.47, 4.61, 3.79), c(6.43, 4.59, 3.78))
#     cvals <- rbind(cval.tau2[rowselec, ], cval.phi1[rowselec, 
#                                                     ])
#     testnames <- c("tau2", "phi1")
#   }
#   if (type == "trend") {
#     cval.tau3 <- rbind(c(-4.38, -3.6, -3.24), c(-4.15, -3.5, 
#                                                 -3.18), c(-4.04, -3.45, -3.15), c(-3.99, -3.43, 
#                                                                                   -3.13), c(-3.98, -3.42, -3.13), c(-3.96, -3.41, 
#                                                                                                                     -3.12))
#     cval.phi2 <- rbind(c(8.21, 5.68, 4.67), c(7.02, 5.13, 
#                                               4.31), c(6.5, 4.88, 4.16), c(6.22, 4.75, 4.07), 
#                        c(6.15, 4.71, 4.05), c(6.09, 4.68, 4.03))
#     cval.phi3 <- rbind(c(10.61, 7.24, 5.91), c(9.31, 6.73, 
#                                                5.61), c(8.73, 6.49, 5.47), c(8.43, 6.49, 5.47), 
#                        c(8.34, 6.3, 5.36), c(8.27, 6.25, 5.34))
#     cvals <- rbind(cval.tau3[rowselec, ], cval.phi2[rowselec, 
#                                                     ], cval.phi3[rowselec, ])
#     testnames <- c("tau3", "phi2", "phi3")
#   }
#   colnames(cvals) <- c("1pct", "5pct", "10pct")
#   rownames(cvals) <- testnames
#   new("ur.df", y = y, model = type, cval = cvals, lags = lag, optilags = optimaxlags,
#       teststat = teststat, testreg = testreg, res = res, test.name = "Augmented Dickey-Fuller Test")
# }








##### Packages Loader #####

pkgs <- c('vars', 'glue', 'MSwM', 'lazyeval',
          'quantreg', 'tidyverse', 'devtools',
          'tseries', 'dynlm', 'stargazer',
          'dyn', 'strucchange', 'xts',
          'MASS', 'car', 'rvest',
          'mFilter', 'fredr',
          'readr', 'quantmod',
          'devtools', 'lubridate',
          'readxl', 'VARsignR', 'tbl2xts')
# fill pkgs with names of the packages to install

devtools::install_github('sboysel/fredr')
devtools::install_github('ceschi/urcabis')
#library(urcabis)

instant_pkgs(pkgs)


rm(pkgs)
