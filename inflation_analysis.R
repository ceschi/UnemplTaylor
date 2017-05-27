##### Code for Inflation analysis #####


##### Packages #####

library(ggplot2)
library(tseries)
library(mFilter)
library(tidyverse)
library(xts)
library(MSwM)
library(strucchange)

##### Functions #####

# ts.plotter <- function(df, x, y, col, title){
#   
# return(
#     ggplot()+theme_bw()+ggtitle(title)+labs('leg.')+
#       ylab('')+xlab('')+theme(legend.position='top')+
#       geom_line(data=df, aes(x=x, y=y, color=col))
#   )
# }

lagger <- function(series, lag){
  matrix <- as.data.frame(matrix(ncol=lag+1, nrow=nrow(series)))
  for (i in 1:lag+1){
    matrix[,i] <- stats::lag(series, k=(i-1))
  }
  names(matrix) <- c(names(series), paste(names(series), 1:lag, sep='.'))
  matrix[, 1] <- series
  return(matrix)
}

formula.maker <- function(df, y){
  fomu <- as.formula(paste(y, 
                           paste(names(df)[names(df)!=y], collapse='+'),
                           sep='~'))
  attr(fomu, which='.Environment') <- .GlobalEnv
  return(fomu)
}

##### Data extraction #####

us_pi <- merge(db_US$deflt, db_US$deflt1)


#### Data visualization ####



plot_pi <- ggplot(na.omit(us_pi))+
  geom_line(aes(x=index(na.omit(us_pi)), y=deflt, color='t=0'), na.rm = T)+
  geom_line(aes(x=index(na.omit(us_pi)), y=deflt1, color='t=1'), na.rm = T)+
  ylab(' ')+xlab(' ')+theme(legend.position='top', legend.title = NULL)+
  theme_bw()+ggtitle('Nowcast and 1Q ahead forecast')
print(plot_pi)


plot_hist <- ggplot(data=us_pi)+geom_density(aes(x=deflt, fill='now'), alpha= .5)+  labs(' ')+
    geom_density(aes(x=deflt1, fill='1 ahead'), alpha=.5)+theme_bw()+
    scale_fill_manual( values = c("red","blue"), labels = c('t=0', 't=1'), name=' ')+ xlab('Inflation rates')+
    ggtitle('Distribution of the inflation rates')
print(plot_hist)


##### First battery of tests on the two series #####

# unit root
adf.test(na.omit(us_pi$deflt))
adf.test(na.omit(us_pi$deflt1))
cat('both series show signs of unit root, although deflt is stronger in this result')

#######################################
#### FIND WAY TO MINIMIZE BIC #########
###### WHILE SELECTING LAGS ###########
#######################################

# ref for this in urca::ur.df()


# AR(5) fitting and plotting with bands

lag_deflt <-  unname(adf.test(na.omit(us_pi$deflt))$parameter)
lag_deflt1 <- unname(adf.test(na.omit(us_pi$deflt1))$parameter)

ar_t <- arma(na.omit(us_pi$deflt), order=c(lag_deflt, 0))
ar_t1 <- arma(na.omit(us_pi$deflt1), order=c(lag_deflt1, 0))

cat('\nFor the first series the lags estimates are\n'); print(summary(ar_t))
cat('\n\n\n\n\nFor the second series the lags estimates are\n'); print(summary(ar_t1))


resid_ar_t <- ar_t$residuals
ts.plot(resid_ar_t, main='Residuals plot, t=0',
        ylab=paste('AR(', lag_deflt,') resid for t=0'))
abline(h=mean(resid_ar_t, na.rm=T), col='red')
abline(h=2*sd(resid_ar_t, na.rm=T), col='blue')
abline(h=-2*sd(resid_ar_t, na.rm=T), col='blue')

resid_ar_t1<- ar_t1$residuals
ts.plot(resid_ar_t1, main='Residuals plot, t=1',
        ylab=paste('AR(', lag_deflt1,') resid for t=1'))
abline(h=mean(resid_ar_t1, na.rm=T), col='red')
abline(h=2*sd(resid_ar_t1, na.rm=T), col='blue')
abline(h=-2*sd(resid_ar_t1, na.rm=T), col='blue')

rollvar_t <- rollapply(resid_ar_t, 8, var)
rollvar_t1 <- rollapply(resid_ar_t1, 8, var)

plot_rollvar <- ggplot(data=data.frame(rollvar_t, rollvar_t1))+
  geom_line(aes(x=index(rollvar_t), y=rollvar_t), color=variable)+
  geom_line(aes(x=index(rollvar_t1), y=rollvar_t1), color=variable)+
  labs(' ')+theme_bw()+#scale_fill_manual(values=c('red','blue'), labels=c('h=0', 'h=1'), name=' ')+
  scale_colour_manual(values=c("red","green"))+
  xlab('Time')+ylab(' ') + ggtitle('Rolling variances of the residuals')
print(plot_rollvar)


# HP filter on the two series to recover components

comp_deflt  <- mFilter(na.omit(us_pi$deflt), 'HP', freq=4)
comp_deflt1 <- mFilter(na.omit(us_pi$deflt1), 'HP', freq=4)


##### Brutti grafici da migliorare #####
# possibilmente con ggplot2 

par(mfrow=c(2,1), mar=rep(4, 4))
plot.ts(comp_deflt$trend, main='Trend', xlab='time', ylab='t=0')
plot.ts(comp_deflt1$trend, xlab='time', ylab='t=1')

plot.ts(comp_deflt$cycle, main='Cycle', xlab='time', ylab='t=0')
plot.ts(comp_deflt1$cycle, xlab='time', ylab='t=1')


##### Markov Switching on the series #####

# lagged matrices
mlag_t <- as.xts(lagger(us_pi$deflt, lag_deflt), order.by=index(us_pi))
mlag_t1 <- as.xts(lagger(us_pi$deflt1, lag_deflt1), order.by=index(us_pi))

# AR regressions
for_t <- formula.maker(mlag_t, 'deflt')
for_t1 <- formula.maker(mlag_t1, 'deflt1')

msm_ar_t <- msmFit(for_t, k=2, sw=rep(T, (lag_deflt+2)),data=as.data.frame(mlag_t))
msm_ar_t1 <- msmFit(for_t1, k=2, sw=rep(T, (lag_deflt1+2)), data=as.data.frame(mlag_t1))

cat('\nResults for 2-state MSModel on deflator nowcast (t=0):\n')
    print(summary(msm_ar_t))
cat('\nResults for 2-state MSModel on expected deflator (t=1):\n')
    print(summary(msm_ar_t1))

plotProb(msm_ar_t, 2)
plotProb(msm_ar_t, 1)

plotProb(msm_ar_t1, 2)
plotProb(msm_ar_t1, 1)

cat('There is great variability in the state switching, 
    no clear pravailing of one state
    over the other for long periods of time in the sample')


##### Break analysis ####


# diagnostics with CUSUM
cusum_ar_t <- efp(for_t, data=as.data.frame(mlag_t), type='OLS-CUSUM')
plot(cusum_ar_t, alpha=.01, boundary=T)
cat('\n OLS-CUSUM test does not provide any hint of structural change in the \'delft\' time series')

# diagnostics with Fstats
Fstat_t <- Fstats(for_t, data=as.data.frame(mlag_t))
plot(Fstat_t)
breakpoints(Fstat_t)
bp_t <- breakpoints(for_t, data=as.data.frame(mlag_t))
summary(bp_t)
cat('\nThe most likely break for \'deflt\' is at date ', 
    toString(index(na.omit(mlag_t))[Fstat_t$breakpoint]))

# diagnostics with CUSUM
cusum_ar_t1 <- efp(for_t1, data=as.data.frame(mlag_t1), type='OLS-CUSUM')
plot(cusum_ar_t1, alpha=.01, boundary=T)

# diagnostics with Fstats
Fstat_t1 <- Fstats(for_t1, data=as.data.frame(mlag_t1))
plot(Fstat_t1)
breakpoints(Fstat_t1)
bp_t1 <- breakpoints(for_t1, data=as.data.frame(mlag_t1))
summary(bp_t1)
cat('\nThe most likely break for \'deflt1\' is at date ', 
    toString(index(na.omit(mlag_t1))[Fstat_t1$breakpoint]))

# For both series there is a break at the end of 1980, might
# be worth it to check back the time series for such date


##### TENTATIVE VAR #####
library(vars)
varia <- merge(db_US$realtime_gap, db_US$ffr, db_US$deflt1, db_US$deflt, db_US$spread_sp_3m)

vect <- VAR(varia['1967-01/2011-12'], lag.max=8, type='const', ic='AIC')
summary(vect)
impulses <- irf(vect)

amatrix <- matrix(c(NA,NA,NA,0,0,
            NA,NA,NA,0,NA,
            0,NA,NA,0,0,
            NA,0,NA,NA,0,
            NA,NA,0,0,NA), 
            nrow=5, ncol=5, byrow=T)

bmatrix <- matrix(c(NA,NA,NA,0,0,
                    NA,NA,NA,0,NA,
                    0,NA,NA,0,0,
                    NA,0,NA,NA,0,
                    0,0,NA,0,NA), 
                  nrow=5, ncol=5, byrow=T)
sbar <- SVAR(vect, Amat=amatrix, estmethod='direct', hessian=T, method='CG')#, start=rep(1, 10))
summary(sbar)

impulseSVAR <- irf(sbar)
plot(impulseSVAR)

# housekeeping
# dev.off()
# rm(lag_deflt, lag_deflt1, us_pi, comp_deflt, comp_deflt1,
# for_t, for_t1, mlag_t, mlag_t1
# )














