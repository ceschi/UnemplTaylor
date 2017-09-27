##### US regression #####


#### Functions ####
repara <- function(x, rho=4){
  params <- coef(summary(x))[,1:2]/(1-coef(x)[rho])
  params[rho,] <- coef(summary(x))[rho, 1:2]
  return(params)
}


# consider putting all elements in lists of homogeneous 
# elements and then looping over these ones

regressions <- list(
  formula=list(),
  messages=list(),
  models=list(),
  params=list(),
  plot=list()
)

# Formulas for regressions, appended to first sublist
regressions$formula <- list(
    tr_standard =  ffr ~ deflt + realtime_gap + ffrb,
    # standard TR
    tr_layoff = ffr  ~ deflt + layoffs + ffrb,
    # TR with layoffs
    tr_spread = ffr ~ deflt + realtime_gap + ffrb + spread_baa,
    tr_treasury = ffr ~ deflt + realtime_gap + ffrb + spread_sp_3m,
    tr_layspread = ffr ~ deflt + layoffs + ffrb + spread_sp_3m,
    tr_laybaa = ffr ~ deflt + layoffs + ffrb + spread_baa,
    tr_spf_mean = ffr ~ spf_cpi_h1_mean + realtime_gap + ffrb,
    tr_spf_uncert = ffr ~ deflt + realtime_gap + ffrb + spf_cpi_h1_iqr
    )

### Looping over different specifications

for (m in 1:length(regressions$formula)){
  
  # fit a linear model
  regressions$models[[m]] <- lm(data=db_US, regressions$formula[[m]])
  
  # rescale parameters
  regressions$params[[m]] <- repara(regressions$models[[m]])
  
  # graphing residuals w/ ggplot2
  res <- residuals(regressions$models[[m]])
  res <- data.frame(date=)
}

us_reg_1 <- lm(data=db_US, tr_standard)
summary(us_reg_1)
params_1 <- repara(us_reg_1)
cat('\n Parameters for Taylor Rule regression:\n')
print(params_1)

df <- data.frame(date=names(res) %>% as.yearqtr('%Y Q%q'),
                 res=res)

p <- ggplot(df, aes(x=date, y=res))+geom_line()+scale_x_yearqtr('%YQ%q',20)

ts.plot(us_reg_1$residuals)
abline(h=0, col='red')
abline(h=2*sd(us_reg_1$residuals), col='blue')
abline(h=-2*sd(us_reg_1$residuals), col='blue')


us_reg_2 <- lm(data=db_US, tr_layoff)
summary(us_reg_2)
params_2 <- repara(us_reg_2)
cat('\n Parameters for realtime output gap with layoff rate:\n')
print(params_2)

ts.plot(us_reg_2$residuals)
abline(h=0, col='red')
abline(h=2*sd(us_reg_2$residuals), col='blue')
abline(h=-2*sd(us_reg_2$residuals), col='blue')


us_reg_3 <- lm(data=db_US, tr_spread)
summary(us_reg_3)
params_3 <- repara(us_reg_3)
cat('\n Parameters for standard TR with 10y Treasuries vs BAA bonds:\n')
print(params_3)

ts.plot(us_reg_3$residuals)
abline(h=0, col='red')
abline(h=2*sd(us_reg_3$residuals), col='blue')
abline(h=-2*sd(us_reg_3$residuals), col='blue')


us_reg_4 <- lm(data=db_US, tr_treasury)
summary(us_reg_4)
params_4 <- repara(us_reg_4)
cat('\n Parameters for standard TR with 3 months Tbill return rate vs S&P500 return spread:\n')
print(params_4)

ts.plot(us_reg_4$residuals)
abline(h=0, col='red')
abline(h=2*sd(us_reg_4$residuals), col='blue')
abline(h=-2*sd(us_reg_4$residuals), col='blue')


us_reg_5 <- lm(data=db_US, tr_layspread)
summary(us_reg_5)
params_5 <- repara(us_reg_5)
cat('\n Parameters for TR with layoffs and 3 months Tbill return rate vs S&P500 return spread :\n')
print(params_5)

ts.plot(us_reg_5$residuals)
abline(h=0, col='red')
abline(h=2*sd(us_reg_5$residuals), col='blue')
abline(h=-2*sd(us_reg_5$residuals), col='blue')



us_reg_6 <- lm(data=db_US, tr_laybaa)
summary(us_reg_6)
params_6 <- repara(us_reg_6)
cat('\n Parameters for TR with layoffs and  10y Treasuries vs BAA bonds:\n')
print(params_6)

ts.plot(us_reg_6$residuals)
abline(h=0, col='red')
abline(h=2*sd(us_reg_6$residuals), col='blue')
abline(h=-2*sd(us_reg_6$residuals), col='blue')



us_reg_7 <- lm(data=db_US, tr_spf_mean)
summary(us_reg_7)
params_7




cat('\n\n\n\n us_reg_1 is standard Taylor Rule regression\n',
    'us_reg_2 replaces realtime output gap with layoff rate\n',
    'us_reg_3 includes a standard TR with 10y Treasuries vs BAA bonds\n',
    'us_reg_4 replaces previous spread with 3 months Tbill return rate vs S&P500 return spread\n',
    'us_reg_5 is us_reg_4 with layoffs',
    'us_reg_6 is us_reg_3 with layoffs')




##### VAR #####
# Model: y_t = A_i y_{t-1} + \eps_t

variables <- merge(db_US$realtime_gap,          # realtime output gap
                   db_US$ffr,                   # federal fund rate
                   db_US$deflt1,                # one period ahead GDP deflator forecast
                   db_US$deflt,                 # current GDP deflator nowcast
                   db_US$spread_sp_3m)          # SP500 v 3MTBill spread

VAR_spread <- VAR(varia['1967-01/2011-12'],     # period subsample, to exclude NAs
                  lag.max=8,                    # max lags to select
                  type='const',                 # VAR includes a vector for intercepts
                  ic='HQ')                      # selection criterion for optimal lags; HQ SC


summary(VAR_spread)                             # prints estimates


IRF_VAR_spread <- irf(VAR_spread)               # prints IRFs for VAR with spread

#### SVAR ####
# Model: Ay_t = A_i y_{t-1} + \eps_t

# Declare A matrix 
SVAR_Amat <- matrix(c(NA,NA,NA,0,0,
                      NA,NA,NA,0,NA,
                      0,NA,NA,0,0,
                      NA,0,NA,NA,0,
                      NA,NA,0,0,NA), 
                    nrow=5, ncol=5, byrow=T)

SVAR_Amat_alt <- matrix(c(NA,NA,NA,0,0,
                          NA,NA,NA,0,NA,
                          0,NA,NA,0,0,
                          NA,0,NA,NA,0,
                          0,0,NA,0,NA), 
                        nrow=5, ncol=5, byrow=T) # alternative A matrix specification

SVAR_spread <- SVAR(VAR_spread,
                    Amat=SVAR_Amat,
                    Bmat=NULL,
                    estmethod='direct', hessian=T, method='CG')

IRF_SVAR_spread <- irf(SVAR_spread)

plot(IRF_SVAR_spread)


# housekeeping
# rm()