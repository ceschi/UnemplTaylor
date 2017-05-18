##### US regression #####


#### Functions ####
repara <- function(x, rho=4){
  params <- coef(summary(x))[,1:2]/(1-coef(x)[rho])
  params[rho,] <- coef(summary(x))[rho, 1:2]
  return(params)
}


# consider putting all elements in lists of homogeneous 
# elements and then looping over these ones

# Formulas for regressions
tr_standard <-  ffr ~ deflt + realtime_gap + ffrb
tr_layoff <- ffr  ~ deflt + layoffs + ffrb
tr_spread <- ffr ~ deflt + realtime_gap + ffrb + spread_baa
tr_treasury <- ffr ~ deflt + realtime_gap + ffrb + spread_sp_3m
tr_layspread <- ffr ~ deflt + layoffs + ffrb + spread_sp_3m
tr_laybaa <- ffr ~ deflt + layoffs + ffrb + spread_baa



us_reg_1 <- lm(data=db_US, tr_standard)
summary(us_reg_1)
params_1 <- repara(us_reg_1)
cat('\n Parameters for Taylor Rule regression:\n')
print(params_1)

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

cat('\n\n\n\n us_reg_1 is standard Taylor Rule regression\n',
    'us_reg_2 replaces realtime output gap with layoff rate\n',
    'us_reg_3 includes a standard TR with 10y Treasuries vs BAA bonds\n',
    'us_reg_4 replaces previous spread with 3 months Tbill return rate vs S&P500 return spread\n',
    'us_reg_5 is us_reg_4 with layoffs',
    'us_reg_6 is us_reg_3 with layoffs')



# housekeeping
rm()