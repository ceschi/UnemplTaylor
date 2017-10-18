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
  stab=list(
    cusum=list(),
    cusumplot=list(),
    fstat=list(),
    fstatpoints=list(),
    fstatplot=list()
  ),
  plot=list()
)

# Formulas for regressions, appended to first sublist
regressions$formula <- list(
  # 1
    tr_standard =  ffr ~ deflt + realtime_gap + ffrb,
  # 2
    tr_layoff = ffr  ~ deflt + layoffs + ffrb,
  # 3
    tr_spread = ffr ~ deflt + realtime_gap + ffrb + spread_baa,
  # 4
    tr_treasury = ffr ~ deflt + realtime_gap + ffrb + spread_sp_3m,
  # 5
    tr_layspread = ffr ~ deflt + layoffs + ffrb + spread_sp_3m,
  # 6
    tr_laybaa = ffr ~ deflt + layoffs + ffrb + spread_baa,
  # 7
    tr_spf_mean = ffr ~ spf_cpi_h1_mean + realtime_gap + ffrb,
  # 8
    tr_spf_uncert = ffr ~ deflt + realtime_gap + ffrb + spf_cpi_h1_iqr
    )

# Strings to indentify models 
regressions$messages <- list(
  # 1
  'Standard TR',
  # 2
  'TR with layoffs replacing output gap',
  # 3 
  'TR and BAA spread',
  # 4
  'TR and 3M spread',
  # 5
  'TR with layoffs and 3M spread',
  # 6
  'TR with layoffs and BAA spread',
  # 7
  'TR with SPF mean expected inflation',
  # 8
  'TR augmented with IQR SPF'
)

### Looping over different specifications

for (m in 1:length(regressions$formula)){
  
  # fit a linear model
  regressions$models[[m]] <- lm(data=db_US, regressions$formula[[m]])
  
  # rescale parameters
  regressions$params[[m]] <- repara(regressions$models[[m]])
  
  # graphing residuals w/ ggplot2
  regressions$plot[[m]] <- ggplot(data=data.frame(date=residuals(regressions$models[[m]]) %>%
                                                    names() %>% as.yearqtr('%Y Q%q'),
                                                  res=residuals(regressions$models[[m]])),
                                  aes(x=date, y=res)) + 
    geom_line()+theme_bw()+scale_x_yearqtr(format='%Y Q%q', n=20)+
    geom_hline(color='red', yintercept=regressions$models[[m]] %>% residuals() %>% sd() %>% `*`(2))+
    geom_hline(color='red', yintercept=regressions$models[[m]] %>% residuals() %>% sd() %>% `*`(-2))+
    xlab(' ') + ylab('Residuals') + ggtitle(regressions$messages[[m]])
  
  ggsave(paste0(regressions$messages[[m]],'.pdf'),
    regressions$plot[[m]], 'pdf', 
    file.path(working_directory, graphs_dir),
    height=8, width=14.16, units='in')
  
  # stability checks on OLS
  # CUSUM
  regressions$stab$cusum[[m]] <- efp(formula=regressions$formula[[m]], data=as.data.frame(db_US), type='OLS-CUSUM')
  regressions$stab$cusumplot[[m]] <- recordPlot(regressions$stab$cusum[[m]], alpha=.01, boundary=T)
  
  # FStat
  regressions$stab$fstat[[m]] <- Fstats(regressions$formula[[m]], data=db_US)
  regressions$stab$fstatpoints[[m]] <- 
  regressions$stab$fstatplot[[]] <- 
  
}


<<<<<<< HEAD
#### Rolling window regression ####


#### Markov Switching models with K states ####
=======
# code for rolling window OLS regressions on time series 
>>>>>>> origin/master

# require(zoo)
# rollapply(zoo(database),
#          width=262,    # length of the window
#          FUN = function(Z) # FUN to apply rolling
#          { 
#             t = lm(formula=y~x, data = as.data.frame(Z), na.rm=T); 
#             return(t$coef) 
#          },
#          by.column=FALSE, align="right")



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

#### Rolling window regression ####


#### Markov Switching models with K states ####

# code for rolling window OLS regressions on time series 


# require(zoo)
# rollapply(zoo(database),
#          width=262,    # length of the window
#          FUN = function(Z) # FUN to apply rolling
#          { 
#             t = lm(formula=y~x, data = as.data.frame(Z), na.rm=T); 
#             return(t$coef) 
#          },
#          by.column=FALSE, align="right")




# housekeeping
# rm()
