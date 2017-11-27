##### US regression #####


#### Functions ####
if (flag___singular == 1){
  
  cat('Single file execution')
  source('directories.R')
  source('functs.R')
  source('USdatacoll.R')
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
    fstat=list(),
    fstatpoints=list(),
    fstatcandidates=list()
  ),
  mswm=list(
    fit=list(),
    coefs=list()
  ),
  var=list(
    varfit=list(),
    varirf=list()
  ),
  svar=list(
    svarfit=list(),
    svarirf=list()
  ),
  plot=list()
)

# Formulas for regressions, appended to first sublist
regressions$formula <- list(
  # 1
    tr_standard =  ffr ~ deflt1 + realtime_gap + ffrb,
  # 2
    tr_layoff = ffr  ~ deflt1 + layoffs + ffrb,
  # 3
    tr_spread = ffr ~ deflt1 + realtime_gap + ffrb + spread_baa,
  # 4
    tr_treasury = ffr ~ deflt1 + realtime_gap + ffrb + spread_sp_3m,                  # ADDED 1 TO ALL DEFLT
  # 5
    tr_layspread = ffr ~ deflt1 + layoffs + ffrb + spread_sp_3m,
  # 6
    tr_laybaa = ffr ~ deflt1 + layoffs + ffrb + spread_baa,
  # 7
    tr_spf_mean = ffr ~ spf_cpi_h1_mean + realtime_gap + ffrb,
  # 8
    tr_spf_uncert = ffr ~ deflt1 + realtime_gap + ffrb + spf_cpi_h1_iqr
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
  
  ##### Simple OLS with stability checks #####

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
    regressions$plot[[m]], 
    device='pdf', 
    graphs_dir,
    height=8, width=14.16, units='in')
  
  # stability checks on OLS
  # CUSUM
  regressions$stab$cusum[[m]]<- efp(formula=regressions$formula[[m]], data=as.data.frame(db_US), type='OLS-CUSUM')
  # modifies title
  regressions[["stab"]][["cusum"]][[m]][["type.name"]] <- paste0(regressions$messages[[m]], ': OLS-based CUSUM test')
  
  # FStat - Chow test
  regressions$stab$fstat[[m]] <- Fstats(formula=regressions$formula[[m]], data=as.data.frame(db_US))
  # saves most likely date of structural break
  regressions$stab$fstatpoints[[m]] <- regressions[["models"]][[m]][["model"]][regressions$stab$fstat[[m]]$breakpoint,] %>% 
    row.names()
  # extracts and saves dates of breaks detected
  regressions$stab$fstatcandidates[[m]] <- breakpoints(regressions$formula[[m]],
                                                       data=as.data.frame(db_US))
  
  
  
  #### Markov Switching models with K states ####
  # looping over formulae to estimate j-state
  # Markov Switching model 
  if (flag___msm==1) j <- 2
  if (flag___msm==2) j <- 3
  
  if (flag___msm!=0){
      regressions$mswm$fit[[m]] <- msmFit(object=regressions$models[[m]],
                                          #data=db_US,
                                          k=j,
                                          sw=rep(T, 1+regressions$formula[[m]] %>% all.vars() %>% length())
                                          )
      regressions$mswm$coefs[[m]] <- regressions$mswm$fit[[m]]@Coef/(1-regressions$mswm$fit[[m]]@Coef[,4])
      
  }
  
  ##### VAR #####
  # Model: y_t = A_i y_{t-1} + \eps_t
  
  # slice database
  dat <- db_US %>% as.tibble()  %>% 
    select(regressions$formula[[m]] %>% all.vars(), -ffrb) %>%
    na.omit(.)
  
  # estimate a VAR model with pre-set formulas
  regressions$var$varfit[[m]] <- VAR(y = dat,
                                     lag.max = 16,
                                     type = 'const',
                                     ic = 'HQ')
  # stock irfs for 40 quarters,
  # the impulse is given to
  # the interest rate  --> might consider inclusion of 
  # actual cpi series to check how real inflation reacts
  regressions$var$varirf[[m]] <- irf(regressions$var$varfit[[m]],
                                     impulse='ffr',
                                     n.ahead=20,
                                     runs=250)
  
  ##### Setting up environment for SVAR #####
  
  ## Declaration of struct matrix
  # B mat is identity by default (orthogonal shocks)
  # all off-diagonal elements are suppressed
  # from the estimation <- CAREFUL!
  AA <- matrix(0, ncol=regressions$formula[[m]] %>% all.vars() %>% length() - 1,
               nrow=regressions$formula[[m]] %>% all.vars()%>% length() - 1)

  # diagonal elements are set to NA
  # so to be estimated
  diag(AA) <- NA
  
  # SVAR estimation
  # Model: AAy_t = A_i y_{t-1} + \eps_t
  regressions$svar$svarfit[[m]] <- SVAR(regressions$var$varfit[[m]],
                                Amat=AA,
                                estmethod='direct',
                                hessian=T,
                                method="BFGS") # alternative for method is 'CG'
  
  # SVAR IRFs
  regressions$svar$svarirf[[m]] <- irf(regressions$svar$svarfit[[m]],
                                       impulse='ffr',
                                       n.ahead=20,
                                       runs=250)
  
  
  # housekeeping
  rm(AA, dat)
}



# 
# #### SVAR ####
# # Model: Ay_t = A_i y_{t-1} + \eps_t
# 
# # Declare A matrix 
# SVAR_Amat <- matrix(c(NA,NA,NA,0,0,
#                       NA,NA,NA,0,NA,
#                       0,NA,NA,0,0,
#                       NA,0,NA,NA,0,
#                       NA,NA,0,0,NA), 
#                     nrow=5, ncol=5, byrow=T)
# 
# SVAR_Amat_alt <- matrix(c(NA,NA,NA,0,0,
#                           NA,NA,NA,0,NA,
#                           0,NA,NA,0,0,
#                           NA,0,NA,NA,0,
#                           0,0,NA,0,NA), 
#                         nrow=5, ncol=5, byrow=T) # alternative A matrix specification
# 
# SVAR_spread <- SVAR(VAR_spread,
#                     Amat=SVAR_Amat,
#                     Bmat=NULL,
#                     estmethod='direct', hessian=T, method='CG')
# 
# IRF_SVAR_spread <- irf(SVAR_spread)
# 
# plot(IRF_SVAR_spread)




# housekeeping
# rm()
