##### Code for Inflation analysis #####
##### On revised time series #####


##### Packages #####

if (flag___singular == 1){
  cat('Single file execution')
  source('directories.R')
  source('functs.R')
}

##### Subset columns to have inflations ####

pi <- merge(db_US$cpit,
            db_US$coret,
            db_US$deflt,
            db_US$rev_pci,
            db_US$rev_pci_fe,
            db_US$rev_defl,
            db_US$rev_pce,
            db_US$rev_pce_fe)

# looping upper bound
n=length(names(pi))

# results collecter
inflation <- list(
  unitroot=list(),
  ark=list(),
  aropti=list(),
  rollm=lm()
)

##### Unit root tests #####

for (i in 1:n){
  inflation[['unitroot']][[i]] <- urca.df(na.omit(pi[,i]),
                                         # DF test, max lag to consider
                                         lags=8,
                                         # lags selection criterion, min BIC
                                         selectlags='BIC')
  
  if (inflation[['unitroot']][[i]]@teststat>
      inflation[['unitroot']][[i]]@cval[3]) {
              cat(paste0('For ', names(pi)[i], ' it is not possible to reject \nthe null hypothesis of unit root.\n\n'))
  }else if ((inflation[['unitroot']][[i]]@teststat<
            inflation[['unitroot']][[i]]@cval[3]) &
            (inflation[['unitroot']][[i]]@teststat>
             inflation[['unitroot']][[i]]@cval[2])) {
              cat(paste0('For ', names(pi)[i], ' it is possible to reject \nthe null hypothesis of unit root at 90%.\n\n'))
  }else if((inflation[['unitroot']][[i]]@teststat<
            inflation[['unitroot']][[i]]@cval[2]) &
           (inflation[['unitroot']][[i]]@teststat>
            inflation[['unitroot']][[i]]@cval[1])) {
              cat(paste0('For ', names(pi)[i], ' it is possible to reject \nthe null hypothesis of unit root at 95%.\n\n'))
  }else if(inflation[['unitroot']][[i]]@teststat<
           inflation[['unitroot']][[i]]@cval[1]) {
              cat(paste0('For ', names(pi)[i], ' it is possible to reject \nthe null hypothesis of unit root at 99%.\n\n'))
  }
}

##### AR regression with fixed lags k=5 #####

for (i in 1:n){
  inflation[['ark']][[i]] <- lm(data= (pi[,i] %>% lagger(lag=5)),
                                formula=formula.maker(df=pi[,i] %>% lagger(lag=5),
                                                      y= pi[,i] %>% lagger(lag=5) %>% 
                                                        names(.) %>% first())
                                ) %>% summary() %>% coef()
  print(inflation[['ark']][[i]])
}

##### AR regression with optimal lags #####

for (i in 1:n){
  cat('\ndo something here')
}


##### AR(k) rolling window regressions #####

for (i in 1:n){
  matrix <- pi[,i] %>% lagger(lag=5)
  formu <- formula.maker(matrix, matrix %>%  names(.) %>% first())
                           
  regs <-rollapply(matrix,
                   width=8,
                   lm(formula=formu) %>% coef())
    
  rm(regs, formu, matrix)
}


rollm <- function(df, formula){
  # returns an object that has for each 
  # parameter in the estimation the point estimate
  # and the doubled SD, ideally a row with names
  # or on the left point estimates column group
  # and an equally sized column group with 2SD
}














##### Housekeeping ####
rm(pi, n, i)