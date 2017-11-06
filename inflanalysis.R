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
  arlm=list()
)

##### Unit root tests #####
options(warn=-1) # suppress warnings
for (i in 1:n){
  inflation[['unitroot']][[i]] <- ur.df(na.omit(pi[,i]),
                                         # DF test, max lag to consider
                                         lags=8,
                                         # lags selection criterion, min BIC
                                         selectlags='BIC')
  
  if (inflation[['unitroot']][[i]]@teststat>
      inflation[['unitroot']][[i]]@cval[3]) {
              cat(paste0('For ', names(pi)[i], ' it is not possible to reject \nthe null hypothesis of unit root.'))
  }else if ((inflation[['unitroot']][[i]]@teststat<
            inflation[['unitroot']][[i]]@cval[3]) &
            (inflation[['unitroot']][[i]]@teststat>
             inflation[['unitroot']][[i]]@cval[2])) {
              cat(paste0('For ', names(pi)[i], ' it is possible to reject \nthe null hypothesis of unit root at 90%'))
  }else if((inflation[['unitroot']][[i]]@teststat<
            inflation[['unitroot']][[i]]@cval[2]) &
           (inflation[['unitroot']][[i]]@teststat>
            inflation[['unitroot']][[i]]@cval[1])) {
              cat(paste0('For ', names(pi)[i], ' it is possible to reject \nthe null hypothesis of unit root at 95%'))
  }else if(inflation[['unitroot']][[i]]@teststat<
           inflation[['unitroot']][[i]]@cval[1]) {
              cat(paste0('For ', names(pi)[i], ' it is possible to reject \nthe null hypothesis of unit root at 99%'))
  }
}
options(warn=0) # reactivates warnings





















##### Housekeeping ####
rm(pi)