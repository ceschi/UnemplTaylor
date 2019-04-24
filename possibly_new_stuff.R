##### Easy regressions #####


##### FULL SAMPLE ##### 

splits <- c('/1979:1', '1982:1/2007:6', '2007:6/')

full_sample <- lapply(regressions$formula, lm, data=db_US[splits[2]])


summa <- lapply(full_sample, summary)

rep <- lapply(full_sample, repara)

##### First sample

subsample <- summa2 <- rep2 <- list()

# subsample with incomplete data
for (i in c(1, 2, 4, 5)){
  subsample[[i]] <- lm(data=db_US["/1979:6"], regressions$formula[[i]])
  summa2[[i]] <-  summary(subsample[[i]])
  rep2[[i]] <- repara(subsample[[i]])
}

## bootstrap with ts is a deeply disturbing idea
## especially since it crashes lags strc
## but check boot::tsboot
bootstrapped <- db_US %>% as.tibble() %>% sample_n(200000, replace = T) %>%
bootlm <- lapply(regressions$formula, lm, data=bootstrapped)
BS_summary <- lapply(bootlm, summary)
BS_rep <- lapply(bootlm, repara)

for (i in 1:8){
  print(BS_summary[[i]]); print(BS_rep[[i]])
}



####### inflation ######


apply(pi,MARGIN = 2, function(x){print(length(x)-sum(is.na(x)))} )

for (i in 1:n){nobs(inflation[['ark']][[i]]) %>% print}

for (i in 1:n){BIC(inflation[['ark']][[i]]) %>% print}

for (i in 1:n){BIC(inflation[['ark']][[i]]) %>% print}

for (i in 1:n){summary(inflation[['unitroot']][[i]]) %>% print()}
