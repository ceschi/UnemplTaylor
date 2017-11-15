##### Code for Inflation analysis #####
##### On revised time series #####


##### Packages #####

if (flag___singular == 1){
  cat('Single file execution')
  source('directories.R')
  source('functs.R')
  source('USdatacoll.R')
}

##### Subset columns to have inflations ####

pi <- merge(db_US$cpit,
            db_US$coret,
            db_US$deflt,
            db_US$rev_cpi,
            db_US$rev_cpi_fe,
            db_US$rev_defl,
            db_US$rev_pce,
            db_US$rev_pce_fe)

# looping upper bound
n=length(names(pi))

# exogenous lag
k=5

# selector for coefficient
# MUST be <k
r=1

# results collecter
inflation <- list(
  names=list('CPI nowcast',
             'Core nowcast',
             'GDP deflator nowcast',
             'Revised CPI',
             'Revised CPI, no FE',
             'Revised GDP deflator',
             'Revised PCE',
             'Revised PCE, no FE'),
  unitroot=list(),
  ark=list(),
  aropti=list(),
  rollm=list(),
  plot_rollm=list()
)

##### Unit root tests #####

for (i in 1:n){
  inflation[['unitroot']][[i]] <- ur.df(na.omit(pi[,i]),
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
  inflation[['ark']][[i]] <- lm(data= (pi[,i] %>% lagger(lag=k)),
                                formula=formula.maker(df=pi[,i] %>% lagger(lag=k),
                                                      y= pi[,i] %>% lagger(lag=k) %>% 
                                                        names(.) %>% first())
                                ) %>% summary() %>% coef()
  print(inflation[['ark']][[i]])
}

##### AR regression with optimal lags #####

for (i in 1:n){
  cat('\nget to know how to extract the optimal lag: in ur.df class there is a section @lags')
}


##### AR(k) rolling window regressions #####
## consider using map() or apply() function families
## to vectorise code and make it faster. This requires
## wrapping loop's actions in an ad-hoc function

for (i in 1:n){

  inflation[['rollm']][[i]] <- rolloop(df = pi[,i], window = 58, lags = k)

}



##### Plots registration #####

for (i in 1:n){
  # attaches data, select first column of estimates and time
  inflation[['plot_rollm']][[i]] <- ggplot(data=inflation[['rollm']][[i]],
                                           aes(x=index(inflation[['rollm']][[i]]),
                                               y=inflation[['rollm']][[i]][,r]))+
    # plot the above with line geom, in black
    geom_line(colour='black', size=1)+
    # adds upper confidence band in red
    geom_line(aes(y=inflation[['rollm']][[i]][,r]+inflation[['rollm']][[i]][,k+r]),
              colour='red')+
    # adds lower confidence band in red
    geom_line(aes(y=inflation[['rollm']][[i]][,r]-inflation[['rollm']][[i]][,k+r]),
                  colour='red')+
    # adds unit root line
    geom_line(aes(y=1), colour='black', size=.8)+
    # plot makeup
    geom_smooth(method='loess', colour='blue')+scale_x_yearqtr(format='%Y Q%q', n=20)+theme_bw()+
    scale_y_continuous()+xlab(' ') + ylab(paste0('AR(',r,') coeff. estimates')) + ggtitle(inflation$names[[i]])
  
  if  (flag___plot==0) plot(inflation[['plot_rollm']][[i]])
  
  # saves graphs in proper directory with names
  ggsave(paste0('AR(',r,') coeff. estimates ', inflation[['names']][[i]], '.pdf'),
         inflation[['plot_rollm']][[i]],
         device='pdf',
         graphs_dir,
         height=8, width=14.16, units='in')
}












##### Housekeeping ####
rm(pi, n, i, r, k)






