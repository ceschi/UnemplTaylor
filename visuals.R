############## Visualization for TR study ###############

library(ggplot2)


## shorthand for the saving path
pat <- file.path(working_directory, graphs_dir)

trvars_plot <- ggplot(db_US, aes(x=time(db_US)))+
  geom_line(aes(y=ffr, color='FFR'))+
  geom_line(aes(y=deflt1, color='Exp. Infl.'))+
  geom_line(aes(y=realtime_gap, color='Gap'))+
  theme_bw()+xlab(' ')+ylab(' ')+labs(colour=' ')+
  ggtitle('US Taylor rule - main components')
  
print(trvars_plot)

ggsave(filename='trvars.png', path = pat, width=900/4, height = 448/4, units = 'mm')


#### PLOTS TO BE REPRODUCED AND STORED ######

#### Comparing measures of uncertainty

# par(mfrow=c(3,1),mar=rep(2, 4))
# attach(ref2)
# plot(na.omit(EPUdisp), type='l', col='red')
# plot(na.omit(spfvar), type='l',col='blue')
# plot(na.omit(spfSD), type='l', col='black')

# plot(spfSD, type='l', col='red', main='spfSD')
# plot(cpinow, type='l', col='blue', main='cpinow')
# plot(cpipost, type='l', col='black', main='cpipost')

# par(mfrow=c(4,1),mar=rep(2, 4))
# plot(spfSD, type='l', col='red', main='spfSD')
# plot(spfrange, type='l', col='blue', main='spfrange')
# plot(spfmedian, type='l', col='black', main='spfmedian')
# plot(spfmean, type='l', col='green', main='spfmean')

# plot(EPUdisp, type='l', col='red', main='EPUdisp')
# plot(spfrange, type='l', col='blue', main='spfrange')
# plot(spfmedian, type='l', col='black', main='spfmedian')
# plot(spfmean, type='l', col='green', main='spfmean')

# par(mfrow=c(2,1),mar=rep(2, 4))
# plot(spfrange, type='l', col='red', main='spfrange')
# plot(EPUdisp, type='l', col='blue', main='EPU')
# par(mfrow=c(1,1))
# detach(ref2)

# dev.off() # resets the device for graphs

# # Full specification of traditional TR
# plot_tayrule<-ggplot()+geom_line(data=ref2, aes(x=time, y=ffr_act, color='FFR'))+
# 	geom_line(data=ref2, aes(x=time, y=cpinow, color='CPI now'))+
# 	geom_line(data=ref2, aes(x=time, y=cpipost, color='CPI post'))+
# 	geom_line(data=ref2, aes(x=time, y=outputpost, color='Y post'))+
# 	geom_line(data=ref2, aes(x=time, y=outputgaphand, color='Y gap'))+
# 	geom_line(data=ref2, aes(x=time, y=trendoutput, color='q-trend'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('All variables')+
# 	theme(legend.position='top')
# print(plot_tayrule)

# # Parismonious TR
# plot_tayrule2<-ggplot()+geom_line(data=ref2, aes(x=time, y=ffr_act, color='FFR'))+
# 	geom_line(data=ref2, aes(x=time, y=cpinow, color='CPI now'))+
# 	geom_line(data=ref2, aes(x=time, y=outputgaphand, color='Y gap'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('Parismonious TR spec')+
# 	theme(legend.position='top')
# print(plot_tayrule2)

# # TR with Philly outputgap
# plot_tayrule3<-ggplot()+geom_line(data=ref2, aes(x=time, y=ffr_act, color='FFR'))+
# 	geom_line(data=ref2, aes(x=time, y=cpinow, color='CPI now'))+
# 	geom_line(data=ref2, aes(x=time, y=outputgaphilly, color='Y gap (Philly)'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('TR with official Philly Ygap')+
# 	theme(legend.position='top')
# print(plot_tayrule3)


# # Philly outputgap vs FRED (potential - actual)
# plot_gap1<-ggplot()+geom_line(data=ref2, aes(x=time, y=outputgaphilly, color='Philly'))+
# 	geom_line(data=ref2, aes(x=time, y=outputgaphand, color='Fred'))+
# 	geom_line(data=ref2, aes(x=time, y=trendoutput, color='q-trend'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('Cfr Philly vs crude Fred vs q-trend')+
# 	theme(legend.position='top')
# print(plot_gap1)

# plot_gap2<-ggplot()+geom_line(data=ref2, aes(x=time, y=outputgaphand, color='Fred'))+
#   geom_line(data=ref2, aes(x=time, y=trendoutput, color='q-trend'))+
#   theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('crude Fred vs q-trend')+
#   theme(legend.position='top')
# print(plot_gap2)

# # check the consistency of longer over shorter
# fed<-lm(outputgaphilly~outputgaphand, data=ref2)
# summary(fed)
# q_trend<-lm(data=ref2, outputgaphilly~trendoutput)
# summary(q_trend)
# crafted <- lm(trendoutput~outputgaphand, ref2)
# summary(crafted)
# # outputgaphand has a greater explanatory power with respect to the official series

# rm(crafted, fed, q_trend)

# # CPI discrepancies
# plot_cpis<-ggplot()+geom_line(data=ref2, aes(y=cpinow, x=time, color='now'))+
# 	geom_line(data=ref2, aes(y=cpipost, x=time, color='post'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color='CPIs')+ggtitle('CPI\'s comparison')+
# 	theme(legend.position='top')
# print(plot_cpis)

# regcpi<-lm(cpipost~cpinow, data=ref2)
# summary(regcpi)
# # almost one-to-one relation
# rm(regcpi)

# # TR with spf
# plot_tayspf<-ggplot()+geom_line(data=ref2, aes(x=time, y=ffr_act, color='FFR'))+
# 	geom_line(data=ref2, aes(x=time, y=cpinow, color='CPI now'))+
# 	geom_line(data=ref2, aes(x=time, y=spfrange, color='spfrange'))+
# 	geom_line(data=ref2, aes(x=time, y=spfvar, color='spfvar'))+
# 	geom_line(data=ref2, aes(x=time, y=spfSD, color='spfSD'))+
# 	geom_line(data=ref2, aes(x=time, y=EPUdisp/100, color='EPU'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+ggtitle('TR with SPF measures')+
# 	theme(legend.position='top')
# print(plot_tayspf)

# #SFPVAR alone
# plot_spfvari<-ggplot(data=ref2, aes(x=time, y=spfvar, color='SPF variance'))+geom_line()+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+theme(legend.position='top')
# print(plot_spfvari)

# # comparison of different risk/uncertainty measures

# plot_risk<-ggplot()+geom_line(data=ref2, aes(x=time, y=scale(vix), color='VIX'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(EPUdisp), color='EPU'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfrange), color='spfrange'))+
# 	theme_bw()+ggtitle('Risk & uncertainty')+labs(' ')+ylab(' ')+theme(legend.position='top')
# print(plot_risk)

# plot_fullrisk<-ggplot()+geom_line(data=ref2, aes(x=time, y=scale(vix), color='VIX'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(EPUdisp), color='EPU'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(vixdif), color='vixdif'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfmedian), color='spfmedian'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfmean), color='spfmean'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfSD), color='spfSD'))+
# 	geom_line(data=ref2, aes(x=time, y=scale(spfrange), color='spfrange'))+
# 	theme_bw()+ggtitle('Risk & uncertainty')+labs('leg.')+ylab(' ')+
# 	theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_fullrisk)

# # Measures of inflation at different timings and w/ or w/o crisis
# plot_inflationnow <- ggplot()+
# 	geom_line(data=db, aes(x=index(db), y=cpinow, color='cpinow'))+
# 	geom_line(data=db, aes(x=index(db), y=cpit, color='cpit'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# plot_inflationahead <- ggplot()+
# 	geom_line(data=db, aes(x=index(db), y=cpif, color='cpif'))+
# 	geom_line(data=db, aes(x=index(db), y=cpit1, color='cpit1'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# plot_interinflation <- ggplot()+
# 	geom_line(data=db, aes(x=index(db), y=cpit1, color='cpit1'))+
# 	geom_line(data=db, aes(x=index(db), y=cpit, color='cpit'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# print(plot_inflationnow)
# print(plot_inflationahead)
# print(plot_interinflation)


# plot_corefull <- ggplot()+
# 	geom_line(data=db, aes(x=index(db), y=coret, color='core'))+
# 	geom_line(data=db, aes(x=index(db), y=deflt, color='defl'))+
# 	geom_line(data=db, aes(x=index(db), y=cpit, color='cpi'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# plot_corefullnogfc <- ggplot()+
# 	geom_line(data=nogfc, aes(x=index(nogfc), y=coret1, color='core'))+
# 	geom_line(data=nogfc, aes(x=index(nogfc), y=deflt1, color='defl'))+
# 	geom_line(data=nogfc, aes(x=index(nogfc), y=cpit1, color='cpi'))+
# 	theme_bw()+ylab('')+xlab('Time')+labs(color=' ')+
# 	theme(legend.position='top')

# print(plot_corefull)
# print(plot_corefullnogfc)

# # plots of money aggregates

# plot_money <- ggplot()+geom_line(data=db, aes(y=m1, x=index(db), color='M1'))+
# 	geom_line(data=db, aes(y=m2, x=index(db), color='M2'))+
# 	geom_line(data=db, aes(y=curr, x=index(db), color='Tot. Curr.'))+
# 	geom_line(data=db, aes(y=base, x=index(db), color='Mon. Base'))+
# 	theme_bw()+labs('Monetary aggregates')+ylab('Bls of $')+xlab(' ')+
# 	theme(legend.position='top')
# print(plot_money)

# # plots of fiscal variables
# plot_debtgraph <- ggplot()+geom_line(data=db, aes(x=index(db), y=debt))+
# 	theme_bw()+labs(title='Debt level')+ylab('Bls of $')+xlab(' ')
# print(plot_debtgraph)

# plot_surplusgraph <- ggplot()+geom_line(data=db, aes(x=index(db), y=surplus), color='red')+
# 	theme_bw()+labs(title='Surplus as % of GDP')+ylab(' ')+xlab(' ')+geom_hline( yintercept = 0)
# print(plot_surplusgraph)

# # uncertainty measured as FFR range
# plot_ffr_range <- ggplot()+geom_line(data=db, aes(x=index(db), y=spfrange*3, color='SPF Range'))+
# 	geom_line(data=db, aes(x=index(db), y=ffr_act, color='FFR'))+
# 	theme_bw()+labs(' ')+ylab(' ')+xlab(' ')+
# 	theme(legend.position='top')#+  guides(colour = guide_legend(override.aes = list(size=3))) 
# print(plot_ffr_range)

# # principal rates in game
# plot_rates <- ggplot()+geom_line(data=db, aes(x=index(db), y=deflt1, color='Deflator h=1'))+
# 	geom_line(data=db, aes(x=index(db), y=ffr_act, color='FFR'))+
# 	theme_bw()+ggtitle('Rates')+labs('leg.')+ylab(' ')+xlab(' ')+
# 	theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_rates)

# # plotting data from Michigan survey
# plot_soc_mich_infl <- ggplot()+geom_line(data = db3, aes(x=index(db3), y=deflt4, color='Deflator h=4'))+
#   geom_line(data = db3, aes(x=index(db3), y=soc_e_cpi_1y_ahead, color='SOC inflation expectation')) +
#   geom_line(data = db3, aes(x=index(db3), y=ffr_act, color='FFR')) +
#   theme_bw()+ggtitle('SOC vs SPF')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# plot(plot_soc_mich_infl)

# # levels for SOC sentiment indexes
# plot_soc_lvl <- ggplot()+geom_line(data = db3, aes(x=index(db3), y=actual_soc_ind, color='Actual SOC lvl'))+
#   geom_line(data = db3, aes(x=index(db3), y=expected_soc_ind, color='Expected SOC lvl')) +
#   theme_bw()+ggtitle('SOC Levels')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# plot(plot_soc_lvl)

# # inflation, SOC lvl diff, FFR
# plot_soc_diff <-ggplot()+ geom_line(data = db3, aes(x=index(db3), y=ffr_act, color='FFR'))+
#   geom_line(data = db3, aes(x=index(db3), y=diff_soc_ind, color='SOC lvl difference')) +
#   geom_line(data = db3, aes(x=index(db3), y=deflt1, color='Deflator h=1')) +
#   theme_bw()+ggtitle('Inflation, SOC, FFR')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_soc_diff)

# # inflation, SOC perch, FFR
# plot_soc_perch <-ggplot()+ geom_line(data = db3, aes(x=index(db3), y=ffr_act, color='FFR'))+
#   geom_line(data = db3, aes(x=index(db3), y=perch_actual_soc_ind, color='Actual SOC perc. ch.')) +
#   geom_line(data = db3, aes(x=index(db3), y=perch_expected_soc_ind, color='Expected SOC perc. ch.')) +
#   geom_line(data = db3, aes(x=index(db3), y=deflt1, color='Deflator h=1')) +
#   theme_bw()+ggtitle('Inflation, SOC percentage change, FFR')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_soc_perch)


# plot_shffr <- ggplot()+geom_line(data=db_shadow, aes(x=index(db_shadow), y=ffr_act, color='FFR'))+
#   geom_line(data=db_shadow, aes(x=index(db_shadow), y=shffr, color= 'Wu Xia Shadow rate'))+
#   geom_line(data = db_shadow, aes(x=index(db_shadow), y=deflt1, color='Deflator h=1')) +
#   theme_bw()+ggtitle('Deflator, FFR, Wu Xia Shadow rate')+labs('leg.')+ylab(' ')+xlab(' ')+
#   theme(legend.position='top')+  guides(colour = guide_legend(override.aes = list(size=3)))
# print(plot_shffr)


### housekeeping
# rm(pat, ) 