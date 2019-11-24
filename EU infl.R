library(eurostat)

df <- search_eurostat('HICP')

dat <- get_eurostat(id=df[3,2], select_time='M')
dat2 <- dat %>% select(-unit) %>%  filter(coicop=='CP00')
dat3<- dat2 %>% select(-coicop) %>% group_by(geo) %>% summarise(median=mean(values))

ggplot(dat2, aes(x=time, y=values, colour=geo))+geom_line()+facet_wrap(~geo, nrow=8, ncol=5)

# Code is meant to download CICP indexes for almost all the 
# countries and bodies in Europe. Missing is a conversion to time series
# and BG (bulgary) should be excluded in the visualization because it breaks the scaling

# moreover data are MoM percentage variations, so one needs to aggregate carefully