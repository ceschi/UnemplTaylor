##### function #####


spf_funct = function('filnam', 'typs', ahead=1){
	# this function imports the files, reformats,
	# renames, saves in raw format and produces
	# aggregate statistics in XTS format


	# typ is one of CPI, CORECPI, PCE, COREPCE
	

	# 'ahead' allows to select the horizon of 
	# forecasts one wishes to extract:
	# -1 for previous quarter estimates
    # 0 for nowcast
	# 1 for one quarter ahead -- default
	# 2 for two quarters ahead
	# 3 for three quarters ahead
	# 4 for one year ahead

	typ=tolower(typs)

	colu=c(rep('numeric',3),  # picks year, quarter, ID
		rep('skip', 2+ahead),	 # skips industry
		'numeric',				 # moving target picking 'ahead' horizon
		rep('skip', 7-ahead)	 # skips the rest
		)

	df=read_excel(file.path(working_directory,temp_dir,filnam), 
                      na='#N/A', col_types=colu) %>%
					spread(ID, paste0(typ,ahead+2)) %>% 
					ts(start=c(1968, 4), frequency=4) %>%
					as.xts()

	pst=paste0(typ,'_')
	if (ahead==-1){
		pst=paste0(pst,'b1')
		} 	else {
		pst=paste0(pst,'h') %>% paste0(ahead)
		}

	names(df)=c('year', 'quarter', paste(pst, (1:(ncol(df)-2)), sep='_'))

	df$year <- df$quarter <- NULL

	# saving in txt csv format the raw data
	write.table(df, file.path(getwd(), data_dir, paste(pst,'txt', sep='.'), sep=';', row.names=F)


	iqr <- apply(df, 1, IQR, na.rm=TRUE)
	stand<-apply(df, 1, var, na.rm=T) %>% sqrt()
	mean<-apply(df, 1, mean, na.rm=T)

	df_stat=merge(iqr, stand, mean)
	names(df_stat)=paste(typ, c('iqr', 'sd', 'mean'), sep='_')


	return(df_stat)
}