
years_f <- 2018:2022

df3 <- NULL
df4 <- NULL
for(q in 1:length(years_f)){
  print(q)
  #if(years_f[q]==2017) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_spring_results.Rdata"))
  if(years_f[q]==2018) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results_FINAL.Rdata"))
  if(years_f[q]==2019) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results.Rdata"))
  if(years_f[q]==2020) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results.Rdata"))
  if(years_f[q]==2021) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results.Rdata"))
  if(years_f[q]==2022) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results.Rdata"))
  
  for(b in which(!names(mw.dat.all) %in% names(mw.dat.all)[grep(x=names(mw.dat.all), pattern = "-", fixed=T)])){
    mw.dat.all[[b]]$run <- years_f[q]
    mw.dat.all[[b]]$bank <- names(mw.dat.all)[b]
    mw.dat.all[[b]]$year <- as.numeric(as.character(mw.dat.all[[b]]$year))
    mw.dat.all[[b]]$tow <- as.numeric(as.character(mw.dat.all[[b]]$tow))
    mw.dat.all[[b]] <- na.omit(mw.dat.all[[b]])
    
    cf.data[[b]]$CFyrs$run <- years_f[q]
    cf.data[[b]]$CFyrs$bank <- names(cf.data)[b]
    cf.data[[b]]$CFyrs$year <- as.numeric(as.character(cf.data[[b]]$CFyrs$year))
    
    if(q == 1){
      df3 <- mw.dat.all[[b]]
      df4 <- cf.data[[b]]$CFyrs
    }
    if(q>1){
      df3 <- full_join(df3, mw.dat.all[[b]])
      df4 <- full_join(df4, cf.data[[b]]$CFyrs)
    }
  }
}

df <- NULL
df2 <- NULL
for (i in 1:length(unique(df3$bank))){
  wgt.dat <- df3[df3$bank==unique(df3$bank)[i],]
  for(y in unique(wgt.dat$run)){
    df$bank <- unique(df3$bank)[i]
    df$depth <- mean(subset(wgt.dat[wgt.dat$run==y,], year >=2005 & year <2015)$depth,na.rm=T)
    df$lat <- mean(subset(wgt.dat[wgt.dat$run==y,], year >=2005 & year <2015)$lat,na.rm=T)
    df$lon <- mean(subset(wgt.dat[wgt.dat$run==y,], year >=2005 & year <2015)$lon,na.rm=T)
    df$run <- y
    df <- as.data.frame(df)
    if(is.null(df2)) df2 <- df
    if(!is.null(df2)) df2 <- full_join(df2, df)
  }
}

ggplot() + geom_line(data=df4, aes(year, lon, colour=as.factor(run))) + facet_grid(bank~run, scales="free") +
  xlim(1980, 2022)

ggplot() + geom_point(data=df2, aes(lon, lat, colour=as.factor(run), group=as.factor(run))) + facet_wrap(~bank)
