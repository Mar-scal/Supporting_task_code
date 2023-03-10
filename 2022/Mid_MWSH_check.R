load("Y:/Offshore/Assessment/Data/Survey_data/2022/Survey_summary_output/testing_results_spring2022.Rdata")

annual <-NULL
for (y in sort(unique(mw$Mid$year))){
  mw.dm <- na.omit(subset(mw$Mid, year==y))
  
  mw.dm$sh<-mw.dm$sh/100
  # MODEL - This is the meat weight Shell height realationship.  
  #MEAT WEIGHT SHELL HEIGHT RELATIONSHIP in current year 
  #Source5 source("fn/shwt.lme.r") note that the exponent is set as a parameter here b=3
  
  mwshyr <- shwt.lme(mw.dm,random.effect='tow',b.par=3)
  
  mwshyr$fit$year <- y
  
  annual <- rbind(annual, mwshyr$fit)
}

p1 <- ggplot() + geom_boxplot(data=annual[annual$year>2010,], aes(year, a)) + geom_jitter(data=annual[annual$year>2010,], aes(year, a), colour="red") + ylab("MWSH model fit")
ggplot() + geom_boxplot(data=mw$Mid[mw$Mid$year>2010,], aes(y=wmw/sh, x=year))
ggplot() + geom_boxplot(data=mw$Mid[mw$Mid$year>2010,], aes(y=wmw/sh, x=tow, group=tow)) + facet_wrap(~year)
wmw <- ggplot() + geom_boxplot(data=mw$Mid[mw$Mid$year>2010,], aes(y=wmw, x=tow, group=tow)) + facet_wrap(~year, nrow=1)
sh <- ggplot() + geom_boxplot(data=mw$Mid[mw$Mid$year>2010,], aes(y=sh, x=tow, group=tow)) + facet_wrap(~year, nrow=1)
ggplot() + geom_boxplot(data=mw$Mid[mw$Mid$year>2010,], aes(y=depth, x=tow, group=tow)) + facet_wrap(~year)
ggplot() + geom_boxplot(data=mw$Mid[mw$Mid$year>2010,], aes(y=lon, x=tow, group=tow)) + facet_wrap(~year)
ggplot() + geom_boxplot(data=mw$Mid[mw$Mid$year>2010,], aes(y=lat, x=tow, group=tow)) + facet_wrap(~year)

require(patchwork)
png("Y:/Offshore/Assessment/2022/Presentations/Survey_summary/Exploratory_figures/Mid/MWSH_2022.png", height=6, width=12, units="in", res=400)
p1/wmw / sh
dev.off()

mw$Mid[mw$Mid$year==2022,]$ID

# Individual tow lines are hidden underneath the blue line. They are all VERY similar. 
# 2022 mwsh model results ("a") are nearly identical between tows (17.3). Other years are more varied. 
# There is variability between samples, but very high consistency in the measurements between tows