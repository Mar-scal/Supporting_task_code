# This script is used to pull all the log records for trips within an AOI being discussion with the folks in oceans.

# Set the working directory and bring in functions/data as necessary.
direct <- "V:/Offshore/Assessment/"
direct_fns <- "C:/Users/keyserf/Documents/Github/FK/Assessment_fns/"
library(PBSmapping)
library(lubridate)
require(tidyverse)
source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))
source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep="")) #logs_and_fish is function call
fundian.aoi <- read.csv(paste0(direct,"2019/Supporting_tasks/Fundian_AOI/fundian_georgebasin_coords.csv"))
# decided to just append 2019 onto previous years
years <- 1980:2019
# Get the fishery data
logs_and_fish(loc="offshore",year = years, direct=direct, direct_fns=direct_fns)#,un=un.ID,pw=pwd.ID,db.con=db.con) Pulling locally. 
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
# Get fishery locations of PBSmapping
fish.locs <- data.frame(EID=1:nrow(fish.dat),X=fish.dat$lon,Y=fish.dat$lat)
fish.locs <- na.omit(fish.locs)
# Now find all the data within these polygons
key <-findPolys(fish.locs, fundian.aoi)
# And pull the data
inside.dat <- fish.dat[1:nrow(fish.dat) %in% key$EID,c("lon","lat","pro.repwt","year","depth.m","depth","vesid","vrnum","kg.hm","hm",'date')]
inside.dat$month <- month(inside.dat$date)
# How many vessels on the bank each year...
aggregate(pro.repwt ~ year + vrnum,inside.dat,length)
aggregate(pro.repwt ~ month+year,inside.dat,function(x) sum(x,na.rm=T))

# check out the old vesid column (this only shows old data)
table(aggregate(pro.repwt ~ year + vesid,inside.dat,length)$year)

# need the vessel info since Dave provided it last year
# just going to update 2019 and append to last year's version
inside.dat <- subset(inside.dat, year==2019) %>%
  select(year, lon, lat, pro.repwt, kg.hm, hm, depth.m, vrnum)

fleet <- read.csv(paste0(direct, "Data/Offshore_Fleet.csv"))
fleet <- fleet %>% rename(vrnum = ID)

inside.dat <- left_join(inside.dat, fleet) %>%
  select(year, lon, lat, pro.repwt, kg.hm, hm, depth.m, vrnum, Vessel, Company)

write.csv(inside.dat,paste0(direct,"2020/Supporting_tasks/Fundian_AOI/FGB_fishery_locations_with_vessel_names_2019.csv"))

# then append to last year in excel

table(inside.dat$year)
# here is all the data...
windows(11,11)
ScallopMap(direct_fns=direct_fns,direct=direct,xlim=c(-68,-65),ylim=c(41.5,42.8),plot.bathy = T,plot.boundries = T,title = "All Years",cex.mn = 2)
addPolys(fundian.aoi,border="blue",lwd=2)
points(inside.dat$lon,inside.dat$lat,pch=19,cex=0.5)
text(-67.75,41.6,paste("n =",length(inside.dat$lon)),cex=1.5)
text(-67.2,41.6,paste("catch =",round(sum(inside.dat$pro.repwt/1000,na.rm = T),digits=0)),cex=1.5)


windows(11,11)
ScallopMap(direct_fns=direct_fns,direct=direct,xlim=c(-68,-65),ylim=c(41.5,42.8),plot.bathy = T,plot.boundries = T,title = "1980-1989",cex.mn = 2)
addPolys(fundian.aoi,border="blue",lwd=2)
points(inside.dat$lon[inside.dat$year %in% 1980:1989],inside.dat$lat[inside.dat$year %in% 1980:1989],pch=19,cex=0.5)
text(-67.75,41.6,paste("n =",length(inside.dat$lon[inside.dat$year %in% 1980:1989])),cex=1.5)
text(-67.2,41.6,paste("catch =",round(sum(inside.dat$pro.repwt[inside.dat$year %in% 1980:1989]/1000,na.rm = T),digits=0)),cex=1.5)

windows(11,11)
ScallopMap(direct_fns=direct_fns,direct=direct,xlim=c(-68,-65),ylim=c(41.5,42.8),plot.bathy = T,plot.boundries = T,title = "1990-1999",cex.mn = 2)
addPolys(fundian.aoi,border="blue",lwd=2)
points(inside.dat$lon[inside.dat$year %in% 1990:1999],inside.dat$lat[inside.dat$year %in% 1990:1999],pch=19,cex=0.5)
text(-67.75,41.6,paste("n =",length(inside.dat$lon[inside.dat$year %in% 1990:1999])),cex=1.5)
text(-67.2,41.6,paste("catch =",round(sum(inside.dat$pro.repwt[inside.dat$year %in% 1990:1999]/1000,na.rm = T),digits=0)),cex=1.5)

windows(11,11)
ScallopMap(xlim=c(-68,-65),ylim=c(41.5,42.8),plot.bathy = T,plot.boundries = T,title = "2000-2009",cex.mn = 2)
addPolys(fundian.aoi,border="blue",lwd=2)
points(inside.dat$lon[inside.dat$year %in% 2000:2009],inside.dat$lat[inside.dat$year %in% 2000:2009],pch=19,cex=0.5)
text(-67.75,41.6,paste("n =",length(inside.dat$lon[inside.dat$year %in% 2000:2009])),cex=1.5)
text(-67.2,41.6,paste("catch =",round(sum(inside.dat$pro.repwt[inside.dat$year %in% 2000:2009]/1000,na.rm = T),digits=0)),cex=1.5)

windows(11,11)
ScallopMap(xlim=c(-68,-65),ylim=c(41.5,42.8),plot.bathy = T,plot.boundries = T,title = "2010-2017",cex.mn = 2)
addPolys(fundian.aoi,border="blue",lwd=2)
points(inside.dat$lon[inside.dat$year %in% 2010:2019],inside.dat$lat[inside.dat$year %in% 2010:2017],pch=19,cex=0.5)
text(-67.75,41.6,paste("n =",length(inside.dat$lon[inside.dat$year %in% 2010:2017])),cex=1.5)
text(-67.2,41.6,paste("catch =",round(sum(inside.dat$pro.repwt[inside.dat$year %in% 2010:2017]/1000,na.rm = T),digits=0)),cex=1.5)

# Run this for each year and produce the figure
pdf(file=paste(direct,"2020/Supporting_tasks/Fundian_AOI/Fundian_fishery_by_year.pdf",sep=""),onefile=T)
for(i in 1:length(years))
{
  ScallopMap(direct_fns=direct_fns,direct=direct,xlim=c(-68,-65),ylim=c(41.5,42.8),plot.bathy = T,plot.boundries = T,title = paste0("Fishery data (",years[i],")"),cex.mn = 2)
  addPolys(fundian.aoi,border="blue",lwd=2)
  points(inside.dat$lon[inside.dat$year == years[i]],inside.dat$lat[inside.dat$year== years[i]],pch=19,cex=0.5)
  text(-67.75,41.6,paste("n =",length(inside.dat$lon[inside.dat$year == years[i]])),cex=1.5)
  text(-67.2,41.6,paste("catch =",round(sum(inside.dat$pro.repwt[inside.dat$year == years[i]]/1000,na.rm = T),digits=0)),cex=1.5)
  
}
dev.off()
           