# This script is used to pull all the log records for trips within an AOI being discussion with the folks in oceans.

# Set the working directory and bring in functions/data as necessary.
direct <- "Y:/Offshore/Assessment/"
#direct_fns <- "C:/Users/keyserf/Documents/Github/FK/Assessment_fns/"
direct_fns <- "C:/Documents/Assessment_fns/"
library(PBSmapping)
library(lubridate)
require(tidyverse)
require(sf)
source(paste(direct_fns,"Maps/ScallopMap.r",sep=""))
source(paste(direct_fns,"Fishery/logs_and_fishery_data.r",sep="")) #logs_and_fish is function call
source(paste(direct_fns,"Maps/pectinid_projector_sf.R",sep="")) #logs_and_fish is function call
source(paste(direct_fns,"Maps/combo_shp.R",sep="")) #logs_and_fish is function call
#fundian.aoi <- read.csv(paste0(direct,"2019/Supporting_tasks/Fundian_AOI/fundian_georgebasin_coords.csv"))

# 2021 coords revised from Gary Pardy (bigger area)
fundian.aoi <- st_as_sf(data.frame(ID = 1:5, 
                                   lon  = c(-68.1411, -68.3348, -64.3653, -64.3113, -68.1411),
                                   lat = c(40.87354, 43.22435, 43.34079, 40.98401, 40.87354)), 
                        coords=c("lon", "lat"),
                        crs=4326)

fundian.aoi <- st_as_sf(st_cast(st_combine(fundian.aoi), "POLYGON")) %>% st_transform(32620)
plot(fundian.aoi)

# this crosses the EEZ. Crop off EEZ. 
temp <- tempfile()
# Download this to the temp directory you created above
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/EEZ/EEZ.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
# Now read in the shapefile
eez.all <- st_read(paste0(temp2, "/EEZ.shp"), quiet=T) %>%
  st_transform(4326)
eez <- st_polygonize(st_combine(eez.all))
eez <- st_transform(eez,32620)
eez <- st_crop(eez, fundian.aoi)
ggplot() + geom_sf(data=fundian.aoi) + geom_sf(data=eez)
plot(eez)

#need to exclude points from 29. 
temp <- tempfile()
# Download this to the temp directory you created above
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore/offshore.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
# Now read in the shapefile
offshore.spa <- combo.shp(temp2,make.sf=T, quiet=T)
# Now transform all the layers in the object to the correct coordinate system, need to loop through each layer
# Because of issues with the polygons immediately needed to turn it into a multilinestring to avoid bad polygons, works a charm after that...
if(any(st_is_empty(offshore.spa))) message(paste0("removed ", offshore.spa[st_is_empty(offshore.spa),]$ID, " because they were empty"))
offshore.spa <- offshore.spa[!st_is_empty(offshore.spa),]
offshore.spa  <- st_transform(offshore.spa,4326)
offshore.spa <- st_make_valid(offshore.spa)
sf_use_s2(FALSE)
offshore.spa <- st_union(offshore.spa[!offshore.spa$ID %in% c("NL", "SFA10", "SFA11", "SFA12"),])
offshore.spa <- st_transform(offshore.spa,32620)
offshore.spa <- st_crop(offshore.spa, fundian.aoi) 

ggplot() + geom_sf(data=offshore.spa) +
  geom_sf(data=fundian.aoi, fill=NA)

# decided to just append 2019 onto previous years
years <- 1980:2019
# Get the fishery data
logs_and_fish(loc="offshore",year = years, direct=direct, direct_fns=direct_fns)#,un=un.ID,pw=pwd.ID,db.con=db.con) Pulling locally. 
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
# Get fishery locations of PBSmapping
fish.locs <- data.frame(EID=1:nrow(fish.dat),X=fish.dat$lon,Y=fish.dat$lat)
fish.locs <- na.omit(fish.locs)
fish.locs <- fish.locs[!(fish.locs$X==0 | fish.locs$Y==0),]
fish.locs <- fish.locs %>%
  st_as_sf(coords = c("X", "Y"),
           crs = 4326) %>%
  st_transform(32620)


ggplot() + geom_sf(data=fundian.aoi) + 
  geom_sf(data=eez) +
  geom_sf(data=offshore.spa)
  #geom_sf(data=fish.locs)

# Now find all the data within these polygons
dim(fish.locs) #240652
fish.locs <- fish.locs[fundian.aoi,] #210801
dim(fish.locs)
fish.locs <- fish.locs[eez,] #207536
dim(fish.locs)
fish.locs <- fish.locs[offshore.spa,] # 207523
dim(fish.locs)

ggplot() + geom_sf(data=fundian.aoi) + 
  geom_sf(data=eez) +
  geom_sf(data=offshore.spa) + 
  geom_sf(data=fish.locs)

# And pull the data
inside.dat <- fish.dat[1:nrow(fish.dat) %in% fish.locs$EID,c("lon","lat","pro.repwt","year","depth.m","depth","vesid","vrnum","kg.hm","hm",'date')]
inside.dat$month <- month(inside.dat$date)
# How many vessels on the bank each year...
aggregate(pro.repwt ~ year + vrnum,inside.dat,length)
aggregate(pro.repwt ~ month+year,inside.dat,function(x) sum(x,na.rm=T))

# check out the old vesid column (this only shows old data)
table(aggregate(pro.repwt ~ year + vesid,inside.dat,length)$year)

# need the vessel info since Dave provided it last year
# In 2021, they asked for 1980-2019 for this new bbox
inside.dat <- inside.dat %>%
  dplyr::select(year, lon, lat, pro.repwt, kg.hm, hm, depth.m, vrnum)

fleet <- read.csv(paste0(direct, "Data/Offshore_Fleet.csv"))
fleet <- fleet %>% rename(vrnum = ID)

inside.dat <- left_join(inside.dat, fleet) %>%
  dplyr::select(year, lon, lat, pro.repwt, kg.hm, hm, depth.m, vrnum, Vessel, Company)

rm(eez.all)
rm(eez)
rm(offshore.spa)

offshore_map <- pecjector(area="offshore", add_layer=list(land="world",
                                                     sfa="offshore"))

map <- offshore_map + geom_sf(data=st_transform(fundian.aoi, 4326), fill=NA, colour="red", lty="dashed") + 
  geom_point(data=inside.dat, aes(lon, lat)) +
  ggtitle("1980-2019")

png(file=paste0(direct,"2021/Supporting_tasks/Fundian_AOI/fishing_locations_1980-2019.png"), height=8.5, width=11, units="in", res=400)
map
dev.off()


require(patchwork)
maps <- NULL
yr5 <- seq(1980, 2019, 5)
for(i in 1:length(yr5)){
  map1 <- offshore_map + geom_sf(data=fundian.aoi, fill=NA, colour="red", lty="dashed") + 
    geom_point(data=inside.dat[inside.dat$year %in% yr5[i]:(yr5[i]+4),], aes(lon, lat)) +
    ggtitle(paste0(yr5[i], "-", (yr5[i]+4)))
  if(i==1) maps <- map1
  if(i>1) maps <- maps + map1
}

png(file=paste0(direct,"2021/Supporting_tasks/Fundian_AOI/fishing_locations_1980-2019_5y.png"), height=8.5, width=11, units="in", res=400)
maps
dev.off()


write.csv(inside.dat,paste0(direct,"2021/Supporting_tasks/Fundian_AOI/FGB_fishery_locations_with_vessel_names_1980-2019_broaderpoly.csv"))

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
           