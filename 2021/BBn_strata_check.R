require(rgdal)
require(rgeos)
require(raster)
require(sf)

### Skip this and go to line 190

stratum1 <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Originals/BBN_Strata1.shp")
plot(stratum1[1])
st_area(stratum1)
stratum1b <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Edited/BBN_Strata1_UTM19_new.shp")
plot(stratum1b[1])
st_area(stratum1b)/100000
strata <- st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore_survey_strata/BBn.shp")
plot(strata[1])
st_area(strata[strata$Strt_ID==201,])/100000

stratum2 <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Originals/BBN_Strata2.shp")
plot(stratum2)
stratum2b <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Edited/BBN_Strata2_UTM19_new.shp")
plot(stratum2b[1])
sum(st_area(stratum2b[1])/100000)
plot(strata[1])
st_area(strata[strata$Strt_ID==202,])/100000


stratum3 <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Originals/BBN_Strata3.shp")
plot(stratum3)
stratum3b <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Edited/BBN_Strata3_UTM19_new.shp")
plot(stratum3b[1])
sum(st_area(stratum3b[1])/100000)
plot(strata[1])
st_area(strata[strata$Strt_ID==203,])/100000

stratum4 <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Originals/BBN_Strata4.shp")
plot(stratum4)
stratum4b <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Edited/BBN_Strata4_UTM19_new.shp")
plot(stratum4b[1])
sum(st_area(stratum4b[1])/100000)
plot(strata[1])
st_area(strata[strata$Strt_ID==204,])/100000


stratum5 <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Originals/BBN_Strata5.shp")
plot(stratum5)
stratum5b <- st_read("Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Edited/BBN_Strata5_UTM19.shp")
plot(stratum5b[1])
sum(st_area(stratum5b[1])/100000)
plot(strata[1])
st_area(strata[strata$Strt_ID==205,])/100000


surv.info <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/archive/survey_information_2021-07-09.csv")
surv.info2 <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/survey_information.csv")

surv.info[surv.info$label=="BBn",]$area_km2==surv.info2[surv.info2$label=="BBn",]$area_km2

#plot(st_union(st_union(st_union(st_union(stratum1b, stratum2b), stratum3b), stratum4b), stratum5b))

#from CSV
surv.info[surv.info$label=="BBn",]$area_km2
c(sum(st_area(stratum1b)/1000000),
  sum(st_area(stratum2b)/1000000),
  sum(st_area(stratum3b)/1000000),
  sum(st_area(stratum4b)/1000000),
  sum(st_area(stratum5b)/1000000))


require(ROracle)
source("Other_functions/ScallopQuery.R")
#chan <- odbcConnect(package="db.lib", un=un.ID, pw=pwd.ID, db.con=db.con, db.con,uid=un,pwd=pw,believeNRows=FALSE)
# The query to grab log data
# This is the query to grab the strata for the offshore fishery
quer.off <- paste(
  "SELECT * FROM SCALOFF.OSSTRATA",
  sep=""
)
# Run the query and add data to the log.lst object
strata.db <- ScallopQuery(package="ROracle", un="keyserf", pw="Decade06", db.con="ptran", SQLtext= quer.off)
head(strata.db)
strata.db <- strata.db %>%
  dplyr::filter(grepl(COMMENTS, pattern = "BBn"))

summary(strata.db)

table(strata.db$STRATA_ID)

unique(strata.db[,c("STRATA_ID", "STRATA_DESC", "COMMENTS")])

#surv.info$area_km2*1000*1000/800/(8/3.2808)

round(unique(strata.db[strata.db$STRATA_ID=="201",]$STRATA_AREA)/(1000*1000/800/(8/3.2808)),3) == round(surv.info[surv.info$Strata_ID==201,]$area_km2,3)
round(unique(strata.db[strata.db$STRATA_ID=="202",]$STRATA_AREA)/(1000*1000/800/(8/3.2808)),3) == round(surv.info[surv.info$Strata_ID==202,]$area_km2,3)
round(unique(strata.db[strata.db$STRATA_ID=="203",]$STRATA_AREA)/(1000*1000/800/(8/3.2808)),3) == round(surv.info[surv.info$Strata_ID==203,]$area_km2,3)
round(unique(strata.db[strata.db$STRATA_ID=="204",]$STRATA_AREA)/(1000*1000/800/(8/3.2808)),3) == round(surv.info[surv.info$Strata_ID==204,]$area_km2,3)
round(unique(strata.db[strata.db$STRATA_ID=="205",]$STRATA_AREA)/(1000*1000/800/(8/3.2808)),3) == round(surv.info[surv.info$Strata_ID==205,]$area_km2,3)

head(strata.db[strata.db$STRATA_ID=="201",])
unique(strata.db[strata.db$STRATA_ID=="201",]$STRATA_AREA) == surv.info[surv.info$Strata_ID==201,]$towable_area
round(unique(strata.db[strata.db$STRATA_ID=="202",]$STRATA_AREA), 1) == round(surv.info[surv.info$Strata_ID==202,]$towable_area, 1)
unique(strata.db[strata.db$STRATA_ID=="203",]$STRATA_AREA) == surv.info[surv.info$Strata_ID==203,]$towable_area
unique(strata.db[strata.db$STRATA_ID=="204",]$STRATA_AREA) == surv.info[surv.info$Strata_ID==204,]$towable_area
unique(strata.db[strata.db$STRATA_ID=="205",]$STRATA_AREA) == surv.info[surv.info$Strata_ID==205,]$towable_area

# Strata 206 in the db?!


unique(dplyr::select(strata.db[strata.db$STRATA_ID=="202",], -LONGITUDE, -LATITUDE, -ORDINAL, -SECONDARY_ID))
surv.info2[surv.info2$Strata_ID==202,]

unique(dplyr::select(strata.db[strata.db$STRATA_ID=="203",], -LONGITUDE, -LATITUDE, -ORDINAL, -SECONDARY_ID))
surv.info2[surv.info2$Strata_ID==203,]

stratum1b

strata.db <- dplyr::select(strata.db, STRATA_ID, SECONDARY_ID, ORDINAL, LATITUDE, LONGITUDE)

# Now do some pre-processing so we can convert data into polysets for PBSmapping
# Rename the headers in the strata for PBSmapping
# Notice the offshore has SID's due to complex nature of the survey strata.
colnames(strata.db) <- c("PID","SID","POS","Y","X")

# Check for NA's and remove any found, the is.finite identifies which parts of the matrix are NA's (not finite)
# The !rowSums then logically identifies all rows with non-finite sums making them FALSE and allowing for them to be removed from data.frame
strata.db <- strata.db[!rowSums(!is.finite(as.matrix(strata.db))),]

# Unforntunately for the offshore strata the SQL database is not set up to put the data how PBSmapping needs it.
# Since I don't have access to the tables the temporary (permenant?) fix is the convoluted for loop to reorder the data 
# as needed by PBSmapping
# For offshore this is just the first step before starting the loop
strata.db <- strata.db[order(strata.db$PID),]

# Setting up variables for a loop.
PIDs <- unique(strata.db$PID)
num.PID <- length(PIDs)
strata.db.PBS <- NULL

# For loop used to reorder the data in the offshore strata so that in conforms to PBSmapping requirements.
for (i in 1:num.PID)
{
  # Make a temporary object to store the [i]th primary Strata ID at a time
  temp1 <- strata.db[strata.db$PID == PIDs[i],]
  # Get all of the secondary ID's for this [i]th primary Strata ID.
  SIDs <- sort(unique(temp1$SID))
  num.SID <- length(SIDs)
  # Now move through the [j]th secondary ID's for within [i]th primary Strata ID
  for(j in 1:num.SID)
  {
    # create a second temporary object with the [i]th primary Strata ID and [j]th secondary ID data
    temp2 <- temp1[temp1$SID == SIDs[j],]
    # If the POS value is increasing this is a polygon and we want these ordered with from least to greatest
    if(temp2$POS[2] > temp2$POS[1]) temp2 <- temp2[order(temp2$POS),]
    # if the POS value is declining this is a hole and we want these ordered from greatest to least 
    if(temp2$POS[2] < temp2$POS[1]) temp2 <- temp2[order(temp2$POS,decreasing =T),]
    
    strata.db.PBS <- rbind(strata.db.PBS,temp2)
  } # end for(j in 1:num.SID)
} # end for (i in 1:num.PID)

# Finally make these into PolySet's for PBSmapping
attr(strata.db.PBS,"projection") <- "LL"
require(PBSmapping)

plotMap(strata.db.PBS, 
        ylim=c(42.4,43), xlim=c(-66.6,-65.6),
        col= RPMG::pastel.colors(64,seed=2))

head(surv.poly[[i]][surv.poly[[i]]$startyear==max(surv.poly[[i]]$startyear),])

head(strata.db.PBS)

unique(strata.db.PBS$PID==surv.poly[[i]][surv.poly[[i]]$startyear==max(surv.poly[[i]]$startyear),]$Strata_ID)

plotMap(surv.poly[[i]][surv.poly[[i]]$startyear==max(surv.poly[[i]]$startyear),],polyProps=polydata[[i]])

strata.db.PBS$PID <- strata.db.PBS$PID-200

plotMap(strata.db.PBS, polyProps=polydata[[i]])


ggplot() + geom_point(data=strata.db[strata.db$STRATA_ID %in% 201:205,], aes(LONGITUDE, LATITUDE, group=SECONDARY_ID, colour=SECONDARY_ID))

unique(strata.db[strata.db$STRATA_ID %in% 201:205,]$COMMENTS)

####################################################################

surv.info <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/survey_information_2021-07-09.csv")
surv.info2 <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/survey_information.csv")


######################### START HERE ###########################

require(ROracle)
source("Other_functions/ScallopQuery.R")
#chan <- odbcConnect(package="db.lib", un=un.ID, pw=pwd.ID, db.con=db.con, db.con,uid=un,pwd=pw,believeNRows=FALSE)
# The query to grab log data
# This is the query to grab the strata for the offshore fishery
quer.off <- paste(
  "SELECT * FROM SCALOFF.OSSTRATA",
  sep=""
)
# Run the query and add data to the log.lst object
strata.db <- ScallopQuery(package="ROracle", un="keyserf", pw="Decade06", db.con="ptran", SQLtext= quer.off)

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/Convert_PBSmapping_into_GIS_shapefiles.R")

banks <- c("BBn")#, "BBs", 
           #"Sab", "GBa", "GBb")
strata <- NULL
for(i in 1:length(banks)){
  strata.db.sub <- strata.db[grepl(x=strata.db$COMMENTS, pattern=banks[i]),]
  strata.db.sub <- dplyr::select(strata.db.sub, STRATA_ID, SECONDARY_ID, ORDINAL, LATITUDE, LONGITUDE)
  colnames(strata.db.sub) <- c("PID","SID","POS","Y","X")
  
  # Check for NA's and remove any found, the is.finite identifies which parts of the matrix are NA's (not finite)
  # The !rowSums then logically identifies all rows with non-finite sums making them FALSE and allowing for them to be removed from data.frame
  strata.db.sub <- strata.db.sub[!rowSums(!is.finite(as.matrix(strata.db.sub))),]
  
  # Unforntunately for the offshore strata the SQL database is not set up to put the data how PBSmapping needs it.
  # Since I don't have access to the tables the temporary (permenant?) fix is the convoluted for loop to reorder the data 
  # as needed by PBSmapping
  # For offshore this is just the first step before starting the loop
  strata.db.sub <- dplyr::arrange(strata.db.sub, PID, SID, POS)
  
  print(paste0("start ", banks[i]))
  
  # Setting up variables for a loop.
  PIDs <- unique(strata.db.sub$PID)
  num.PID <- length(PIDs)
  strata.db.PBS <- NULL
  # For loop used to reorder the data in the offshore strata so that in conforms to PBSmapping requirements.
  for (j in 1:num.PID){
    # Make a temporary object to store the [i]th primary Strata ID at a time
    temp1 <- strata.db.sub[strata.db.sub$PID == PIDs[j],]
    # Get all of the secondary ID's for this [i]th primary Strata ID.
    SIDs <- sort(unique(temp1$SID))
    num.SID <- length(SIDs)
    # Now move through the [j]th secondary ID's for within [i]th primary Strata ID
    for(k in 1:num.SID){
      # create a second temporary object with the [i]th primary Strata ID and [j]th secondary ID data
      temp2 <- temp1[temp1$SID == SIDs[k],]
      # If the POS value is increasing this is a polygon and we want these ordered with from least to greatest
      if(temp2$POS[2] > temp2$POS[1]) temp2 <- temp2[order(temp2$POS),]
      # if the POS value is declining this is a hole and we want these ordered from greatest to least 
      if(temp2$POS[2] < temp2$POS[1]) temp2 <- temp2[order(temp2$POS,decreasing =T),]
      
      strata.db.PBS <- rbind(strata.db.PBS,temp2)
    } # end for(k in 1:num.SID)
    print(PIDs[j])
  } # end for (j in 1:num.PID)
  
  # Finally make these into PolySet's for PBSmapping
  attr(strata.db.PBS,"projection") <- "LL"
  require(PBSmapping)
  
  #names(strata.db.sub)[which(names(strata.db.sub)=="SECONDARY_ID")] <- "SID"
  strata.db.PBS$label <- strata.db.PBS$PID
  stratum <- unique(strata.db.PBS$label)
  sf.obj<- NULL
  for(j in 1:length(stratum)){
    sf.obj[[j]] <- pbs.2.gis(dat=strata.db.PBS[strata.db.PBS$label==stratum[j],], type = "polygon", make.sf=T, env.object = T)[[stratum[j]]]
    sf.obj[[j]] <- st_make_valid(sf.obj[[j]])
    sf.obj[[j]]$label <- unique(strata.db.PBS[strata.db.PBS$label==stratum[j], "label"])
    
    print(stratum[j])
  }
  
  strata[[i]] <- dplyr::bind_rows(sf.obj)
  
  print(paste0("done ", banks[i]))
}  

db <- NULL
info <- NULL
for(i in 1:length(banks)){
  db[[i]] <- data.frame(strata_id = unique(strata[[i]]$label), area=round(st_area(strata[[i]])/1000000, 3))
  info[[i]] <- data.frame(strata_id = surv.info[surv.info$label==banks[i],]$Strata_ID, area=round(surv.info[surv.info$label==banks[i],]$area_km2, 3))
}

db[[which(banks == "Sab")]]
info[[which(banks == "Sab")]]
#database needs updating

db[[which(banks == "BBn")]]
info[[which(banks == "BBn")]]
#database needs updating

db[[which(banks == "BBs")]]
info[[which(banks == "BBs")]]
#close but not quite

db[[which(banks == "GBa")]]
info[[which(banks == "GBa")]]
#close but not quite

#map(strata, function(x) plot(x, col=RPMG::pastel.colors(n=64)))

##### BBn ######
stratashp <- st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore_survey_strata/BBn.shp")
stratum <- NULL
path <- "Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank North Historic Commercial Catch Rate Edited/"
for(i in 1:5){
  files <- list.files(path)
  if(!bank=="BBs") file <- files[which(grepl(x=files, paste0("Strata", i)) & grepl(x=files, paste0(".shp")) & grepl(x=files, paste0("UTM")) & !grepl(x=files, "part")  & !grepl(x=files, ".xml"))]
  stratum[[i]] <- st_read(paste0(path, file))
  stratum[[i]]$Strata_ID <- i
}

##### BBs ######
stratashp <- st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore_survey_strata/BBs.shp")
stratum <- NULL
path <- "Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Browns Bank South Surficial Geology Edited/"
PName <- data.frame(Strt_ID = stratashp$Strt_ID, PName = c("_GravelLag_", "_GravelLagThinSand_", "_ThickSand_", "_ThinSandOverGravelLag_"))
for(i in 1:4){
  files <- list.files(path)
  file <- files[which(grepl(x=files, PName$PName[PName$Strt_ID==stratashp$Strt_ID[i]]) & grepl(x=files, paste0(".shp")) & grepl(x=files, paste0("UTM")) & !grepl(x=files, "part")  & !grepl(x=files, ".xml"))]
  stratum[[i]] <- st_read(paste0(path, file))
  stratum[[i]]$Strata_ID <- stratashp$Strt_ID[i]
}

##### Sab ######
stratashp <- st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore_survey_strata/Sab.shp")
stratum <- NULL
path <- "Y:/Offshore/Survey/SurveyDatabase/Strata/shp/polygons/Sable Bank Historic Survey Index Edited/"
PName <- data.frame(Strt_ID = stratashp$Strt_ID, PName = c("_VLow_", "_Low_", "_Med_", "_High_", "_VHigh_"))
for(i in 1:5){
  files <- list.files(path)
  file <- files[which(grepl(x=files, PName$PName[PName$Strt_ID==stratashp$Strt_ID[i]]) & grepl(x=files, paste0(".shp")) & grepl(x=files, paste0("utm")) & !grepl(x=files, "part")  & !grepl(x=files, ".xml"))]
  stratum[[i]] <- st_read(paste0(path, file))
  stratum[[i]]$Strata_ID <- stratashp$Strt_ID[i]
}

realstrata <- dplyr::bind_rows(stratum)

realstrata %>%
  dplyr::mutate(area=st_area(.)) %>%
  dplyr::group_by(Strata_ID) %>%
  dplyr::summarise(area=round(sum(area)/1000000, 3)) 
  
realstrata <- st_transform(realstrata, 4326)

# it's not the 15m bathymetry....
# bathy.scallopmap <- st_transform(bathy.scallopmap, 4326)
# 
# bathy.scallopmap <- st_crop(bathy.scallopmap, st_bbox(realstrata))

bathy.org <- getNOAA.bathy(lon1 = st_bbox(realstrata)$xmin ,lon2=st_bbox(realstrata)$xmax,lat1 = st_bbox(realstrata)$ymin,lat2=st_bbox(realstrata)$ymax,resolution =1)

bathy <- marmap::as.raster(bathy.org)

# Now clip this to the bounding area, note that the bathy is basically a EPSG:4326 so we need to crop it accordingly and transform our b.box to this...
bathy <- crop(bathy,as_Spatial(realstrata))
bathy.sf <- bathy %>% 
  st_as_stars()

ggplot() + #geom_sf(data = strata[[which(banks=="BBn")]], fill="#1b9e77", alpha=0.2) + coord_sf() +
  #geom_sf(data = stratashp, fill="#d95f02", colour="#d95f02", alpha=0.2, lty=2) + 
 geom_sf(data=realstrata, aes(fill=as.factor(Strata_ID)))+#, fill="#7570b3", colour="#7570b3", alpha=0.2, lty=2) +
  # geom_sf(data=st_contour(bathy.sf, breaks=seq(-250,-50, 10)), colour="black", fill=NA)+ 
  # coord_sf(xlim=c(-66.25, -66.1), ylim=c(42.8, 42.9))+
  theme_bw() +
  geom_segment(data=survBBn.dat[survBBn.dat$year==2013 & survBBn.dat$state=="live",], aes(x=slon, xend=elon, y=slat, yend=elat)) +
  geom_text(data=survBBn.dat[survBBn.dat$year==2013 & survBBn.dat$state=="live",], aes(x=slon, y=slat, label=tow))

require(plotly)


ggplot() + geom_sf(data=st_union(realstrata), fill="#d95f02", colour=NA, lwd=3, alpha=1) +
  #geom_sf(data=st_union(realstrata), fill="white", colour=NA) +
  #geom_sf(data=st_union(strata[[which(banks=="BBn")]]), fill=NA, colour="#d95f02", lwd=2, alpha=0.2) +
  geom_sf(data=st_union(stratashp), fill="#7570b3", colour=NA, lwd=2, alpha=1) + 
  #geom_sf(data=st_contour(bathy.sf, breaks=-110), colour="black", fill=NA)+ 
 # coord_sf(xlim=c(-66.25, -66.1), ylim=c(42.8, 42.9))+
  theme_bw()

#BBn has issues
#BBs is fine

realstrata
#BBn strata
stratashp %>% st_transform(32619) %>% st_area

#indreport <- ScallopQuery(package="ROracle", un="keyserf", pw="Decade06", db.con="ptran", 
 #            SQLtext= "SELECT * FROM SCALOFF.OSINDUSTRYREPORT_SS_VW")
load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")

bbndat <- bank.dat$BBn %>%
  st_as_sf(coords=c("lon", "lat")) %>%
  filter(state=="live") %>%
  st_set_crs(4326)

ggplot() + geom_sf(data=st_union(realstrata), fill="#d95f02", colour=NA, lwd=3, alpha=1) +
  #geom_sf(data=st_union(realstrata), fill="white", colour=NA) +
  #geom_sf(data=st_union(strata[[which(banks=="BBn")]]), fill=NA, colour="#d95f02", lwd=2, alpha=0.2) +
  geom_sf(data=st_union(stratashp), fill="#7570b3", colour=NA, lwd=2, alpha=1) + 
  geom_sf(data=st_contour(bathy.sf, breaks=-110), colour="black", lwd=2,fill=NA)+ 
  coord_sf(expand = F)+
  theme_bw() +
  geom_sf(data=bbndat, size=1) +
  ggtitle("BBn tows 1991-2019")

load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")

ggplot() + geom_sf(data=st_union(realstrata), fill="#d95f02", colour=NA, lwd=3, alpha=1) +
  #geom_sf(data=st_union(realstrata), fill="white", colour=NA) +
  #geom_sf(data=st_union(strata[[which(banks=="BBn")]]), fill=NA, colour="#d95f02", lwd=2, alpha=0.2) +
  geom_sf(data=st_union(stratashp), fill="#7570b3", colour=NA, lwd=2, alpha=1) + 
  geom_sf(data=st_contour(bathy.sf, breaks=-110), colour="black", lwd=2,fill=NA)+ 
  coord_sf(expand = F)+
  theme_bw() +
  geom_sf(data=bbndat[bbndat$year>2008,], size=1) +
  ggtitle("BBn survey tows 2009-2019 (current strata in use since ~2015)") + 
  facet_wrap(~year)

source("C:/Documents/Assessment_fns/Fishery/logs_and_fishery_data.R")
logs_and_fish(loc = "offshore", year=2002:2012, get.marfis=F, direct="Y:/Offshore/Assessment/", direct_fns="C:/Documents/Assessment_fns/")
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
bbnfish <- fish.dat %>% filter(bank=="BBn") %>%
  st_as_sf(coords=c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_crop(st_bbox(realstrata))

ggplot() + geom_sf(data=st_union(realstrata), fill="#d95f02", colour=NA, lwd=3, alpha=1) +
  #geom_sf(data=st_union(realstrata), fill="white", colour=NA) +
  #geom_sf(data=st_union(strata[[which(banks=="BBn")]]), fill=NA, colour="#d95f02", lwd=2, alpha=0.2) +
  geom_sf(data=st_union(stratashp), fill="#7570b3", colour=NA, lwd=2, alpha=1) + 
  geom_sf(data=st_contour(bathy.sf, breaks=-110), colour="black", lwd=2,fill=NA)+ 
  coord_sf(expand = F)+
  theme_bw() +
  geom_sf(data=bbnfish, size=1) +
  ggtitle("BBn fishing tows 2002-2012 (current strata in use since ~2015)") 
