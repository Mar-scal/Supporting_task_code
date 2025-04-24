# For SPANS request

require(ggplot2)
require(tidyr)
require(dplyr)
require(patchwork)
require(sf)
require(ggrepel)
require(readxl)
require(tidyverse)

funs <- c("https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Fishery/jackknife.r",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/create_grid.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/pectinid_projector_sf.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/github_spatial_import.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Fishery/logs_and_fishery_data.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs)
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}

# privacy screen the CDD files for 2002-2010 using my 10km2 gridding code (ensuring 5 trips/vessels per cell) and send raw anonymized data for trips that meet those criteria.
# privacy screen our 1990-2001 data using my 10km2 gridding code (ensuring 5 trips/vessels per cell) and send raw anonymized data for trips that meet those criteria. 
# DR form, just for our tracking purposes and because it’s a good spot to put column metadata. 

# get our data
logs_and_fish(loc="offshore", get.local=T, direct="Y:/Offshore/Assessment/", year = 1990:2010)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish.dat <- fish.dat[!is.na(fish.dat$lat),]
fish.dat <- fish.dat[!fish.dat$lon==0,]
fish.dat <- fish.dat[!fish.dat$lat==0,]

fish.dat$month <- month(ymd(fish.dat$fished))
sabmid <- fish.dat[!is.na(fish.dat$sfa),]

# summarize and make sure they match (or are close?)
sabmid %>% group_by(year, month) %>% summarize(kg=sum(pro.repwt))

# spatial analysis
fish_sf <- st_as_sf(fish.dat, coords=c(X="lon", Y="lat"), crs=4326)

offshore <- github_spatial_import("offshore", "offshore.zip", quiet=T)
#ggplot() + geom_sf(data=offshore) + facet_wrap(~ID)

sf_use_s2(FALSE)
fish_sf <- st_intersection(fish_sf, offshore)
sf_use_s2(TRUE)
unique(fish_sf$ID.1)
unique(fish_sf$bank)
unique(fish_sf$sfa)
fish_sf$ID.1 <- gsub(x=fish_sf$ID.1, pattern="SFA", replacement="")

# if shp ID doesn't match log sfa:
dim(fish_sf[!fish_sf$ID.1==fish_sf$sfa & !is.na(fish_sf$sfa),])
# if shp ID matches log sfa:
dim(fish_sf[fish_sf$ID.1==fish_sf$sfa,])
unique(fish_sf$sfa)
unique(fish_sf$ID.1)

# look at the "bad" matches
check <- fish_sf[!fish_sf$ID.1==fish_sf$sfa & !is.na(fish_sf$sfa),]
ggplot() + geom_point(data=check, aes(sfa, ID.1))
#unique(fish_sf[fish_sf$bank=="Ban",]$ID.1)
#unique(fish_sf[fish_sf$bank=="Sab",]$ID.1)
table(check$sfa, check$ID.1) # relatively few misattributed records. Use intersected results

fish_sf$sfa <- fish_sf$ID.1

#fish_sf <- fish_sf[!fish_sf$sfa %in% c("25A.shp"),]
fish_sf <- fish_sf %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])
st_geometry(fish_sf) <- NULL
fish_sf <- fish_sf[!is.na(fish_sf$pro.repwt),]
fish_sf$period <- cut(fish_sf$year, breaks=seq(1990,2011, 5), right=F)
fish_sf$sfa <- gsub(x=fish_sf$sfa, pattern=".shp", replacement="", fixed=T)
fish_sf <- dplyr::select(fish_sf, sfa, year, lon, lat, pro.repwt, hm, vesid, vrnum, licence)
unique(fish_sf$vesid)

# trying Mike's code on our log data
require(Mar.utils)
log_mike <- NULL
poly_kg <- NULL
for(i in unique(fish_sf$year)){
  print(i)
  log_mike_yr <- Mar.utils::assess_privacy(fish_sf[fish_sf$year ==i,], agg.fields=c("pro.repwt"),
                                           calculate = "SUM", 
                                           sens.fields = c("vesid"),
                                           lat.field = "lat",
                                           lon.field = "lon",
                                           for.public=T, create.spatial=T)
  if(!is.null(log_mike_yr$Grid2Min)) {
    log_mike_yr$Grid2Min$year <- i
    log_mike_yr$Grid2Min <- st_transform(log_mike_yr$Grid2Min, 4326)
    log_mike <- rbind(log_mike, log_mike_yr$Grid2Min)
    log_mike_yr$POLY_AGG$year <- i
    log_mike_yr$POLY_AGG <- st_transform(log_mike_yr$POLY_AGG, 4326)
    poly_kg <- rbind(poly_kg, log_mike_yr$POLY_AGG)
  }
}

log_mike_hm <- NULL
poly_hm <- NULL
for(i in unique(fish_sf$year)){
  print(i)
  log_mike_yr <- Mar.utils::assess_privacy(fish_sf[fish_sf$year ==i & !is.na(fish_sf$hm),], agg.fields=c("hm"),
                                           calculate = "SUM", 
                                           sens.fields = c("vesid"),
                                           lat.field = "lat",
                                           lon.field = "lon",
                                           for.public=T, create.spatial=T)
  if(!is.null(log_mike_yr$Grid2Min)) {
    log_mike_yr$Grid2Min$year <- i
    log_mike_yr$Grid2Min <- st_transform(log_mike_yr$Grid2Min, 4326)
    log_mike_hm <- rbind(log_mike_hm, log_mike_yr$Grid2Min)
    log_mike_yr$POLY_AGG$year <- i
    log_mike_yr$POLY_AGG <- st_transform(log_mike_yr$POLY_AGG, 4326)
    poly_hm <- rbind(poly_hm, log_mike_yr$POLY_AGG)
  }
}

ggplot() + geom_sf(data=log_mike[log_mike$pro.repwt>0,], aes(fill=pro.repwt), colour=NA) + facet_wrap(~year) +
  geom_sf(data=poly_kg[poly_kg$CAN_SHOW=="NO",])
ggplot() + geom_sf(data=log_mike_hm[log_mike_hm$hm>0,], aes(fill=hm), colour=NA) + facet_wrap(~year) +
  geom_sf(data=poly_hm[poly_hm$CAN_SHOW=="NO",])

# devtools::install_github('Maritimes/Mar.utils')
# devtools::install_github('PopulationEcologyDivision/Mar.data')

# compare screening to unscreened
screened <- log_mike %>% dplyr::group_by(year) %>%
  dplyr::summarize(screen_kg=sum(pro.repwt, na.rm=T)) %>% 
  st_drop_geometry()  
screened_hm <- log_mike_hm %>% dplyr::group_by(year) %>%
  dplyr::summarize(screen_hm=sum(hm, na.rm=T)) %>% 
  st_drop_geometry()  

unscreened <- fish_sf %>% dplyr::group_by(year) %>%
  dplyr::summarize(kg=sum(pro.repwt, na.rm=T),
                   hm=sum(hm, na.rm=T)) %>% 
  st_drop_geometry()

compare <- full_join(screened, unscreened)
compare <- full_join(compare, screened_hm)
compare$screen_kg[is.na(compare$screen_kg)] <- 0
compare$screen_hm[is.na(compare$screen_hm)] <- 0
compare$kg[is.na(compare$kg)] <- 0
compare$hm[is.na(compare$hm)] <- 0
compare$diff_kg <- compare$kg-compare$screen_kg
compare$diff_hm <- compare$hm-compare$screen_hm

#plot limits
lims <- poly_kg %>%
  st_transform(32620) %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_buffer(dist=50000) %>%
  st_bbox()

bp <- pecjector(area=list(y = c(lims$ymin[[1]], lims$ymax[[1]]),
                          x =c(lims$xmin[[1]], lims$xmax[[1]]),
                          crs = 32620),
                repo = 'github',c_sys = 32620, add_layer = list(bathy = c(200,'c'), sfa = 'offshore'),plot=F, quiet=T)# +

#manual adjustment to scalebar text size
#bp$layers[[5]]$geom_params$text_cex <- 1.5

png(filename=paste0("Y:/Offshore/Data requests/2024/Maps_for_SPANS_CDDrequest/footprint_kg_Science.png"),width=16, height=10, units = "in", res=420)
print(bp +
        #geom_sf(data=foot_grid[!is.na(foot_grid$kg) & !is.na(foot_grid$year),], fill="transparent", colour=NA, show.legend=T) +
        geom_sf(data=log_mike[!is.na(log_mike$pro.repwt) & !is.na(log_mike$year) & log_mike$pro.repwt>0 & log_mike$year<2002,], aes(fill=pro.repwt/1000), colour=NA, show.legend=T) +
        geom_text(data=compare[!is.na(compare$year) & compare$year<2002,], aes(x=-Inf, y=-Inf, label=paste0("kg screened in: ", round(screen_kg,0), "\nkg screened out: ", round(diff_kg,0))), hjust = -0.1, vjust = -0.1)+
        geom_sf(data=poly_kg[poly_kg$CAN_SHOW=="NO" & poly_kg$year<2002,], fill="black", alpha=0.1)+
        theme(legend.position="right") +
        scale_fill_viridis_c(name=paste0("Catch", " (t)"), option="rocket", begin=0.95, end=0)+
        coord_sf(expand=F)+
        # geom_sf_text(data = nonGB[nonGB$SFA %in% base$ID,],
        #              aes(label = final),
        #              size=6,
        #              fun.geometry = sf::st_centroid,
        #              nudge_x = nonGB[nonGB$SFA %in% base$ID,]$nudgex,
        #              nudge_y = nonGB[nonGB$SFA %in% base$ID,]$nudgey
        # )+
        facet_wrap(~year, nrow=4)
)
dev.off()

png(filename=paste0("Y:/Offshore/Data requests/2024/Maps_for_SPANS_CDDrequest/footprint_hm_Science.png"),width=16, height=10, units = "in", res=420)
print(bp +
        #geom_sf(data=foot_grid[!is.na(foot_grid$kg) & !is.na(foot_grid$year),], fill="transparent", colour=NA, show.legend=T) +
        geom_sf(data=log_mike_hm[!is.na(log_mike_hm$hm) & !is.na(log_mike_hm$year) & log_mike_hm$hm>0,], aes(fill=hm), colour=NA, show.legend=T) +
        geom_text(data=compare[!is.na(compare$year),], aes(x=-Inf, y=-Inf, label=paste0("hm screened in: ", round(screen_hm,0), "\nhm screened out: ", round(diff_kg,0))), hjust = -0.1, vjust = -0.1)+
        geom_sf(data=poly_hm[poly_hm$CAN_SHOW=="NO",], fill="black", alpha=0.1)+
        theme(legend.position="right") +
        scale_fill_viridis_c(name=paste0("Effort", " (hm)"), option="rocket", begin=0.95, end=0)+
        coord_sf(expand=F)+
        # geom_sf_text(data = nonGB[nonGB$SFA %in% base$ID,],
        #              aes(label = final),
        #              size=6,
        #              fun.geometry = sf::st_centroid,
        #              nudge_x = nonGB[nonGB$SFA %in% base$ID,]$nudgex,
        #              nudge_y = nonGB[nonGB$SFA %in% base$ID,]$nudgey
        # )+
        facet_wrap(~year, nrow=5)
)
dev.off()

# for CSV
log_mike$kgs <- log_mike$pro.repwt
log_mike <- log_mike[!is.na(log_mike$kgs),]
log_mike <- st_centroid(log_mike)
log_mike <- st_transform(log_mike, 4326)

log_mike <- st_intersection(log_mike, offshore)
log_mike_hm <- st_intersection(log_mike_hm, offshore)
log_mike$sfa <- gsub(x=log_mike$ID, pattern=".shp", replacement="", fixed=T)
log_mike_hm$sfa <- gsub(x=log_mike_hm$ID, pattern=".shp", replacement="", fixed=T)
log_mike$sfa <- gsub(x=log_mike$sfa, pattern="SFA", replacement="", fixed=T)
log_mike_hm$sfa <- gsub(x=log_mike_hm$sfa, pattern="SFA", replacement="", fixed=T)
log_mike <- dplyr::select(log_mike, -ID)
log_mike_hm <- dplyr::select(log_mike_hm, -ID)

coords <- as.data.frame(st_coordinates(log_mike))
log_mike$lon <- coords$X
log_mike$lat <- coords$Y
st_geometry(log_mike) <- NULL
#log_mike$SFA <- "25A"
log_mike <- dplyr::select(log_mike, year, sfa, kgs, lon, lat)
names(log_mike) <- c("year", "SFA", "landings_kg", "lon_centroid", "lat_centroid")
log_mike <- arrange(log_mike, SFA, year, lon_centroid, lat_centroid)
log_mike$lon_centroid <- round(log_mike$lon_centroid, 5)
log_mike$lat_centroid <- round(log_mike$lat_centroid, 5)
log_mike <- log_mike[log_mike$landings_kg>0,]
log_mike <- log_mike[log_mike$year<2002,]
write.csv(x=log_mike, file = "Y:/Offshore/Data requests/2024/Maps_for_SPANS_CDDrequest/logdata_1990-2001_kg_gridded_screened_Science.csv")

log_mike_hm <- log_mike_hm[!is.na(log_mike_hm$hm),]
log_mike_hm <- st_centroid(log_mike_hm)
log_mike_hm <- st_transform(log_mike_hm, 4326)
coords <- as.data.frame(st_coordinates(log_mike_hm))
log_mike_hm$lon <- coords$X
log_mike_hm$lat <- coords$Y
st_geometry(log_mike_hm) <- NULL
#log_mike_hm$SFA <- "25A"
log_mike_hm <- dplyr::select(log_mike_hm, year, sfa, hm, lon, lat) 
names(log_mike_hm) <- c("year", "SFA","effort_hm", "lon_centroid", "lat_centroid") 
log_mike_hm <- arrange(log_mike_hm, SFA, year, lon_centroid, lat_centroid) 
log_mike_hm$lon_centroid <- round(log_mike_hm$lon_centroid, 5)
log_mike_hm$lat_centroid <- round(log_mike_hm$lat_centroid, 5)
log_mike_hm <- log_mike_hm[log_mike_hm$effort_hm>0,]
write.csv(x=log_mike_hm, file = "Y:/Offshore/Data requests/2024/Maps_for_SPANS_CDDrequest/logdata_1990-2010_hm_gridded_screened_Science.csv")

##### CDD DATA
# compare CDD 2002-2010 data to ours first, then privacy screen after doing the analysis below (vessel/license/company)

cdd1 <- read.csv("Y:/Offshore/Data requests/2024/maps_for_SPANS_CDDrequest/Offshore Scallop Effort Data - 2002-10_1.csv")
cdd2 <- read.csv("Y:/Offshore/Data requests/2024/maps_for_SPANS_CDDrequest/Offshore Scallop Effort Data - 2002-10_2.csv")

names(cdd1) == names(cdd2)
cdd <- rbind(cdd1, cdd2)
cdd$YEAR <- lubridate::year(lubridate::ymd(cdd$DATE_SAILED))
cdd <- as.data.frame(lapply(cdd, function(x) gsub(x = x, pattern=",", "", fixed=T)))
cdd$ENT_LATITUDE <- as.numeric(cdd$ENT_LATITUDE)
cdd$ENT_LONGITUDE <- as.numeric(cdd$ENT_LONGITUDE)
cdd$ENT_LATITUDE <- cdd$ENT_LATITUDE/100
cdd$ENT_LONGITUDE <- -cdd$ENT_LONGITUDE/100
cdd$RND_WEIGHT_KGS <- as.numeric(cdd$RND_WEIGHT_KGS)

source("C:/Users/keyserf/Documents/GitHub/Assessment_fns/Survey_and_OSAC/convert.dd.dddd.r")
cdd$ENT_LATITUDE <- convert.dd.dddd(cdd$ENT_LATITUDE)
cdd$ENT_LONGITUDE <- convert.dd.dddd(cdd$ENT_LONGITUDE)

cdd <- sf::st_as_sf(x = cdd[!is.na(cdd$ENT_LATITUDE),], coords=c(X="ENT_LONGITUDE", Y="ENT_LATITUDE"), crs=4326, remove=F)
ggplot() + geom_sf(data=cdd)
#cdd <- st_intersection(cdd, offshore)
cdd <- cdd %>% st_drop_geometry()
cdd[is.na(cdd$ENT_LATITUDE),]
cdd[is.na(cdd$ENT_LONGITUDE),]


#write.csv(cdd, "C:/Users/keyserf/Documents/temp_data/cdd.csv")

require(Mar.utils)
#source("C:/Users/keyserf/Documents/Github/Mar.utils/R/assess_privacy.r")
# source("C:/Users/keyserf/Documents/Github/Mar.utils/R/df_to_sf.r")

#cdd <- cdd[cdd$AREA == "25A",]
#cdd$h <- (as.numeric(cdd$NUM_TOW) * as.numeric(cdd$AVG_TOW))/60
# no gear width in cdd data

cdd_mike <- NULL
poly_kg <- NULL
for(i in unique(cdd$YEAR)){
  print(i)
  cdd_mike_yr <- Mar.utils::assess_privacy(cdd[cdd$YEAR==i,], agg.fields=c("RND_WEIGHT_KGS"),
                                           calculate = "SUM", 
                                           sens.fields = c("VR_NUMBER", "LICENCE_ID"),
                                           lat.field = "ENT_LATITUDE",
                                           lon.field = "ENT_LONGITUDE",
                                           for.public=T)
  if(!is.null(cdd_mike_yr$Grid2Min)) {
    cdd_mike_yr$Grid2Min$YEAR <- i
    cdd_mike_yr$Grid2Min <- st_transform(cdd_mike_yr$Grid2Min, 4326)
    cdd_mike <- rbind(cdd_mike, cdd_mike_yr$Grid2Min)
    cdd_mike_yr$POLY_AGG$YEAR <- i
    cdd_mike_yr$POLY_AGG <- st_transform(cdd_mike_yr$POLY_AGG, 4326)
    poly_kg <- rbind(poly_kg, cdd_mike_yr$POLY_AGG)
  }
}

ggplot() + geom_sf(data=cdd_mike[cdd_mike$RND_WEIGHT_KGS>0,], aes(fill=RND_WEIGHT_KGS), colour=NA) + facet_wrap(~YEAR)

# devtools::install_github('Maritimes/Mar.utils')
# devtools::install_github('PopulationEcologyDivision/Mar.data')

# compare screening to unscreened
screened <- cdd_mike %>% dplyr::group_by(YEAR) %>%
  dplyr::summarize(screen_kg=sum(RND_WEIGHT_KGS)/8.3) %>% 
  st_drop_geometry()  

unscreened <- cdd %>% dplyr::group_by(YEAR) %>%
  dplyr::summarize(kg=sum(RND_WEIGHT_KGS)/8.3) %>% 
  st_drop_geometry()

compare <- full_join(screened, unscreened)
compare$screen_kg[is.na(compare$screen_kg)] <- 0
#compare$screen_hm[is.na(compare$screen_hm)] <- 0
compare$kg[is.na(compare$kg)] <- 0
#compare$hm[is.na(compare$hm)] <- 0
compare$diff_kg <- compare$kg-compare$screen_kg
#compare$diff_hm <- compare$hm-compare$screen_hm

# plot limits
lims <- poly_kg %>%
  st_transform(32620) %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_buffer(dist=50000) %>%
  st_bbox()

bp <- pecjector(area=list(y = c(lims$ymin[[1]], lims$ymax[[1]]),
                          x =c(lims$xmin[[1]], lims$xmax[[1]]),
                          crs = 32620),
                repo = 'github',c_sys = 32620, add_layer = list(bathy = c(200,'c'), sfa = 'offshore'),plot=F, quiet=T)# +

#manual adjustment to scalebar text size
#bp$layers[[5]]$geom_params$text_cex <- 1.5

png(filename=paste0("Y:/Offshore/Data requests/2024/Maps_for_SPANS_CDDrequest/footprint_kg_CDD.png"),width=16, height=10, units = "in", res=420)
print(bp +
        #geom_sf(data=foot_grid[!is.na(foot_grid$kg) & !is.na(foot_grid$year),], fill="transparent", colour=NA, show.legend=T) +
        geom_sf(data=cdd_mike[!is.na(cdd_mike$RND_WEIGHT_KGS) & !is.na(cdd_mike$YEAR) & cdd_mike$RND_WEIGHT_KGS>0,], aes(fill=RND_WEIGHT_KGS/8.3/1000), colour=NA, show.legend=T) +
        geom_text(data=compare[!is.na(compare$YEAR),], aes(x=-Inf, y=-Inf, label=paste0("kg screened in: ", round(screen_kg,0), "\nkg screened out: ", round(diff_kg,0))), hjust = -0.1, vjust = -0.1)+
        geom_sf(data=poly_kg[poly_kg$CAN_SHOW=="NO",], fill="black", alpha=0.1)+
        theme(legend.position="right") +
        scale_fill_viridis_c(name=paste0("Catch", " (t)"), option="rocket", begin=0.95, end=0)+
        coord_sf(expand=F)+
        # geom_sf_text(data = nonGB[nonGB$SFA %in% base$ID,],
        #              aes(label = final),
        #              size=6,
        #              fun.geometry = sf::st_centroid,
        #              nudge_x = nonGB[nonGB$SFA %in% base$ID,]$nudgex,
        #              nudge_y = nonGB[nonGB$SFA %in% base$ID,]$nudgey
        # )+
        facet_wrap(~YEAR, nrow=3)
)
dev.off()

# for CSV
cdd_mike$kgs <- cdd_mike$RND_WEIGHT_KGS/8.3
cdd_mike <- cdd_mike[!is.na(cdd_mike$kgs),]
cdd_mike <- st_centroid(cdd_mike)
cdd_mike <- st_transform(cdd_mike, 4326)

cdd_mike <- st_intersection(cdd_mike, offshore)
cdd_mike$sfa <- gsub(x=cdd_mike$ID, pattern=".shp", replacement="", fixed=T)
cdd_mike$sfa <- gsub(x=cdd_mike$sfa, pattern="SFA", replacement="", fixed=T)
cdd_mike <- dplyr::select(cdd_mike, -ID)

coords <- as.data.frame(st_coordinates(cdd_mike))
cdd_mike$lon <- coords$X
cdd_mike$lat <- coords$Y
st_geometry(cdd_mike) <- NULL
#cdd_mike$SFA <- "25A"
cdd_mike <- dplyr::select(cdd_mike,YEAR, sfa, kgs, lon, lat)# SFA, 
names(cdd_mike) <- c("year", "SFA", "landings_kg", "lon_centroid", "lat_centroid")#"SFA", 
cdd_mike <- arrange(cdd_mike, SFA, year, lon_centroid, lat_centroid)#SFA, 
cdd_mike$lon_centroid <- round(cdd_mike$lon_centroid, 5)
cdd_mike$lat_centroid <- round(cdd_mike$lat_centroid, 5)
cdd_mike <- cdd_mike[cdd_mike$landings_kg>0,]
write.csv(x=cdd_mike, file = "Y:/Offshore/Data requests/2024/Maps_for_SPANS_CDDrequest/logdata_2002-2010_gridded_screened_CDD.csv")





