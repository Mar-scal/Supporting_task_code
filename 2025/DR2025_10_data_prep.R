
# set your directory
direct <- "Y:/Offshore/Assessment/"

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Fishery/logs_and_fishery_data.r")

# to export csv (adjust years to whatever you want):
logs_and_fish(loc="offshore", year=2009:2024, get.marfis = F, export = F, direct = direct)

# pull out Mersey trips
mersey <- slip.dat[slip.dat$company=="MERSEY SEAFOODS LTD",]
mersey <- new.log.dat[new.log.dat$mdid %in% mersey$mdid,]
mersey <- arrange(mersey, year, tripnum, fished, watch)
write.csv(mersey, "Y:/Offshore/Assessment/2025/Supporting_tasks/Mersey_DR2025_10/Mersey_logs_2009-2024.csv")




# For fleet-wide request

require(ggplot2)
require(tidyr)
require(dplyr)
require(patchwork)
require(sf)
require(ggrepel)
require(readxl)
require(tidyverse)
# devtools::install_github('Maritimes/Mar.utils')
# devtools::install_github('PopulationEcologyDivision/Mar.data')


funs <- c("https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Fishery/jackknife.r",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/create_grid.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/pectinid_projector_sf.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/github_spatial_import.R")
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
#logs_and_fish(loc="offshore", get.local=T, direct="Y:/Offshore/Assessment/", year = 1990:2010)
# already done above
fish.dat<-new.log.dat #merge(new.log.dat,old.log.dat,all=T)
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
fish_sf$period <- cut(fish_sf$year, breaks=seq(2009,2024, 5), right=F)
fish_sf$sfa <- gsub(x=fish_sf$sfa, pattern=".shp", replacement="", fixed=T)
fish_sf <- dplyr::select(fish_sf, sfa, year, lon, lat, pro.repwt, hm, vrnum, licence)
unique(fish_sf$vrnum)

# trying Mike's code on our log data
require(Mar.utils)
log_mike <- NULL
poly_kg <- NULL
for(i in unique(fish_sf$year)){
  print(i)
  log_mike_yr <- Mar.utils::assess_privacy(fish_sf[fish_sf$year ==i,], agg.fields=c("pro.repwt"),
                                           calculate = "SUM", 
                                           sens.fields = c("vrnum"),
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
                                           sens.fields = c("vrnum"),
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

joined <- cbind(log_mike, log_mike_hm)

ggplot() + geom_sf(data=log_mike[log_mike$pro.repwt>0,], aes(fill=pro.repwt), colour=NA) + facet_wrap(~year) +
  geom_sf(data=poly_kg[poly_kg$CAN_SHOW=="NO",])
ggplot() + geom_sf(data=log_mike_hm[log_mike_hm$hm>0,], aes(fill=hm), colour=NA) + facet_wrap(~year) +
  geom_sf(data=poly_hm[poly_hm$CAN_SHOW=="NO",])


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

print(bp +
        #geom_sf(data=foot_grid[!is.na(foot_grid$kg) & !is.na(foot_grid$year),], fill="transparent", colour=NA, show.legend=T) +
        geom_sf(data=log_mike[!is.na(log_mike$pro.repwt) & !is.na(log_mike$year) & log_mike$pro.repwt>0,], aes(fill=pro.repwt/1000), colour=NA, show.legend=T) +
        geom_text(data=compare[!is.na(compare$year),], aes(x=-Inf, y=-Inf, label=paste0("kg screened in: ", round(screen_kg,0), "\nkg screened out: ", round(diff_kg,0))), hjust = -0.1, vjust = -0.1)+
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
        facet_wrap(~year, nrow=4)
)

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

# for CSV
log_mike$kgs <- log_mike$pro.repwt
st_geometry(log_mike_hm) <- NULL
log_mike <- full_join(log_mike, log_mike_hm)
#log_mike <- log_mike[!is.na(log_mike$kgs),]
log_mike <- st_centroid(log_mike)
log_mike <- st_transform(log_mike, 4326)

log_mike <- st_intersection(log_mike, offshore)
#log_mike_hm <- st_intersection(log_mike_hm, offshore)
log_mike$sfa <- gsub(x=log_mike$ID, pattern=".shp", replacement="", fixed=T)
#log_mike_hm$sfa <- gsub(x=log_mike_hm$ID, pattern=".shp", replacement="", fixed=T)
log_mike$sfa <- gsub(x=log_mike$sfa, pattern="SFA", replacement="", fixed=T)
#log_mike_hm$sfa <- gsub(x=log_mike_hm$sfa, pattern="SFA", replacement="", fixed=T)
log_mike <- dplyr::select(log_mike, -ID)
#log_mike_hm <- dplyr::select(log_mike_hm, -ID)

coords <- as.data.frame(st_coordinates(log_mike))
log_mike$lon <- coords$X
log_mike$lat <- coords$Y
st_geometry(log_mike) <- NULL
#log_mike$SFA <- "25A"
log_mike <- dplyr::select(log_mike, year, sfa, kgs, hm, lon, lat)
names(log_mike) <- c("year", "SFA", "landings_kg", "effort_hm", "lon_centroid", "lat_centroid")
log_mike <- arrange(log_mike, SFA, year, lon_centroid, lat_centroid)
log_mike$lon_centroid <- round(log_mike$lon_centroid, 5)
log_mike$lat_centroid <- round(log_mike$lat_centroid, 5)
log_mike <- log_mike[!(log_mike$landings_kg==0 & log_mike$effort_hm==0),]

# log_mike_hm <- log_mike_hm[!is.na(log_mike_hm$hm),]
# log_mike_hm <- st_centroid(log_mike_hm)
# log_mike_hm <- st_transform(log_mike_hm, 4326)
# coords <- as.data.frame(st_coordinates(log_mike_hm))
# log_mike_hm$lon <- coords$X
# log_mike_hm$lat <- coords$Y
# st_geometry(log_mike_hm) <- NULL
# #log_mike_hm$SFA <- "25A"
# log_mike_hm <- dplyr::select(log_mike_hm, year, sfa, hm, lon, lat) 
# names(log_mike_hm) <- c("year", "SFA","effort_hm", "lon_centroid", "lat_centroid") 
# log_mike_hm <- arrange(log_mike_hm, SFA, year, lon_centroid, lat_centroid) 
# log_mike_hm$lon_centroid <- round(log_mike_hm$lon_centroid, 5)
# log_mike_hm$lat_centroid <- round(log_mike_hm$lat_centroid, 5)
# log_mike_hm <- log_mike_hm[log_mike_hm$effort_hm>0,]


log_mike <- log_mike %>% dplyr::arrange(year, SFA, lon_centroid, lat_centroid, landings_kg, effort_hm) %>%
  dplyr::select(year, SFA, lon_centroid, lat_centroid, landings_kg, effort_hm)

# NOT QUITE RIGHT! NEED TO FIGURE OUT WHY THIS DOESN'T MATCH PLOTS?! Something with joining of hm to kg?
write.csv(x=log_mike, file = "Y:/Offshore/Assessment/2025/Supporting_tasks/Mersey_DR2025_10/logdata_2009-2024_gridded_screened_Science.csv")
