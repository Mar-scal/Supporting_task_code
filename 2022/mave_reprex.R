# mave reprex for Olex and OV comparisons

# for DK testing:
funs <- c("https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Survey_and_OSAC/getdis.r",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Survey_and_OSAC/olex_import.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} 

require(sf)
require(dplyr)
require(tidyverse)
require(plotly)

## looking at how old code works
ovd <- dist.coef(#stringr::str_pad(1:200, width = 3, side = "left", pad = "0"), 
                tows=c(201:300), path="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/LE15BBNlog/",w=c(1:10,9:1),rule=8,smooth=T,plt=F)

# using sf and mave function manually on OVD data
ovd_mave <- NULL
mave1 <- NULL
for(i in c(201:300)){
  sf_use_s2(FALSE)
  towlog <- read.table(paste0("Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/LE15BBNlog/", i, ".log"), skip=5)
  towlog$lon <- as.numeric(gsub(x=towlog$V2, pattern=",", replacement=""))
  towlog$lat <- as.numeric(gsub(x=towlog$V3, pattern=",", replacement=""))
  towlog$mave_lon <- mave(towlog$lon,w=c(1:10,9:1))
  towlog$mave_lat <- mave(towlog$lat,w=c(1:10,9:1))
  towlog <- towlog[seq(1, nrow(towlog), 2),]
  mave1 <- towlog[, c("mave_lon", "mave_lat")] %>%
    st_as_sf(coords=c("mave_lon", "mave_lat"), crs=4326) %>%
    st_transform(32620) %>%
    group_by() %>%
    summarize(do_union=FALSE) %>%
    st_cast("LINESTRING") %>%
    st_transform(4326)
  
  mave1$tow <- i
  mave1$length <- st_length(st_transform(mave1, 32620))
  mave1$dis <- 800/mave1$length
  
  ovd_mave <- rbind(ovd_mave, mave1)
}


# Using sf and mave on OLEX data
olex_mave <- NULL
mave1 <- NULL
tracks <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/GBBBNGERLE15.gz", ntows=212, type="track")
tracks_df <- as.data.frame(st_coordinates(tracks))
for(i in 1:nrow(tracks)){
  sf_use_s2(FALSE)
  mave1 <- tracks_df[tracks_df$L1==i,]
  mave1$POS <- 1:nrow(mave1)
  
  mave1$mave_lon <- mave(mave1$X,w=c(1:10,9:1))
  mave1$mave_lat <- mave(mave1$Y,w=c(1:10,9:1))
  if(nrow(mave1)>2) mave1 <- mave1[seq(1, nrow(mave1), 2),]
  mave1 <- mave1[, c("mave_lon", "mave_lat")] %>%
    st_as_sf(coords=c("mave_lon", "mave_lat"), crs=4326) %>%
    st_transform(32620) %>%
    group_by() %>%
    summarize(do_union=FALSE) %>%
    st_cast("LINESTRING") %>%
    st_transform(4326)
  
  mave1$ID <- i
  mave1$length <- st_length(st_transform(mave1, 32620))
  mave1$dis <- 800/mave1$length
  
  olex_mave <- rbind(olex_mave, mave1)
}

# match olex tows to OV tows to get tow ID
sf_use_s2(FALSE)
tow_match <- st_nearest_feature(ovd_mave, olex_mave)
ovd_mave$ID <- tow_match
joiner <- ovd_mave[,c("tow", "ID")]
st_geometry(joiner) <- NULL
olex_mave <- left_join(olex_mave, joiner)
names(tracks)[1] <- "ID"
tracks <- left_join(tracks, joiner)

ovd_raw <- ovd[[2]] %>%
  st_as_sf(coords=c("X", "Y"), crs=4326) %>%
  group_by(PID) %>%
  summarize(do_union=FALSE) %>%
  st_cast("LINESTRING")


ovd_pts <- ovd[[2]] %>%
  st_as_sf(coords=c("X", "Y"), crs=4326)

olex_pts <- as.data.frame(st_coordinates(tracks)) %>%
  group_by(L1) %>%
  mutate(POS = 1:n(),
         ID=L1) %>%
  left_join(olex_mave) %>%
  ungroup() %>%
  dplyr::select(-geometry) %>%
  group_by(L1) %>%
  st_as_sf(coords=c("X", "Y"), crs=4326) %>%
  filter(!is.na(tow))

olex_mave <- olex_mave[!is.na(olex_mave$tow),]
tracks <- tracks[!is.na(tracks$tow),]

# not every tow was tracked with olex, and I only grabbed the GBa OV logs.
# Change the colours to improve visibility. 
# Zoom way in at the start and end points of a tow to see the difference.
# OVD raw = OVD mave very closely. 
# Olex mave is much shorter than Olex raw. Olex raw is longer than OVD raw.
# change the number from 28 to other tows found in tow_match object to look around, or remove the subset and use plotly to explore
plotly::ggplotly(
  ggplot() + 
    geom_sf(data=tracks[tracks$tow==201,], lwd=4, colour="yellow") + 
    geom_sf(data=ovd_raw[ovd_raw$PID==201,], lwd=3, colour="black") + 
    geom_sf(data=ovd_mave[ovd_mave$tow==201,], lwd=2, colour="blue") +
    geom_sf(data=olex_mave[olex_mave$tow==201,], colour="red") +
    geom_sf_text(data=olex_pts[olex_pts$tow==201,], aes(label=POS)) +
    geom_sf_text(data=ovd_pts[ovd_pts$PID==201,], aes(label=POS), colour="white")
)

plotly::ggplotly(
  ggplot() + 
     geom_sf(data=tracks[tracks$tow==201,], lwd=4, colour="yellow") + 
     geom_sf(data=ovd_raw[ovd_raw$PID==201,], lwd=3, colour="black") + 
    # geom_sf(data=ovd_mave[ovd_mave$tow==29,], lwd=2, colour="blue") +
    # geom_sf(data=olex_mave[olex_mave$tow==29,], colour="red") +
    geom_sf_text(data=olex_pts[olex_pts$tow==201,], aes(label=POS)) +
    geom_sf_text(data=ovd_pts[ovd_pts$PID==201,], aes(label=POS), colour="blue")
)

lengthcomp <- as.data.frame(olex_mave) %>%
  dplyr::select(tow, length) %>%
  dplyr::mutate(length_olex=length) %>%
  dplyr::select(-length) %>%
  left_join(dplyr::select(as.data.frame(ovd_mave), tow, length))

summary(lengthcomp$length_olex-lengthcomp$length)

tracks <- tracks %>%
  st_transform(32620) %>%
  mutate(length=st_length(.)) %>%
  st_transform(4326) 

ovd_raw <- ovd_raw %>%
  st_transform(32620) %>%
  mutate(length=st_length(.)) %>%
  st_transform(4326) %>%
  mutate(tow=PID)

rawlengthcomp <- as.data.frame(tracks) %>%
  dplyr::select(tow, length) %>%
  dplyr::mutate(length_olex=length) %>%
  dplyr::select(-length) %>%
  left_join(dplyr::select(as.data.frame(ovd_raw), tow, length))

summary(rawlengthcomp$length_olex-rawlengthcomp$length)

pt_sum <- olex_pts %>%
  group_by(ID,tow) %>%
  summarize(npoints_olex = max(POS))

pt_sum <- as.data.frame(ovd_pts) %>%
  group_by(PID) %>%
  summarize(npoints_ov = max(POS)) %>%
  mutate(tow=PID) %>%
  left_join(as.data.frame(pt_sum))
# Olex is polling more frequently, and ov often has 189 records? Huh? Why...

summary(pt_sum$npoints_ov)
summary(pt_sum$npoints_olex)
ggplot() + geom_histogram(data=pt_sum, aes(npoints_ov), binwidth=1)
ggplot() + geom_histogram(data=pt_sum, aes(npoints_olex), binwidth=1)

pts_olex <- left_join(olex_mave, data.frame(tow=pt_sum$tow, npoints_olex=pt_sum$npoints_olex))
pts_tracks <- left_join(tracks, data.frame(tow=pt_sum$tow, npoints_olex=pt_sum$npoints_olex))
pts_tracks$length <- st_length(pts_tracks)
pts_ov <- left_join(ovd_mave, data.frame(tow=pt_sum$tow, npoints_ov=pt_sum$npoints_ov))


olex_comp <- full_join(data.frame(tow=pts_tracks$tow, raw_length = pts_tracks$length, npoints_olex=pts_tracks$npoints_olex),
                       data.frame(tow=pts_olex$tow, smooth_length = pts_olex$length, npoints_olex=pts_olex$npoints_olex))

olex_comp$diff <- olex_comp$raw_length - olex_comp$smooth_length

ggplot() + geom_point(data=pts_olex, aes(as.numeric(length), npoints_olex))
ggplot() + geom_point(data=pts_tracks, aes(as.numeric(length), npoints_olex))
ggplot() + geom_point(data=pts_ov, aes(as.numeric(length), npoints_ov))
ggplot() + geom_point(data=olex_comp, aes(as.numeric(diff), npoints_olex))


# CHECK ALL DISTANCE COEFFICIENTS!
source("C:/Users/keyserf/Documents/Github/Assessment_fns/Other_functions/ScallopQuery.R")
dis_hist <- ScallopQuery("ROracle", db.con="ptran", SQLtext = "Select * from SCALOFF.OSTOWS")
head(dis_hist)
require(lubridate)
dis_hist$year <- year(ymd_hms(dis_hist$TOW_DATE))

dis_hist_means <- dis_hist %>%
  group_by(year) %>%
  summarize(mean_dis_coef = mean(DIS_COEF),
            sd_dis_coef = sd(DIS_COEF))

ggplot() + geom_point(data=dis_hist, aes(ymd_hms(TOW_DATE), DIS_COEF, colour=as.factor(TECH_ID), shape=as.factor(DIS_COEF_ID)))

ggplot() + geom_point(data=dis_hist[dis_hist$year==2016,], aes(ymd_hms(TOW_DATE), DIS_COEF, colour=as.factor(TECH_ID), shape=as.factor(DIS_COEF_ID)), alpha=0.5)

ggplot() + geom_boxplot(data=dis_hist, aes(year, DIS_COEF, group=year), outlier.shape=NA) + ylim(0.3,1.3)

ggplot() + geom_point(data=dis_hist_means, aes(year, mean_dis_coef))

# GPS could be off by 1 hundredth. Olex has a separate feed (not a splitter). 
# We had a freezing issue last year when we tried to use a splitter.
# TPD has nautical miles for each tow in Olex.
# speeds varied between systems too
# Slight difference in vessel location between olex and OV, noticed when "on station"
# OV starts first, timer, then olex
# 3 second intervals. TPD to check olex polling frequency.
# warp length (fathoms)

# GBMon olex stations are on average 15m longer than OVD stations
# Polling frequency is different because we log
