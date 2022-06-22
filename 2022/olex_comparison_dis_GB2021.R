
## To source functions from local directory:
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"
source(paste0(direct_fns, "Survey_and_OSAC/olex_import.R"))
#source(paste0(direct_fns, "Survey_and_OSAC/olex_check_strata.R"))
source(paste0(direct_fns, "Survey_and_OSAC/getdis.R"))
#source(paste0(direct_fns, "Survey_and_OSAC/convert.dd.dddd.R"))

# for DK testing:
funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/getdis.r",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Survey_and_OSAC/olex_import.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} 

## looking at how old code works
ovd <- dist.coef(stringr::str_pad(1:200, width = 3, side = "left", pad = "0"), path="Y:/Offshore/Assessment/Data/Survey_data/2021/Database loading/LE14/GBalog/",w=c(1:10,9:1),rule=8,smooth=T,plt=F)

# using sf and mave function manually on OVD data
ovd_mave <- NULL
mave1 <- NULL
for(i in stringr::str_pad(1:200, width = 3, side = "left", pad = "0")){
  sf_use_s2(FALSE)
  towlog <- read.table(paste0("Y:/Offshore/Assessment/Data/Survey_data/2021/Database loading/LE14/GBalog/", i, ".log"), skip=5)
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
tracks <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2021/Database loading/LE14GBtesttracks.gz", ntows=109, type="track")
tracks_df <- as.data.frame(st_coordinates(tracks))
# ignore tow 1 because it was a do-over, and you don't want to accidentally use it as an example
for(i in 2:nrow(tracks)){
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

tow_match <- st_nearest_feature(olex_mave, ovd_mave)
olex_mave$tow <- tow_match
tracks$tow <- c(NA,tow_match) # just because I know the first tow was crap
ovd_mave$tow <- 1:nrow(ovd_mave)

ovd_raw <- ovd[[2]] %>%
  st_as_sf(coords=c("X", "Y"), crs=4326) %>%
  group_by(PID) %>%
  summarize(do_union=FALSE) %>%
  st_cast("LINESTRING")

# not every tow was tracked with olex, and I only grabbed the GBa OV logs.
# Change the colours to improve visibility. 
# Zoom way in at the start and end points of a tow to see the difference.
# OVD raw = OVD mave very closely. 
# Olex mave is much shorter than Olex raw. Olex raw is longer than OVD raw. 
plotly::ggplotly(
  ggplot() + 
    geom_sf(data=tracks[tracks$tow==28,], lwd=4, colour="yellow") + 
    geom_sf(data=ovd_raw[ovd_raw$PID==28,], lwd=3, colour="black") + 
    geom_sf(data=ovd_mave[ovd_mave$tow==28,], lwd=2, colour="blue") +
    geom_sf(data=olex_mave[olex_mave$tow==28,], colour="red")
)


names(ovd_mave) <- c("geometry", "tow", "mavelength", "mavedis")
dis_comp <- left_join(ovd_mave, ovd[[1]])

dis_comp$chk <- dis_comp$mavelength/(dis_comp$length*1000)
summary(dis_comp$chk)
oddballs <- dis_comp[which(as.numeric(dis_comp$chk) > 1.2),]
# no oddballs!

# plotly::ggplotly(ggplot() + geom_sf(data=oddballs) + 
#   geom_sf_text(data=oddballs, aes(label=tow)))#+
#  #geom_sf(data=oddballs_pbs, colour="red"))

ggplot() + geom_point(data=dis_comp, aes(as.numeric(mavelength), length*1000)) +
  geom_abline(aes(slope=1, intercept=0))
summary(dis_comp$chk)

orig <- ovd[[2]] %>%
  st_as_sf(coords=c("X", "Y"), crs=4326) %>%
  group_by(PID) %>%
  summarize(do_union=FALSE) %>%
  st_cast("LINESTRING")

plotly::ggplotly(ggplot() + geom_sf(data=orig) + 
  geom_sf(data=mave_d, colour="red"))

# pbs <- as.PolySet(data.frame(PID=1, POS=1:nrow(l001), X=l001$mave_lon, Y=l001$mave_lat))
# attr(pbs,"projection")<-"LL"
# pbs <- as.PolySet(pbs)


require(ggplot2)
ggplot() + geom_point(data=l001[c(1, nrow(l001)),], aes(lon, lat)) +
  geom_point(data=l001[c(1, nrow(l001)),], aes(mave_lon, mave_lat), shape=3)
# the more values you include in the moving average, the shorter the tow becomes


# trying new code
olex <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2021/Database loading/LE14GBtesttracks.gz", ntows=109, type="load", length="sf", every_n=2, w=c(1:10,9:1))
#olex_pbs <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2021/Database loading/LE14GBtesttracks.gz", ntows=109, type="load", length="PBSmapping")

tracks <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2021/Database loading/LE14GBtesttracks.gz", ntows=109, type="track")

# SELECT to_char(tow_date, 'yyyy') year, cruise,
# survey_name, area_cd, tow_no, tow_type_id, mgt_area_cd, strata_id, tow_date,
# bottom_temp, depth_f, start_lat, start_lon, end_lat, end_lon, dis_coef, bearing
# FROM ossurveys o,
# ostows t
# where o.survey_seq = t.survey_seq
# and o.cruise like 'LE14'
dis <- read.csv("C:/Users/keyserf/Documents/LE14_dis.csv")

require(tidyverse)
require(sf)
dis_gb <- dis %>%
  filter(MGT_AREA_CD %in% c("GBa", "GBb"))

dis_gb$START_LON_dd <- convert.dd.dddd(dis_gb$START_LON, "dec.deg")
dis_gb$START_LAT_dd <- convert.dd.dddd(dis_gb$START_LAT, "dec.deg")
dis_gb$END_LON_dd <- convert.dd.dddd(dis_gb$END_LON, "dec.deg")
dis_gb$END_LAT_dd <- convert.dd.dddd(dis_gb$END_LAT, "dec.deg")


dis_gb_start <- dis_gb %>%
  st_as_sf(coords=c("START_LON_dd", "START_LAT_dd"), crs=4326) %>%
  dplyr::select(TOW_NO, DIS_COEF, BEARING) %>%
  mutate(pos = "start")

dis_gb_end <- dis_gb %>%
  st_as_sf(coords=c("END_LON_dd", "END_LAT_dd"), crs=4326) %>%
  dplyr::select(TOW_NO, DIS_COEF, BEARING) %>%
  mutate(pos = "end")

ov2021 <- rbind(dis_gb_start, dis_gb_end)

# connect the points to create tracks by summarizing based on tow number. 
# Each track is a linestring, so the target object is a "multilinestring"
ov2021 <- ov2021 %>% 
  group_by(TOW_NO, DIS_COEF, BEARING) %>% 
  summarize() %>% 
  st_cast("MULTILINESTRING")


# Need an ID column instead of tow numbering
ov2021 <- arrange(ov2021, TOW_NO)
ov2021$ID <- 1:nrow(ov2021)

# this tells you the ID of the nearest feature - not the same as tow number!
track <- arrange(tracks, tow)
track$ID <- st_nearest_feature(track, ov2021)

# have to use track because it's an sf object. But it's in the same order, so can assign to olex dataframe
olex <- arrange(olex, tow)
olex$ID <- track$ID

# join the ov2021 data to olex, based on the ID column
olex_j <- left_join(olex, ov2021)

# look for weird tows and remove them
st_length(track) # remove 1, 5, 18, 46, 57, 109
which(as.numeric(st_length(track))>1500)
which(as.numeric(st_length(track))<100)
track <- track[!track$tow %in% c(1, 5, 18, 46, 109),]
olex_j <- olex_j[!olex_j$tow %in% c(1, 5, 18, 46, 109),]

# should only contain 1's (each ID should only have matched to 1 olex tow)
table(olex_j$TOW_NO)

plotly::ggplotly(ggplot() + geom_sf(data=track, lwd=1) +
  geom_sf(data=ov2021, colour="blue"))

plotly::ggplotly(ggplot() + geom_sf(data=track[track$ID==164,], lwd=1) +
                   geom_sf(data=ov2021[ov2021$ID==164,], colour="blue"))

# start comparing!
summary(olex_j$dis_coef)
summary(olex_j$DIS_COEF)

summary(olex_j$dis_coef / olex_j$DIS_COEF)

ggplot() + 
  geom_segment(data=olex_j, aes(x=ID, xend=ID, y=as.numeric(dis_coef), yend=as.numeric(DIS_COEF)))+
  geom_point(data=olex_j, aes(ID, as.numeric(dis_coef)), shape=1) + 
  geom_point(data=olex_j, aes(ID, as.numeric(DIS_COEF)), shape=2) 

ggplot() + 
  geom_text(data=olex_j, aes(x=as.numeric(dis_coef), y=as.numeric(DIS_COEF), label=ID)) + 
  geom_abline(data=olex_j, aes(slope=1,intercept=0)) + 
  xlab("olex dis_coef") + 
  ylab("ovd dis_coef")

ggplot() + geom_boxplot(data=olex_j, aes(as.numeric(dis_coef)), alpha=0.5, fill="yellow") +
  geom_boxplot(data=olex_j, aes(as.numeric(DIS_COEF)), alpha=0.25, fill="blue") + theme_bw()

#ovd distance coefficients are regularly higher than the olex ones at c(1:10, 9:1)

ov2021[166,]
track[track$ID==166,]
ggplot() + geom_sf(data=ov2021[ov2021$ID==28,], colour="red", lwd=2) +
  geom_sf(data=track[track$ID==28,])

