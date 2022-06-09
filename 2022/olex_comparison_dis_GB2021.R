
## To source functions from local directory:
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"
source(paste0(direct_fns, "Survey_and_OSAC/olex_import.R"))
source(paste0(direct_fns, "Survey_and_OSAC/olex_check_strata.R"))
source(paste0(direct_fns, "Survey_and_OSAC/getdis.R"))
source(paste0(direct_fns, "Survey_and_OSAC/convert.dd.dddd.R"))

## looking at how old code works

# ovd <- dist.coef(301:324,path="Y:/People/Alan/LE09/LE09GBMonlogs/",w=c(1:10,9:1),rule=8,smooth=T,plt=F)
# 
sf_use_s2(FALSE)
l301 <- read.table("Y:/People/Alan/LE09/LE09GBMonlogs/301.log", skip=5)
l301$lon <- as.numeric(gsub(x=l301$V2, pattern=",", replacement=""))
l301$lat <- as.numeric(gsub(x=l301$V3, pattern=",", replacement=""))
l301$mave_lon <- mave(l301$lon,w=c(1:10,9:1))
l301$mave_lat <- mave(l301$lat,w=c(1:10,9:1))

ggplot() + geom_point(data=l301[c(1, nrow(l301)),], aes(lon, lat)) +
  geom_point(data=l301[c(1, nrow(l301)),], aes(mave_lon, mave_lat), shape=3)
# the more values you include in the moving average, the shorter the tow becomes


# trying new code
olex <- olex_import(filename="Y:/Offshore/Assessment/Data/Survey_data/2021/Database loading/LE14GBtesttracks.gz", ntows=109, type="load", length="sf", every_n=2, w=c(1:15,14:1))
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

dis_gb <- dis %>%
  filter(MGT_AREA_CD %in% c("GBa", "GBb"))

dis_gb$START_LON_dd <- convert.dd.dddd(dis_gb$START_LON, "dec.deg")
dis_gb$START_LAT_dd <- convert.dd.dddd(dis_gb$START_LAT, "dec.deg")
dis_gb$END_LON_dd <- convert.dd.dddd(dis_gb$END_LON, "dec.deg")
dis_gb$END_LAT_dd <- convert.dd.dddd(dis_gb$END_LAT, "dec.deg")


dis_gb_start <- dis_gb %>%
  st_as_sf(coords=c("START_LON_dd", "START_LAT_dd"), crs=4326) %>%
  select(TOW_NO, DIS_COEF, BEARING) %>%
  mutate(pos = "start")

dis_gb_end <- dis_gb %>%
  st_as_sf(coords=c("END_LON_dd", "END_LAT_dd"), crs=4326) %>%
  select(TOW_NO, DIS_COEF, BEARING) %>%
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
track <- track[!track$tow %in% c(1, 5, 18, 46, 57, 109),]
olex_j <- olex_j[!olex_j$tow %in% c(1, 5, 18, 46, 57, 109),]

# should only contain 1's (each ID should only have matched to 1 olex tow)
table(olex_j$TOW_NO)

plotly::ggplotly(ggplot() + geom_sf(data=track, lwd=1) +
  geom_sf(data=ov2021, colour="blue"))

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

#olex distance coefs are consistently higher than ovd distance coefs

ov2021[166,]
track[track$ID==166,]
ggplot() + geom_sf(data=ov2021[ov2021$ID==28,], colour="red", lwd=2) +
  geom_sf(data=track[track$ID==28,])

