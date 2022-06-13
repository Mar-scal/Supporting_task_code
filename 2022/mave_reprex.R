# mave reprex for Olex and OV comparisons

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

sf_use_s2(FALSE)
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
# change the number from 28 to other tows found in tow_match object to look around, or remove the subset and use plotly to explore
plotly::ggplotly(
  ggplot() + 
    geom_sf(data=tracks[tracks$tow==28,], lwd=4, colour="yellow") + 
    geom_sf(data=ovd_raw[ovd_raw$PID==28,], lwd=3, colour="black") + 
    geom_sf(data=ovd_mave[ovd_mave$tow==28,], lwd=2, colour="blue") +
    geom_sf(data=olex_mave[olex_mave$tow==28,], colour="red")
)
