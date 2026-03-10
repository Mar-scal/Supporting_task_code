# sable survey vs. fishery

load("C:/Users/keyserf/Documents/temp_data/Data/Fishery_data/Summary/2025/OSAC_tidy_logs.RData")

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")
strata <- github_spatial_import(subfolder = "survey_boundaries", zipname = "survey_boundaries.zip")
strata <- strata[strata$ID=="Sab.shp",]

require(sf)
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish.dat <- fish.dat[!is.na(fish.dat$lat),]
fish_sf <- st_as_sf(fish.dat, coords=c("lon", "lat"), crs=4326)

require(tidyverse)
summarized <- fish_sf %>% 
  dplyr::filter(bank=="Sab") %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(nwatches=n(),
                   totalcatch = sum(pro.repwt, na.rm=T),
                   totaleffort = sum(hm, na.rm=T)) %>%
  mutate(strata="null")
st_geometry(summarized) <- NULL

fish_sf2 <- st_intersection(fish_sf, strata)

summarized2 <- fish_sf2 %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarize(nwatches=n(),
                   totalcatch = sum(pro.repwt, na.rm=T),
                   totaleffort = sum(hm, na.rm=T)) %>%
  mutate(strata="inside")

st_geometry(summarized2) <- NULL

summarized <- full_join(summarized, summarized2)

ggplot() + geom_point(data=summarized, aes(year, nwatches, colour=strata)) 
ggplot() + geom_point(data=summarized, aes(year, totalcatch, colour=strata)) 
ggplot() + geom_point(data=summarized, aes(year, totaleffort, colour=strata)) 

summarized2 <- pivot_wider(summarized, names_from = strata, values_from=c(nwatches, totalcatch, totaleffort))
summarized2$prop_nwatches <- summarized2$nwatches_inside/summarized2$nwatches_null
summarized2$prop_totalcatch <- summarized2$totalcatch_inside/summarized2$totalcatch_null
summarized2$prop_totaleffort <- summarized2$totaleffort_inside/summarized2$totaleffort_null

ggplot() + geom_line(data=summarized2, aes(year, prop_nwatches))
ggplot() + geom_line(data=summarized2, aes(year, prop_totalcatch)) + theme_bw() + ylab("Proportion of Sab landings inside strata")
ggplot() + geom_line(data=summarized2, aes(year, prop_totaleffort))

summary(summarized2[summarized2$year>2010,]$prop_totalcatch)
