# DR2024_11 - Offshore survey polygons for Kasia with Marine Planning and Conservation

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")
boundaries <- github_spatial_import(subfolder="survey_boundaries", "survey_boundaries.zip")

plot(boundaries[boundaries$ID == "Sab.shp",])

strata <- github_spatial_import(subfolder="offshore_survey_strata", "offshore_survey_strata.zip")

require(sf)
require(ggplot2)
ggplot()+ geom_sf(data=boundaries[boundaries$ID=="Sab.shp",]) + geom_sf(data=st_union(strata[strata$ID == "Sab.shp_",]))

Sab <- st_union(strata[strata$ID == "Sab.shp_",])
ggplot() + geom_sf(data=Sab)
st_write(obj=Sab, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/Sab.shp")

BBn <- st_union(strata[strata$ID == "BBn.shp_",])
ggplot() + geom_sf(data=BBn, fill="blue", alpha=0.5)+ geom_sf(data=boundaries[boundaries$ID == "BBn.shp",], fill="red", alpha=0.5)
st_write(obj=BBn, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/BBn.shp")

BBs <- st_union(strata[strata$ID == "BBs.shp_",])
ggplot() + geom_sf(data=BBs, fill="blue", alpha=0.5)+ geom_sf(data=boundaries[boundaries$ID == "BBs.shp",], fill="red", alpha=0.5)
st_write(obj=BBs, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/BBs.shp")

sf_use_s2(FALSE)
GBa <- st_union(strata[strata$ID == "GBa.shp_",])
ggplot() + geom_sf(data=GBa, fill="blue", alpha=0.5)+ geom_sf(data=boundaries[boundaries$ID == "GBa.shp",], fill="red", alpha=0.5)
st_write(obj=GBa, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/GBa.shp")

sf_use_s2(TRUE)
plot(st_read("Y:/Offshore/Data requests/2024/DR2024_11_MPC/GBa.shp"))

GBb <- st_union(strata[strata$ID == "GBb.shp_",])
ggplot() + geom_sf(data=GBb, fill="blue", alpha=0.5)+ geom_sf(data=boundaries[boundaries$ID == "GBb.shp",], fill="red", alpha=0.5)
st_write(obj=GBb, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/GBb.shp")

ggplot() + geom_sf(data=boundaries[boundaries$ID == "0",], fill="red", alpha=0.5)
# add the SE triangle area, even though its just extras. what a pain!!!
off <- github_spatial_import(subfolder="offshore", "offshore.zip")
lon1 <- max(as.data.frame(st_coordinates(off[off$ID=="SFA26C.shp",]))$X)
lat1 <- max(as.data.frame(st_coordinates(off[off$ID=="SFA26C.shp",]))[as.data.frame(st_coordinates(off[off$ID=="SFA26C.shp",]))$X==lon1,]$Y)
pt <- st_as_sf(data.frame(lon1, lat1), coords=c("lon1", "lat1"), crs=4326)
points <- boundaries[boundaries$ID=="0",] %>% st_cast("POINT")
points$ID <- 1:nrow(points)
ggplot() + geom_sf_text(data=points, aes(label=ID))
points[points$ID==36,]$geometry <- st_sfc(st_point(c(lon1, lat1)), crs=4326)
ggplot() + geom_sf_text(data=points, aes(label=ID))
points <- rbind(points, points[points$ID==1,])
points[points$ID==1,][2,]$ID <- 37
ggplot() + geom_sf_text(data=points, aes(label=ID))
ger <- points %>% dplyr::group_by() %>% dplyr::summarize(do_union=F) %>% dplyr::ungroup() %>% st_cast("POLYGON")
ger$ID <- "Ger"
st_write(obj=ger, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/Ger.shp", append = F)

ggplot() + geom_sf(data=boundaries[boundaries$ID == "Ban.shp",], fill="red", alpha=0.5)
st_write(obj=boundaries[boundaries$ID == "Ban.shp",], "Y:/Offshore/Data requests/2024/DR2024_11_MPC/Ban.shp")

fixed <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/fixed_station_banks_towlst.csv")
Mid <- st_as_sf(fixed[fixed$Bank=="Mid",], coords=c("X", "Y"), crs=4326)
Mid <- dplyr::select(Mid, EID, Bank, geometry)
st_write(obj=Mid, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/MidStations.shp")

GB <- st_as_sf(fixed[fixed$Bank=="GB",], coords=c("X", "Y"), crs=4326)
GB <- dplyr::select(GB, EID, Bank, geometry)
st_write(obj=GB, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/GBStations.shp")

star <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/Extra_stations.csv")
star <- st_as_sf(star[star$bank=="Sab" & star$year==2024,], coords=c("lon", "lat"), crs=4326)
star <- dplyr::select(star, tow, bank, geometry)
st_write(obj=star, "Y:/Offshore/Data requests/2024/DR2024_11_MPC/StarboxStations.shp")
