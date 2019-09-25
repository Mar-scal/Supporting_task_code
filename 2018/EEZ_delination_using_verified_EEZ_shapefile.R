library(sp)
library(maptools)
library(rgeos)
library(rgdal)
library(raster)
library(PBSmapping)
# This is slow!!
eez <- readOGR("Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/world_eez_boundary")

# epsg:4326 is Lat/Long and WGS84 specification, the EPSG thing is the "European Petroleum Survey Group" who put together a database of all the
# coordinate referece systems.  
# Other important ones Lat/Lon with NAD 83 = EPSG:4269, NAD 27 = EPSG:4267
# THe UTM zone we will mostly be dealing with is UTM 19 EPSG:32619 (Basically GOM and GB, also SPA3,6 and most of SFA 29) 
# or 20 EPSG:32620 (Most of Scotian shelf and most of the BoF)
# This is lat/lon and WGS84
utm.prj4s <-CRS("+init=epsg:32619")
# A box representing most of eastern Canada
atl.box <- as(raster::extent(-71,-47,38,60), "SpatialPolygons")
proj4string(atl.box) = proj4string(eez)
tl.box <- spTransform(atl.box,utm.prj4s)

atl.can.eez <- gIntersection(eez,atl.box)
atl.pbs.eez <- SpatialLines2PolySet(atl.can.eez)
eez <- atl.can.eez
write.csv(atl.pbs.eez,file="Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/atl_can_eez/atl_can_eez.csv")
save(eez,file="Y:/Offshore scallop/Assessment/Data/Maps/approved/TO DO - Good map products to incorporate/atl_can_eez/atl_can_eez.RData")

