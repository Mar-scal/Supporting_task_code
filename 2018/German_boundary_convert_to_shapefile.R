# QUick little script to convert the German detailed strata to a shapefile using sp and maptools

library(PBSmapping)
library(maptools)
library(rgdal)

direct <- "Y:/Offshore scallop/Assessment/Data/Maps/approved/Survey/"

bounds <- read.csv(paste0(direct,"survey_boundary_polygons.csv"))

# Subset to German
ger.bounds <- bounds[bounds$label == "Ger",]
# Make it a PBSmapping object
ger.bounds <- as.PolySet(ger.bounds,projection = "LL")
ger.bounds$PID <-1 # Switch this to 1 or this is a pain later as it tries to match the PID with 
# rowname of the fake "Data"
# Now make that into a shape file, by default this is a WGS84 projection...
ger.bounds <- PolySet2SpatialPolygons(ger.bounds)
ger.nad83 <- ger.bounds
ger.wgs84 <- ger.bounds
#But maybe the data were actually in NAD83, I don't have a clue, this is 
# is not a reprojection of the data it is simply saying the data is NAD83 and not WGS-84
proj4string(ger.nad83) <- CRS("+proj=longlat +datum=NAD83")                               

# Now make a shapefile
p.df <- NA
ger.nad83 <- SpatialPolygonsDataFrame(ger.nad83,data=data.frame(1))
ger.wgs84 <- SpatialPolygonsDataFrame(ger.wgs84,data=data.frame(1))
# Now I think this will convert us to NAD83
#ger.wgs84 <- spTransform(ger.bounds, CRS("+proj=longlat +datum=WGS84" ))
#ger.nad83 <- spTransform(ger.bounds, CRS("+proj=longlat +datum=nad83" ))

writeOGR(ger.wgs84,driver="ESRI Shapefile",dsn = paste0(direct,"German_WGS_84"),
                                                        layer = "WGS_84_German")
writeOGR(ger.nad83,driver="ESRI Shapefile",dsn = paste0(direct,"German_NAD_83"),
         layer = "NAD_83_German")

