# Coordinates provided by Dvora Hart, note the slight modificaiton by DK to align CAII with
# EEZ used by Canada.  Unclear what coordinate system these are in, but seems like
# NOAA likes NAD83 (with GRS80) for up this way so that's what I'm going with.
library(sf)
library(tidyverse)

source("D:/Github/Offshore/Assessment_fns/DK/Maps/pectinid_projector_sf.R")
CA1=data.frame(lon =  -1*c(69.3833, 68.5, 68.5, 68.75), lat=c(41.5, 41.5, 40.75, 40.75)  )
# Note that DK edited the second longitude to be 66.43 from 66.413 as 66.413 appears to be inside Canada
# while 66.43 falls exactly on the ICJ line that we use.  
# See here for CAII which agree with what Dvora sent.
#https://www.fisheries.noaa.gov/new-england-mid-atlantic/commercial-fishing/northeast-multispecies-closed-area-regulations-georges#georges-bank-dedicated-habitat-research-area,-closed-area-ii,-and-great-south-channel-habitat-management-area
CA2=data.frame(lon= -1*c(67.333, 66.43, 66.59667, 67.333), lat = c(42.3667, 41.31, 41.0, 41.0)  )

CA1.sf <- st_as_sf(CA1,coords = c("lon",'lat'), crs = 4269)
CA1.sf <- st_cast(st_combine(CA1.sf),"POLYGON")
CA2.sf <- st_as_sf(CA2,coords = c("lon",'lat'), crs = 4269)
CA2.sf <- st_cast(st_combine(CA2.sf),"POLYGON")

pecjector(area = "GOM",add_layer= list(eez = 'eez'),c_sys = 4269) + geom_sf(data = CA2.sf, fill = NA) + geom_sf(data = CA1.sf)

# now I want to save these are shapefiles...
st_write(CA1.sf,dsn ="d:/github/Offshore/GIS_layers/DK/other_boundaries/CA1.shp")
st_write(CA2.sf,dsn = "d:/github/Offshore/GIS_layers/DK/other_boundaries/CA2.shp")

# So this takes care of CA1 and CA2, now I want to make cod and yellowtail shapefiles
# with the year attached to each multipolygon which will make using these 
# much easier for later analyses...

load(paste0("D:/NAS/Projects/GB_time_area_closure_SPERA/Results/Results_for_paper_all_years.RData"))


# Bind the cod and yellowtail closures into one multipolygon, this will be easier for later...
yt.sf <- aggregate(yt.closures, list(yt.closures$year), function(x) x[1])
cod.sf <- aggregate(cod.closures, list(cod.closures$year), function(x) x[1])

st_write(yt.sf,dsn ="d:/github/Offshore/GIS_layers/DK/other_boundaries/yt_closures.shp")
st_write(cod.sf,dsn = "d:/github/Offshore/GIS_layers/DK/other_boundaries/cod_closures.shp")
