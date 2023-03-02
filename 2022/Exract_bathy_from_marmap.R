# A function to pull in a save marmap object so we don't have to keep querying the NOAA database.

library(marmap)

# define a boundary area, here I'm going for basically all of NW Atl Shelf 
bath.box <- data.frame(xmin = -80,ymin=35,xmax = -35,ymax=60)
# note that the bathy is basically a EPSG:4326
bathy.org <- getNOAA.bathy(lon1 = bath.box$xmin ,bath.box$xmax,lat1 = bath.box$ymin,lat2=bath.box$ymax,resolution =1)
# Convert this to a raster
bathy <- marmap::as.raster(bathy.org)
# We can save this raster and I'll put it up in our GIS repo...
saveRDS(bathy, file = "D:/Github/GIS_layers/bathymetry/NW_Atl_bathy_raster_from_NOAA_marmap.Rds")
# That'll do...
plot(bathy)

