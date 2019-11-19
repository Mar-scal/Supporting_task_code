# load in georges bank scallop survey model output (from super coarse spatial resolution model)
load("C:/Documents/Offshore scallop/Assessment/Data/Survey_data/2019/Survey_summary_output/GB_figures_res_25-25.RData")

# check out the different parts of the inla data
names(proj)
str(proj)
proj$x # these are the longitudes
proj$y # these are the latitudes

mod.res$`FR-spatial` # INLA matrix (raster) of modelled "fully-recruited" scallop abundances (aka. scallops big enough to be fished) 
class(mod.res$`FR-spatial`) # note it's a matrix, not a "raster" object
image(mod.res$`FR-spatial`) # a super quick visualization

# can we turn it into a raster object
require(raster)
rasterobj <- raster(list(x=proj$x, y=proj$y, z=mod.res$`FR-spatial`), # specify the data. make sure it's in this list format 
                                                                        # or else the conversion from matrix to raster goes bad
                    crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") # assign the spatial projection (WGS84, with lat/lon coordinates)

image(rasterobj) # now we have a raster object with our coordinates!

class(rasterobj) 

