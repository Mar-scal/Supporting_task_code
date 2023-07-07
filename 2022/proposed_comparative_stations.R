

stns <- read.csv(paste("Y:/Offshore/Assessment/Data/Survey_data/fixed_station_banks_towlst.csv",sep=""))
stns <- stns[stns$Bank=="GB",]

stns <- st_as_sf(stns, coords=c(X="X", Y="Y"), crs=4326)

source("./Maps/convert_coords.R")
source("./Maps/pectinid_projector_sf.R")

ofi <- st_read("Y:/Projects/OFI/BEcoME/Data/WP3/share_w_DalClearwater/Survey_locations.shp")


p <- pecjector(area="NL", add_layer = list(bathy=list(100, "c", 200)))

png(filename = "Y:/Projects/OFI/BEcoME/Data/WP3/share_w_DalClearwater/GB_fixed_monitoring_survey.png")
p + geom_sf(data=ofi, shape=2) +
  geom_sf(data=stns, shape=1) + 
  theme_bw() + 
  scale_x_continuous(limits=c(-67, -65.5)) +
  scale_y_continuous(limits=c(41.21, 42.3)) +
  coord_sf(expand=F) +
  ggtitle("Georges Bank OFI stations (triangles) relative\nto fixed monitoring stations (circles)")
dev.off()
