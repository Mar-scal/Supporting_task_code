# Quick script to look at impact of Eastern Canyons Conservation Area overlap with Scallop fishery.
yr <- 2020
direct = "d:/github/Offshore/Assessment_fns/DK/"
#direct = "Y:/Offshore scallop/Assessment/"
source(paste(direct,"Maps/pectinid_projector_sf.R",sep=""))
source(paste(direct,"Fishery/logs_and_fishery_data.r",sep=""))
source(paste(direct,"Survey_and_OSAC/convert.dd.dddd.r",sep=""))
library(PBSmapping)
require(rgdal)
library(maptools)
library(sf)
# Read SHAPEFILE.shp from the current working directory (".")

# Bring in the eastern canyons 
ecca <- read.csv("Z:/Maps/AOIs/Eastern_Canyons_AOI/ecca_coords.csv")
ecca <- as.PolySet(ecca,projection = "LL")
ecca <- PolySet2SpatialPolygons(ecca,close_polys = T)
proj4string(ecca) <- CRS("+proj=longlat +datum=NAD83") # According to RM
ecca.sf <- st_as_sf(ecca)
st_write(ecca.sf,dsn = "Z:/Maps/AOIs/Eastern_Canyons_AOI/ecca.shp")

# Bring in the fundian channel shapefile directly, note these data are in NAD83
fund.chan <- st_read("Z:/Maps/AOIs/Fundian_Channel_AOI/FundianChannel_BrownsBank_AOI_poly.shp")
# Easter shore
east.shore <- st_read("Z:/Maps/AOIs/Eastern_shore_AOI/esi_poly.shp")
# The shapefile for the larger MPA network...
mpa.network <- st_read("Z:/Maps/AOIs/MPA_network/SS_MPAN_draftdesign.shp")
mpa.network <- st_simplify(mpa.network,dTolerance = 0.00001) # Clean up the shapefile to get rid of 'self intersections'

# Grab the fishery data. This is just for 2020 and offshore...
logs_and_fish(loc="offshore",year = 2017:yr,direct="D:/NAS/Offshore/Assessment/",direct_fns = direct,un = un.ID,pw = pwd.ID)
#fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat <- new.log.dat
fish.dat$ID<-1:nrow(fish.dat)
fish.dat.sf <- st_as_sf(fish.dat,coords = c("lon","lat"),remove = F,crs = 4269)
ban.dat <- fish.dat %>% dplyr::filter(sfa == "25B") # 25B is Banquereau.
ban.dat.sf <- st_as_sf(ban.dat,coords = c("lon","lat"),remove = F,crs = 4269)
fun.dat <- fish.dat %>% dplyr::filter(sfa %in% c("26A","26B")) # 26A + B = Browns
fun.dat.sf <-st_as_sf(fun.dat,coords = c("lon","lat"),remove = F,crs = 4269)
# Now grab the inshore fishery data...
logs_and_fish(loc="inshore",year = 2017:yr,direct="D:/NAS/Offshore/Assessment/",direct_fns = direct,un = un.ID,pw = pwd.ID,get.marfis = T)
# Inshore data needs converted to lat/lons
log.dat.in$lon <- -convert.dd.dddd(log.dat.in$lon)
log.dat.in$lat <- convert.dd.dddd(log.dat.in$lat)
log.dat.in <- log.dat.in %>% dplyr::filter(!is.na(lon),!is.na(lat))
# Get rid of outliers...
in.bound <- st_as_sf(data.frame(lat=c(43,43,47.9,47.9),lon=c(-67.0,-59.00,-59.00,-67.0)),coords = c('lon','lat'),crs = 4269)
in.bound <- st_cast(st_combine(in.bound),"POLYGON")
in.dat.sf <- st_as_sf(log.dat.in,coords = c("lon","lat"),remove = F,crs = 4269)
in.dat.sf <- st_intersection(in.dat.sf,in.bound)
in.29E.dat <- in.dat.sf %>% dplyr::filter(sfa.marfis == "EOB")
inshore.dat <- in.dat.sf %>% dplyr::filter(sfa.marfis != "EOB")


# First the ECCA area
Ban <- pecjector(area = "Ban",c_sys = 4269, 
          add_layer = list(land = 'grey',eez='eez',sfa = 'offshore',survey = c('offshore','outline')),
          add_custom = list(obj = ecca.sf))
Ban.fish <- Ban + geom_sf(data=fish.dat.sf) + ggtitle("Scallop fishery 2017- August 2020")

# Next up the Fundian Channel.
Fun <- pecjector(area = "WSS",c_sys = 4269, 
                 add_layer = list(land = 'grey',eez='eez',sfa = 'offshore',survey = c('offshore','outline')),
                 add_custom = list(obj = fund.chan))
Fun.fish <- Fun + geom_sf(data=fun.dat.sf) + ggtitle("Scallop fishery 2017- June 2020")


# Here's where the Eastern Shore is...
S29E <- pecjector(area = list(y=c(43,47.9),x=c(-66.00,-59.00),crs = 4269),c_sys = 4269, 
                 add_layer = list(land = 'grey',eez='eez',sfa = 'inshore'),
                 add_custom = list(obj = east.shore))
S29E.fish <- S29E + geom_sf(data=in.29E.dat) + ggtitle("Scallop fishery 2017- June 2020")

# Here's the overall network 
MPAn <- pecjector(area = "NFLD",c_sys = 4269, 
                  add_layer = list(land = 'grey',eez='eez',sfa = 'all'),
                  add_custom = list(obj = mpa.network))

MPA.fish <- MPAn + geom_sf(data = in.dat.sf,size=0.1,alpha=0.1) + 
                   geom_sf(data=fish.dat.sf,size=0.1,alpha=0.1) + 
                   geom_sf(data=mpa.network,fill=NA,lwd=1,color='blue') + ggtitle("Scallop fishery 2017- June 2020")

# And finally here's the inshore piece of the network 
MPA.in <- pecjector(area = "inshore",c_sys = 4269, 
                  add_layer = list(land = 'grey',eez='eez',sfa = 'inshore'),
                  add_custom = list(obj = mpa.network))

MPA.in.fish <- MPA.in + geom_sf(data=inshore.dat,size=0.1,alpha=0.1) + 
                        geom_sf(data=mpa.network,fill=NA,lwd=1,color='blue') + 
                        coord_sf(xlim = c(-67.50,-64.30),ylim=c(43.10,45.80)) +
                        ggtitle("Scallop fishery 2017- June 2020")


