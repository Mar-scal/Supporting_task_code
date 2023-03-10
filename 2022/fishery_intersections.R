

require(tidyverse)

direct <- "Y:/Offshore/Assessment/"
direct_fns <- "C:/Users/keyserf/Documents/Github/Assessment_fns/"

source(paste0(direct_fns, "Maps/github_spatial_import.R"))
offshore <- github_spatial_import("offshore", "offshore.zip")
survey <- github_spatial_import("survey_boundaries", "survey_boundaries.zip")


source(paste0(direct_fns, "Fishery/logs_and_fishery_data.r"))
logs_and_fish(loc="offshore",year = 2022, direct="Y:/Offshore/Assessment/")
new.log.dat$year <- year(new.log.dat$fished)
new.log.dat$month <- month(new.log.dat$fished)
new.log.dat$date <- ymd(new.log.dat$date)

require(sf)
log.sf <- st_as_sf(new.log.dat[!is.na(new.log.dat$lon),], coords = c("lon", "lat"), crs=4326) %>%
  st_transform(32620)

#make a grid
grid <- st_as_sfc(st_bbox(log.sf)) %>%
  st_transform(32620)
grid <- st_as_sf(st_make_grid(grid, cellsize=5000))
plot(grid)
grid$ID <- 1:nrow(grid)

# join log data to grid, attach grid ID to each record
log.sf.grid <- st_intersection(grid, log.sf)
dim(log.sf.grid)[1] == dim(log.sf)[1]

log.sf.grid <- log.sf.grid %>% 
  group_by(ID, bank) %>%#month) %>%
  dplyr::summarize(kg = sum(pro.repwt),
            hm = sum(hm))
  
# replace geometry with grid corners
st_geometry(log.sf.grid) <- NULL
log.sf.grid <- left_join(grid, log.sf.grid)

# cells with catch that are outside SFA polys
# first, get the entire fishable area
boundary <- st_union(st_difference(offshore[!st_is_empty(offshore$geometry),]))

# attach SFA to each cell
# dim(log.sf.grid)
# log.sf.grid$intersects <- st_intersects(log.sf.grid, st_transform(boundary, 32620), sparse = F)
# dim(inside[!is.na(inside$kg),])
# dim(inside)
# 
# ggplot() + geom_sf(data=log.sf.grid[log.sf.grid$intersects==TRUE,], colour="green") +
#   geom_sf(data=log.sf.grid[log.sf.grid$intersects==FALSE,], colour="red") + 
#   geom_sf(data=boundary, fill=NA)

# there's no catch outside the boundary - good!
#log.sf.grid[log.sf.grid$intersects==FALSE & !is.na(log.sf.grid$kg),]

# get the individual sfas
offshore <- st_difference(st_transform(offshore[!st_is_empty(offshore$geometry),], 32620))
#within <- st_join(log.sf.grid, offshore, join=st_within)
intersects <- st_join(log.sf.grid, offshore, join=st_intersects)
#dim(within) == dim(log.sf.grid)
dim(intersects) == dim(log.sf.grid)
mismatch <- intersects[!intersects$bank == intersects$ID.y & !is.na(intersects$kg),]
plot(mismatch)
plot(mismatch[mismatch$bank=="Ger",])

ggplot() + geom_sf(data=st_transform(intersects[intersects$bank=="Ger",], 4326), aes(fill=ID.y)) +
  geom_sf(data=st_transform(offshore[offshore$ID %in% c("Ger", "BBs"),], 4326), fill=NA) +
  geom_sf(data=log.sf[log.sf$bank=="Ger",])+
  coord_sf(xlim=c(-66.1, -65), ylim=c(42.5, 43.5))
  
line <- data.frame(y=c(43.2, 42.305), x=c(-65.52083, -65.97167), ID=1)
line <- st_as_sf(line, coords=c(x="x", y="y"), crs=4326) %>%
  #st_coordinates() %>%
  st_combine %>%
  st_cast("LINESTRING")

ggplot() + geom_sf(data=st_transform(intersects[intersects$bank=="Ger",], 4326), aes(fill=ID.y)) +
  geom_sf(data=st_transform(offshore[offshore$ID %in% c("Ger", "BBs"),], 4326), fill=NA) +
  geom_sf(data=log.sf[log.sf$bank=="Ger",])+
  geom_sf(data=line, colour="blue", lwd=2)+
  geom_text(data=offshorecsv[offshorecsv$bank=="Ger",], aes(X, Y, label=POS))+
  coord_sf(xlim=c(-66.1, -65), ylim=c(42.5, 43.5))  
  

intersects <- intersects[intersects$bank == intersects$ID.y,]

# compare fishing to survey.bound.polys on other banks too.
survey.bound.polys<-read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
                             header=T,stringsAsFactors = F)

offshorecsv <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",sep=""),
                             header=T,stringsAsFactors = F)

offshorecsv <- st_as_sf(offshorecsv, coords=c("X", "Y"), crs=4326)
sfa_ger <- offshorecsv[offshorecsv$bank=="Ger",] %>%
  st_combine() %>%
  st_cast("LINESTRING") %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(ID=1) %>%
  rename(geometry="x")
plot(sfa_ger)

sfa_bbs <- offshorecsv[offshorecsv$bank=="BBs",] %>%
  st_combine() %>%
  st_cast("LINESTRING") %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  mutate(ID=1) %>%
  rename(geometry="x")
plot(sfa_bbs)

sfa_sab<-st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore/Sab.shp")

st_write(sfa_ger, dsn = "Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore/Ger.shp", overwrite=T, append=F)
st_write(sfa_bbs, dsn = "Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore/BBs.shp", overwrite=T, append=F)

ggplot() + geom_text(data=offshorecsv[offshorecsv$bank=="BBs",], aes(X, Y, label=POS), colour="blue") +
  geom_text(data=offshorecsv[offshorecsv$bank=="Ger",], aes(X, Y, label=POS), colour="red")+
  coord_sf(xlim=c(-66.1, -65), ylim=c(42.5, 43.5))  

i="GBa"
ggplot() + geom_sf(data=st_transform(intersects[intersects$bank==i,], 4326), aes(fill=ID.y)) +
  geom_sf(data=st_transform(offshore[offshore$ID ==i,], 4326), fill=NA) +
  geom_sf(data=st_transform(survey[survey$ID==i,], 4326), fill=NA)+
  geom_sf(data=log.sf[log.sf$bank==i,])+
  geom_sf(data=line, colour="blue", lwd=2)+
  geom_text(data=offshorecsv[offshorecsv$bank==i,], aes(X, Y, label=POS))#+
  coord_sf(xlim=c(-66.1, -65), ylim=c(42.5, 43.5))  


intersurv <- st_join(log.sf, st_transform(survey, 32620), join=st_intersects)
offshore$bank <- offshore$ID
survey$bank <- survey$ID


ggplot() + geom_sf(data=st_transform(intersurv[is.na(intersurv$ID),], 4326), colour="blue") +
  geom_sf(data=st_transform(offshore, 4326), fill=NA) +
  geom_sf(data=st_transform(survey, 4326), fill=NA) + 
  #facet_wrap(~bank) + 
  coord_sf(xlim=c(-68, -55), ylim=c(40, 46))
# check BBs

table(intersurv[is.na(intersurv$ID),]$bank)

outsidesurv <- intersurv[is.na(intersurv$ID),] %>%
  group_by(bank) %>%
  dplyr::summarize(outsidekg = sum(pro.repwt),
            outsidehm = sum(hm),
            outsiderecords = n())

st_geometry(outsidesurv) <- NULL

# expand SFA boundaries for each month
months <- data.frame(ID = rep(offshore$ID, 1), month=rep(1, each=length(offshore$ID)))
months <- left_join(months, offshore)
months <- st_as_sf(months)

# get maximums for scales
maxkg <- max(log.sf.grid$kg, na.rm=T)
maxhm <- max(log.sf.grid$hm, na.rm=T)

require(patchwork)
seasonal <- NULL
for (i in unique(log.sf.grid$ID.1)){
  seasonal[[i]] <- ggplot() + geom_sf(data=months[months$ID == i,], fill=NA) +
    geom_sf(data=log.sf.grid[!is.na(log.sf.grid$kg) & log.sf.grid$ID==i,], aes(fill=kg)) + 
    geom_sf(data=survey[survey$ID==0,], fill=NA)+
    scale_fill_viridis_c(option = "magma", direction = -1, limits=c(0,maxkg)) +
    theme_minimal() + 
    facet_grid(~month) + 
    ggtitle(i)
}


seasonal[[1]] / seasonal[[2]] / seasonal[[3]] / seasonal[[4]] / seasonal[[5]] / seasonal[[6]] / seasonal[[7]]


months$ID.1 <- months$ID


log.raster <- log.sf.grid
centroids <- as.data.frame(st_coordinates(st_centroid(log.raster)))
st_geometry(log.raster) <- NULL
log.raster <- cbind(as.data.frame(log.raster), centroids)
#log.raster <- st_as_sf(log.raster, coords=c("X", "Y"), crs=32620)

#log.raster$kg[is.na(log.raster$kg)] <- 0
ggplot(log.raster[!is.na(log.raster$month) & log.raster$month==1 & log.raster$ID.1=="GBa",], aes(X, Y)) + geom_raster(aes(fill=kg)) + 
  scale_fill_viridis_c(option = "magma", direction = -1) +
  facet_grid(ID.1~month, scales="free")

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_point(aes(colour = density))

ggplot(log.raster[!is.na(log.raster$month) & log.raster$month==1 & log.raster$ID.1=="GBa",], aes(X, Y)) +
  geom_point(aes(colour = kg))

i="Sab"
ggplot() + geom_sf(data=log.sf[log.sf$bank==i,], aes(colour=pro.repwt)) +
  geom_sf(data=offshore[offshore$ID==i,], fill=NA) +
  geom_sf(data=survey[survey$ID==i,], fill=NA)

survey <- github_spatial_import("survey_boundaries", "survey_boundaries.zip")
