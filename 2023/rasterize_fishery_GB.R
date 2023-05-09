# Fishery layers for Pete (GB G&C project)

load("Y:/Offshore/Assessment/Data/Fishery_data/Summary/2022/OSAC_tidy_logs.RData")

head(fish.dat)
fish.dat <- fish.dat[fish.dat$bank %in% c("GBa", "GBb") & !is.na(fish.dat$bank),]

head(fish.dat)
require(sf)
require(ggplot2)
require(stars)
require(sf)
require(fasterize)
require(raster)
require(tidyr)
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish_sf <- st_as_sf(fish.dat, coords=c(X="lon", Y="lat"), crs=4326)

ggplot() + geom_sf(data=fish_sf[1:10,])


source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")
offshore <- github_spatial_import("offshore", "offshore.zip", quiet=T)

ggplot() + geom_sf(data=offshore)

GB <- offshore[offshore$ID %in% c("SFA27A", "SFA27B"),]
ggplot() + geom_sf(data=GB)

fish_sf <- st_intersection(fish_sf, GB)

ggplot() + geom_sf(data=GB) + geom_sf(data=fish_sf)

fish_sf <- st_transform(fish_sf, 32619)
GB <- st_transform(GB, 32619)

# rasterize it
# to get size^2 grid
size <- 15000 #(grid size in m)
nx <- round((st_bbox(GB)$xmax[[1]] - st_bbox(GB)$xmin[[1]])/size, 0)
ny <- round((st_bbox(GB)$ymax[[1]] -  st_bbox(GB)$ymin[[1]])/size, 0)
# grid cells are 5 km2

# adjust these numbers to get exactly 1km2
xmin <- st_bbox(GB)$xmin[[1]]
xmax <- xmin + ((nx+1)*size)
ymin <- st_bbox(GB)$ymin[[1]]
ymax <- ymin + ((ny+1)*size)
box <- st_as_sf(x = expand.grid(x=c(xmin,xmax), y=c(ymin, ymax))[c(1,2,4,3),],coords=c(X="x", Y="y"), crs=32619) %>% 
  st_combine() %>% 
  st_cast("POLYGON")
r <- st_rasterize(sf = st_sf(box), template = st_as_stars(box, nx = (nx+1), ny = (ny+1)))
GBraster <- st_as_sf(r)
GBraster$cell <- 1:nrow(GBraster)

vessels <- read.csv("Y:/Offshore/Assessment/Data/Offshore_Fleet.csv")
vessels$vrnum <- vessels$ID
companies_post <- dplyr::select(vessels, Company, vrnum)

table(companies_post$vrnum)
# issue is that 105912 changed from CLSP to A&K in 2016

fish_sf <- fish_sf[fish_sf$year>2008,]
fish_sf2 <- dplyr::filter(fish_sf, year>2008 & year <2016) %>%
  dplyr::left_join(companies_post[!(companies_post$vrnum==105912 & companies_post$Company=="A&K"),])
fish_sf3 <- dplyr::filter(fish_sf, year>2008 & year >2015) %>%
  dplyr::left_join(companies_post[!(companies_post$vrnum==105912 & companies_post$Company=="CLSP"),])

(dim(fish_sf2) + dim(fish_sf3))[1] == dim(fish_sf)[1]

fish_sf <- rbind(fish_sf2, fish_sf3)

fish_grid <- st_intersection(fish_sf[fish_sf$year==2017,], GBraster) %>%
  dplyr::group_by(cell, year) %>%
  dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
            hm = sum(hm, na.rm=T),
            nvessels = length(unique(vrnum)),
            ncompanies = length(unique(Company)))

st_geometry(fish_grid) <- NULL

fish_grid <- dplyr::left_join(GBraster, fish_grid)

comp <- ggplot() + geom_sf(data=fish_grid) + facet_wrap(~ncompanies) + ggtitle("number of companies")
ves <- ggplot() + geom_sf(data=fish_grid) + facet_wrap(~nvessels) + ggtitle("number of vessels")
catch <- ggplot() + geom_sf(data=fish_grid, aes(fill=kg))
hm <- ggplot() + geom_sf(data=fish_grid, aes(fill=hm))


require(patchwork)
png(paste0("Y:/Offshore/Data requests/2023/CDD_Peter/grid_", (size/1000)^2, "km2.png"), height=6, width=12, units="in", res=420)
(catch / hm) | (comp + ves)
dev.off()

hist_comp <- ggplot() + geom_histogram(data=fish_grid, aes(ncompanies))
hist_ves <- ggplot() + geom_histogram(data=fish_grid, aes(nvessels))

png(paste0("Y:/Offshore/Data requests/2023/CDD_Peter/grid_", (size/1000)^2, "km2_hist.png"), height=6, width=10, units="in", res=420)
hist_comp + hist_ves
dev.off()


st_write(fish_grid, "Y:/Offshore/Data requests/2023/CDD_Peter/GB_scallop_catch_2017.shp", append = F)

r <- raster(st_sf(box), res = 1000)
r <- fasterize(fish_grid, r, field = "kg", fun="sum")
plot(r)

r[is.na(r[])] <- 0

writeRaster(r, "Y:/Offshore/Data requests/2023/CDD_Peter/GB_scallop_catch_2017", format = "GTiff", overwrite=T)

y1 <- min(st_coordinates(fish_grid[!is.na(fish_grid$year),])[,2]) %/% 10000 * 10000
y2 <- max(st_coordinates(fish_grid[!is.na(fish_grid$year),])[,2]) %/% 10000 * 10000 + 10000


ggplot() + geom_sf(data=GB) + geom_sf(data=fish_grid[!is.na(fish_grid$year),], aes(fill=kg), colour=NA) +
  ggtitle("2017 scallop catch on GB - 1 km2 grid") +
  coord_sf(ylim=c(y1, y2))


####### testing

test <- st_read("Y:/Offshore/Data requests/2023/CDD_Peter/GB_scallop_catch_2017.shp")

y1 <- min(st_coordinates(test[!is.na(test$year),])[,2]) %/% 10000 * 10000
y2 <- max(st_coordinates(test[!is.na(test$year),])[,2]) %/% 10000 * 10000 + 10000


ggplot() + geom_sf(data=GB) + geom_sf(data=test[!is.na(test$year),], aes(fill=kg), colour=NA) +
  ggtitle("2017 scallop catch on GB - 1 km2 grid") +
  coord_sf(ylim=c(y1, y2))

test2 <- raster("Y:/Offshore/Data requests/2023/CDD_Peter/GB_scallop_catch_2017.tif")
plot(test2)




