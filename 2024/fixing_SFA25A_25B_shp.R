# fixing SFA25A and 25B shapefiles

require(ggplot2)
require(tidyr)
require(dplyr)
require(patchwork)
require(sf)
require(ggrepel)

funs <- c("https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/github_spatial_import.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs)
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}

offshore <- github_spatial_import("offshore", "offshore.zip", quiet=T)

offshore2 <- st_intersection(offshore[offshore$ID %in% c("SFA25A", "SFA25B"),]) %>% mutate(ID2=1:nrow(.))
offshore <- offshore[!offshore$ID %in% c("SFA25A", "SFA25B"),]
#ggplot() + geom_sf(data=offshore2) + facet_wrap(~ID2)
offshore2 <- offshore2[offshore2$ID2 %in% 1:2,] %>% mutate(ID = c("SFA25A", "SFA25B")) %>% select(-ID2, -n.overlaps, -origins)
offshore <- rbind(offshore, offshore2)
ggplot() + geom_sf(data=offshore) + facet_wrap(~ID)
SFA25A <- offshore[offshore$ID=="SFA25A",]
SFA25B <- offshore[offshore$ID=="SFA25B",]
st_write(obj = SFA25A, "C:/Users/keyserf/Documents/Github/GIS_layers/offshore/SFA25A.shp", append=F)
st_write(obj = SFA25B, "C:/Users/keyserf/Documents/Github/GIS_layers/offshore/SFA25B.shp", append=F)