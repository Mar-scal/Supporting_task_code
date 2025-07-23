# Convert our seedboxes into GIS layers....

library(sf)
library(maptools)
library(tidyverse)

source("D:/Github/Assessment_fns/Maps/Convert_PBSmapping_into_GIS_shapefiles.R")

# Bring in the latest seedbox data
seedboxes <- read.csv("Y:/Offshore/Assessment/Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv")

# Easy peasy, convert all the seedboxes into shapefiles.
hmm <- pbs.2.gis(seedboxes,layer.names = "ID",save.loc = "D:/testing_folder/seeds",env.object = F)

