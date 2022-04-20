library(tidyverse)
library(grid)
library(sf)
library(cowplot)
library(PBSmapping)
library(maptools)

# We need a new German polygon...
sbp <- read.csv(paste0("D:/NAS/Offshore/Assessment/Data/Maps/approved/Survey/survey_boundary_polygons.csv"), 
         header=T,stringsAsFactors = F)
nap<-read.csv("D:/NAS/Offshore/Assessment/Data/Maps/approved/Fishing_Area_Borders/Offshore.csv",stringsAsFactors = F,header=T)
g.bnds <- sbp[sbp$label=="Ger",]
Y.range <- range(g.bnds$Y,na.rm=T)
X.range <- range(g.bnds$X,na.rm=T)
g.tmp <- nap[nap$label=="SFA26",]
g.tmp <- g.tmp[, c("PID", "POS", "X", "Y", "label", "bank")]
g.tmp$X[g.tmp$X < X.range[1]] <- X.range[1]
g.tmp$X[g.tmp$X > X.range[2]] <- X.range[2]
g.tmp$Y[g.tmp$Y > Y.range[2]] <- Y.range[2]
g.tmp$Y[g.tmp$Y < Y.range[1]] <- Y.range[1]
# Now I want to insert a segemnt into the boundary to run a diagonal line from around 43?9/-66.755 to 43/-66?24
g.tmp[2,] <- c(5,2,-66.4,Y.range[1],"SFA26","Ger")
g.tmp <- as.data.frame(rbind(g.tmp[c(1,2),],c(5,2,X.range[1],43.15,"SFA26","Ger"),g.tmp[3:nrow(g.tmp),]))
for(k in 1:4) g.tmp[,k] <- as.numeric(g.tmp[,k]) # Silly rbind making everything characters...
g.tmp$POS <- 1:nrow(g.tmp)
g.tmp$PID <- as.numeric(g.tmp$PID)
g.tmp$X <- as.numeric(g.tmp$X)
g.tmp$Y <- as.numeric(g.tmp$Y)
bound.poly.surv <- as.PolySet(g.tmp,projection="LL")
bound.poly.surv.sp <- PolySet2SpatialPolygons(bound.poly.surv)
ger.new <- st_as_sf(bound.poly.surv.sp)
# Make this the new German survey boundary...
# Looks decent, if Freya and Trish approve I'll save this into the GIS world...
ggplot(ger.new) 
st_write(ger.new,"D:/Github/Offshore/GIS_layers/DK/survey_boundaries/Ger.shp")
