# This script is used to re-allocate the GBa & GBb stations in 2018 survey in case funding cannot be found to run 2 legs this summer.


direct <- "Y:/Offshore scallop/Assessment/"
options(stringsAsFactors=F)
source(paste(direct,"Assessment_fns/Maps/ScallopMap.r",sep=""))
library(lubridate)
# Set a random seed so these results are reproducible
seed <- 1
set.seed(seed)
# Bring in the survey strata...
# Get the correct survey polygons
surv.polyset <- read.csv(paste(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv",sep=""),stringsAsFactors = F) #Read1
# Bring in the seedboxes
seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes_and_monitoring_areas.csv",sep=""),
                     stringsAsFactors = F,header=T) # Read3
seedboxes$Open <- dmy(seedboxes$Open)
# Now bring in the data we need for the allocations
surv.strata <- read.csv(paste0(direct,"Data/Survey_data/survey_information.csv"))
GBa.full.allocation <-  read.csv(paste0(direct,"Data/Survey_data/2018/Summer/GBa/Preliminary_Survey_design_Tow_locations.csv"))
GBb.full.allocation <- read.csv(paste0(direct,"Data/Survey_data/2018/Summer/GBb/Preliminary_Survey_design_Tow_locations.csv"))

# Set some parameters....
GBa.alloc <- nrow(GBa.full.allocation)
GBb.alloc <-  nrow(GBb.full.allocation)
GBa.strata.alloc <- table(GBa.full.allocation$STRATA)
org.alloc <- GBa.alloc + GBb.alloc
# How much do we think we can get done...
tows.per.watch <- 5
watch.per.day <- 4
num.days <- 10
exp.alloc <- tows.per.watch * watch.per.day *num.days
alloc.lost <- org.alloc - exp.alloc
per.alloc.lost <- exp.alloc / org.alloc

GBa.new.alloc <- round(GBa.alloc * per.alloc.lost)
GBb.new.alloc <- round(GBb.alloc * per.alloc.lost)

### Start with GBb because this is a bit more straightforward. # Allocation by area would be...
GBb.strata <- surv.strata[surv.strata$label=="GBb",]
GBb.area <- sum(GBb.strata$towable_area)
GBb.strata$prop <- GBb.strata$towable_area/GBb.area
GBb.strata$current_allocation <- table(GBb.full.allocation$Poly.ID)
GBb.strata$backup_allocation <- round(GBb.strata$prop*GBb.new.alloc)


n.strata <- nrow(GBb.strata)
retain <- NULL
for(i in 1:n.strata)
{
dat <- GBb.full.allocation[GBb.full.allocation$Poly.ID == i,]
retain[[i]]  <- sample(dat$EID,GBb.strata$backup_allocation[i])
}
GBb.keep <- sort(do.call("c",retain))
GBb.backup.allocation <- GBb.full.allocation[GBb.full.allocation$EID %in% GBb.keep,]
# So in this scenario we are losing 1 station from each strata except the very high north, I'm o.k. with that...

# GBa is more complicated, we don't want to pull any stations in the south because once we get up north we'll never get back down south, so 
# the 26 station we are losing have to come from the north.  Fortunately, the Medium - Very High north strata have more than a proportional allocation, so 
# if we went with a proportional allocation scheme we'd immediately be losing 23 stations from these strata, so we'd only have 3 more stations to deal with
# which I'd suggest we remove from the Low strata.  Now the allocated by area scheme actually has 52 stations (base allocation has 51 stations) so we'd need to subtract
# 4 from the low allocation_by_area strategy in the below scripts...
#Complicating this is that the low strata is spread across the entire region
GBa.strata <- surv.strata[surv.strata$label=="GBa",]
GBa.area <- sum(GBa.strata$towable_area)
GBa.strata$prop <- GBa.strata$towable_area/GBa.area
GBa.strata$current_allocation <- table(GBa.full.allocation$Poly.ID)
# alloc by area doesn't quite work b/c we'd have 1 too many in low, so toss one of those and put it to medium north which is the most ripped off by rounding...
GBa.strata$allocation_by_area <- round(GBa.strata$prop*GBa.new.alloc)
GBa.strata$allocation_by_area[1]  <- GBa.strata$allocation_by_area[1]-1
GBa.strata$allocation_by_area[2]  <- GBa.strata$allocation_by_area[2]+1
# Now just simply thin it by the current proportion of stations.
GBa.strata$current_allocation_thinned <- round(174*GBa.strata$current_allocation/GBa.alloc)
GBa.strata$save_the_south <- c(GBa.strata$current_allocation[1] -3, GBa.strata$allocation_by_area[2:4],GBa.strata$current_allocation[5:7])


retain.prop <- NULL
retain.thin <- NULL
retain.sts <- NULL
n.strata <- nrow(GBa.strata)

for(i in 1:n.strata)
{
  dat <- GBa.full.allocation[GBa.full.allocation$Poly.ID == i,]
  retain.prop[[i]]  <- sample(dat$EID,GBa.strata$allocation_by_area[i])
  retain.thin[[i]]  <- sample(dat$EID,GBa.strata$current_allocation_thinned[i])
  retain.sts[[i]]  <- sample(dat$EID,GBa.strata$save_the_south[i])
}

# Unpack the lists...
GBa.prop.keep <- sort(do.call("c",retain.prop))
GBa.thin.keep <- sort(do.call("c",retain.thin))
GBa.sts.keep <- sort(do.call("c",retain.sts))

GBa.prop.allocation <- GBa.full.allocation[GBa.full.allocation$EID %in% GBa.prop.keep,]
GBa.thin.allocation <- GBa.full.allocation[GBa.full.allocation$EID %in% GBa.thin.keep,]
GBa.sts.allocation <- GBa.full.allocation[GBa.full.allocation$EID %in% GBa.sts.keep,]

# Now make a column that indicates if a tow is a primary or backup station.
GBa.full.allocation$prop.backup <- "Backup"
GBa.full.allocation$thin.backup <- "Backup"
GBa.full.allocation$sts.backup <- "Backup"
GBb.full.allocation$backup <- "Backup"
GBa.full.allocation$prop.backup[GBa.full.allocation$EID %in% GBa.prop.keep] <- "Primary"
GBa.full.allocation$thin.backup[GBa.full.allocation$EID %in% GBa.thin.keep]<- "Primary"
GBa.full.allocation$sts.backup[GBa.full.allocation$EID %in% GBa.sts.keep]<- "Primary"

GBb.full.allocation$backup[GBb.full.allocation$EID %in% GBb.keep] <- "Primary"

# Save the results...
write.csv(GBb.full.allocation,paste0(direct,"Data/Survey_data/2018/Summer/GBb/backup_allocation_scheme/Proportional_GBb_survey_design_Tow_locations.csv"))
write.csv(GBa.full.allocation,paste0(direct,"Data/Survey_data/2018/Summer/GBa/backup_allocation_scheme/Proportional_GBa_survey_design_Tow_locations.csv"))

# Now make some figures....

# Now if you want to make the plots do all of thism, first set up the survey polygons

GBa.poly <- subset(surv.polyset,label=="GBa")
GBb.poly <- subset(surv.polyset,label=="GBb")
attr(GBa.poly,"projection")<-"LL"
attr(GBb.poly,"projection")<-"LL"
# Get the seedboxes of interest
sb <- subset(seedboxes,Bank == "GBa" & Open >= ymd(paste(2018,"-01-01",sep="")))


# First up is the GBa plot for the proportional allocation backup scheme
windows(11,8.5)
png(paste(direct,2018,"/Survey_Design/GBa/backup_allocation_scheme/Proportional_Survey_allocation-GBa.png",sep=""),width = 11, units="in", res=420,
                      height = 8.5,bg = "transparent")
# Make the plot, add a title, the tow locations, any extra tows and any seedboxes + optionally a legend.
ScallopMap("GBa",poly.lst=list(GBa.poly,GBa.strata),plot.bathy = T,plot.boundries = T,dec.deg = F)
# For some reason I can't figure out Sable is overplotting the medium strata on top of the high and very high, this is my hack to fix....

title(paste("Proportional Backup Allocation Scenario  (","GBa","-",2018,")",sep=""),cex.main=2,line=1)
# text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
addPoints(GBa.full.allocation[GBa.full.allocation$prop.backup == "Backup",],pch=22, cex=1.5, 
          bg = GBa.strata$col[GBa.full.allocation$Poly.ID[GBa.full.allocation$prop.backup == "Backup"]])
addPoints(GBa.full.allocation[GBa.full.allocation$prop.backup == "Primary",],pch=21, cex=1, 
          bg = GBa.strata$col[GBa.full.allocation$Poly.ID[GBa.full.allocation$prop.backup == "Primary"]])

addPolys(sb,lty=2,lwd=2)
legend('bottomleft',legend=GBa.strata$PName,pch=21,pt.bg=GBa.strata$col,bty='n',cex=0.9, inset = .01)
legend('topright',paste("Primary Survey stations (n = ",nrow(GBa.full.allocation[GBa.full.allocation$prop.backup == "Primary",]),")",sep=""),pch=21,bty='n',cex=0.9, inset = .01)
legend('topright',paste("Backup stations (n = ",nrow(GBa.full.allocation[GBa.full.allocation$prop.backup == "Backup",]),")",sep=""),
                                           pch=22,bty='n',cex=0.9, inset = 0.05)
legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
# Turn the device off if necessary.  
dev.off()

# The plot of the thinned alternative
windows(11,8.5)
png(paste(direct,2018,"/Survey_Design/GBa/backup_allocation_scheme/Thinned_Survey_allocation-GBa.png",sep=""),width = 11, units="in", res=420,
    height = 8.5,bg = "transparent")
# Make the plot, add a title, the tow locations, any extra tows and any seedboxes + optionally a legend.
ScallopMap("GBa",poly.lst=list(GBa.poly,GBa.strata),plot.bathy = T,plot.boundries = T,dec.deg = F)
# For some reason I can't figure out Sable is overplotting the medium strata on top of the high and very high, this is my hack to fix....

title(paste("Thinned Backup Allocation Scenario  (","GBa","-",2018,")",sep=""),cex.main=2,line=1)
# text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
addPoints(GBa.full.allocation[GBa.full.allocation$thin.backup == "Backup",],pch=22, cex=1.5, 
          bg = GBa.strata$col[GBa.full.allocation$Poly.ID[GBa.full.allocation$thin.backup == "Backup"]])
addPoints(GBa.full.allocation[GBa.full.allocation$thin.backup == "Primary",],pch=21, cex=1, 
          bg = GBa.strata$col[GBa.full.allocation$Poly.ID[GBa.full.allocation$thin.backup == "Primary"]])

addPolys(sb,lty=2,lwd=2)
legend('bottomleft',legend=GBa.strata$PName,pch=21,pt.bg=GBa.strata$col,bty='n',cex=0.9, inset = .01)
legend('topright',paste("Primary Survey stations (n = ",nrow(GBa.full.allocation[GBa.full.allocation$thin.backup == "Primary",]),")",sep=""),pch=21,bty='n',cex=0.9, inset = .01)
legend('topright',paste("Backup stations (n = ",nrow(GBa.full.allocation[GBa.full.allocation$thin.backup == "Backup",]),")",sep=""),
       pch=22,bty='n',cex=0.9, inset = 0.05)
legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
# Turn the device off if necessary.  
dev.off()

 # The plot of the save the south alternative
windows(11,8.5)
png(paste(direct,2018,"/Survey_Design/GBa/backup_allocation_scheme/STS_Survey_allocation-GBa.png",sep=""),width = 11, units="in", res=420,
    height = 8.5,bg = "transparent")
# Make the plot, add a title, the tow locations, any extra tows and any seedboxes + optionally a legend.
ScallopMap("GBa",poly.lst=list(GBa.poly,GBa.strata),plot.bathy = T,plot.boundries = T,dec.deg = F)
# For some reason I can't figure out Sable is overplotting the medium strata on top of the high and very high, this is my hack to fix....

title(paste("STS Backup Allocation Scenario  (","GBa","-",2018,")",sep=""),cex.main=2,line=1)
# text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
addPoints(GBa.full.allocation[GBa.full.allocation$sts.backup == "Backup",],pch=22, cex=1.5, 
          bg = GBa.strata$col[GBa.full.allocation$Poly.ID[GBa.full.allocation$sts.backup == "Backup"]])
addPoints(GBa.full.allocation[GBa.full.allocation$sts.backup == "Primary",],pch=21, cex=1, 
          bg = GBa.strata$col[GBa.full.allocation$Poly.ID[GBa.full.allocation$sts.backup == "Primary"]])

addPolys(sb,lty=2,lwd=2)
legend('bottomleft',legend=GBa.strata$PName,pch=21,pt.bg=GBa.strata$col,bty='n',cex=0.9, inset = .01)
legend('topright',paste("Primary Survey stations (n = ",nrow(GBa.full.allocation[GBa.full.allocation$thin.backup == "Primary",]),")",sep=""),pch=21,bty='n',cex=0.9, inset = .01)
legend('topright',paste("Backup stations (n = ",nrow(GBa.full.allocation[GBa.full.allocation$thin.backup == "Backup",]),")",sep=""),
       pch=22,bty='n',cex=0.9, inset = 0.05)
legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
# Turn the device off if necessary.  
dev.off()

# The plot of the save the south alternative
windows(11,8.5)
png(paste(direct,2018,"/Survey_Design/GBb/backup_allocation_scheme/Proportional_Survey_allocation-GBb.png",sep=""),width = 11, units="in", res=420,
    height = 8.5,bg = "transparent")
# Make the plot, add a title, the tow locations, any extra tows and any seedboxes + optionally a legend.
ScallopMap("GBb",poly.lst=list(GBb.poly,GBb.strata),plot.bathy = T,plot.boundries = T,dec.deg = F)
# For some reason I can't figure out Sable is overplotting the medium strata on top of the high and very high, this is my hack to fix....

title(paste("Proportional Backup Allocation Scenario  (","GBb","-",2018,")",sep=""),cex.main=2,line=1)
# text(towlst[[i]]$Tows$X,towlst[[i]]$Tows$Y,label=towlst[[i]]$Tows$EID,col='black', cex=0.6)
addPoints(GBb.full.allocation[GBb.full.allocation$backup == "Backup",],pch=22, cex=1.5, 
          bg = GBb.strata$col[GBb.full.allocation$Poly.ID[GBa.full.allocation$backup == "Backup"]])
addPoints(GBb.full.allocation[GBb.full.allocation$backup == "Primary",],pch=21, cex=1, 
          bg = GBb.strata$col[GBb.full.allocation$Poly.ID[GBb.full.allocation$backup == "Primary"]])

#addPolys(sb,lty=2,lwd=2)
legend('bottomleft',legend=GBb.strata$PName,pch=21,pt.bg=GBb.strata$col,bty='n',cex=0.9, inset = .01)
legend('topright',paste("Primary Survey stations (n = ",nrow(GBb.full.allocation[GBb.full.allocation$backup == "Primary",]),")",sep=""),pch=21,bty='n',cex=0.9, inset = .01)
legend('topright',paste("Backup stations (n = ",nrow(GBb.full.allocation[GBb.full.allocation$backup == "Backup",]),")",sep=""),
       pch=22,bty='n',cex=0.9, inset = 0.05)
legend('topleft',paste("Note: The random seed was set to ",seed,sep=""),cex=0.8,bty="n")
# Turn the device off if necessary.  
dev.off()