# Little scirpt to get the fishery summary for Jeremy Collie, he was looking for the daily average gear width, tow duration, and number of tows on GB for each
# year between 1994 and 1999

source("Y:/ESS/Offshore scallop/Assessment/Assessment_fns/Fishery/archive/logs_and_fishery_data.r")
direct <- "Y:/ESS/Offshore scallop/Assessment"

logs_and_fish(loc="offshore",year = 1994:1999,direct.off=direct)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
