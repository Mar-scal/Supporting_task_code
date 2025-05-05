#anonymizing port.dat

#anonymize fleet dat
off.fleet <- read.csv(paste0("Y:/Offshore/Assessment/","Data/Offshore_fleet.csv"))
#off.fleet <- read.csv("C:/Users/keyserf/Documents/temp_data/Data/Offshore_fleet.csv")
off.fleet$anon_id <- sample(100:(100+nrow(off.fleet)), nrow(off.fleet), replace=FALSE)
off.fleet <- dplyr::select(off.fleet, Pre_2008_ID, ID, ID_alt_Port_Sampling, ASM_date, ring_size_2024, gear_size, anon_id)
names(off.fleet) <- c("vesid", "vrnum", "boat", "ASM_date", "ring_size_2024", "gear_size_typical", "anon_id")

off.fleet$vrnum[off.fleet$boat=="GUAR" & !is.na(off.fleet$boat)] <- 105912.1
off.fleet$boat[off.fleet$boat=="COME" & !is.na(off.fleet$boat) & off.fleet$vesid==4211] <- "COME1"
off.fleet$vesid[off.fleet$vesid==5004 & is.na(off.fleet$boat)] <- 5004.1
off.fleet$vrnum[off.fleet$vrnum==1516 & off.fleet$boat=="COME1"] <- 1516.1
# so all the 1516 records are getting assigned MER7

#fishery data
source("C:/Users/keyserf/Documents/GitHub/Assessment_fns/Fishery/logs_and_fishery_data.r")
logs_and_fish(loc="offshore", get.local=T, direct="Y:/Offshore/Assessment/", year = 1981:2024)
#save(new.log.dat, file = "C:/Users/keyserf/Documents/temp_data/newlogdat_1981-2024.RData")
#save(old.log.dat, file = "C:/Users/keyserf/Documents/temp_data/oldlogdat_1981-2024.RData")

new.log.dat2 <- dplyr::left_join(new.log.dat, dplyr::select(off.fleet, vrnum, anon_id))
old.log.dat2 <- dplyr::left_join(old.log.dat, dplyr::select(off.fleet, vesid, anon_id))
new.log.dat2 <- dplyr::select(new.log.dat2, -bottom, -numbags)
old.log.dat2 <- dplyr::select(old.log.dat2, -bottom, -numbags)

nrow(new.log.dat) == nrow(new.log.dat2)
nrow(old.log.dat) == nrow(old.log.dat2)

new.log.dat2$date <- ymd(new.log.dat2$date)
old.log.dat2$date <- ymd(old.log.dat2$date)

fish.dat <- dplyr::full_join(new.log.dat2, old.log.dat2)
nrow(fish.dat)==(nrow(new.log.dat2) + nrow(old.log.dat2))
#save(fish.dat, file = "C:/Users/keyserf/Documents/temp_data/fishdat_1981-2024.RData")
#load("C:/Users/keyserf/Documents/temp_data/fishdat_1981-2024.RData")

head(fish.dat)
fish.dat <- dplyr::select(fish.dat, -ves, -vrnum, -licence, -vesid)
fish.dat$trip.id <- paste0(year(fish.dat$date), ".", fish.dat$anon_id, ".", fish.dat$tripnum)
save(fish.dat, file = "C:/Users/keyserf/Documents/temp_data/fishdat_1981-2024_anon.RData")

#load("C:/Users/keyserf/Documents/temp_data/fishdat_1981-2024_anon.RData")

head(fish.dat)

##############################################################################################################
#port sampling data
# load("C:/Users/keyserf/Documents/temp_data/portdat_2006-2024.RData")
# boat name COME is repeated in off.fleet
# if year>2020, COME = 189
# if year<2020, COME = 188
port.dat$boat[year(ymd(port.dat$date))<2020 & port.dat$boat=="COME"] <- "COME1"

nrow(port.dat) == nrow(dplyr::left_join(port.dat, dplyr::select(off.fleet, boat, anon_id)))
port.dat2 <- dplyr::left_join(port.dat, dplyr::select(off.fleet, boat, anon_id))

port.dat2 <- dplyr::select(port.dat2, -boat, -port, -trip.id)
save(port.dat2, file = "C:/Users/keyserf/Documents/temp_data/portdat_2006-2024_anon.RData")

# OLD PS data
oldps <- read.csv("C:/Users/keyserf/Documents/temp_data/Old_PS_data_clean_1981-2005.csv")
unique(oldps[!oldps$boat %in% off.fleet$vesid,]$boat) 
# these aren't anywhere in off.fleet, and only match 14 records + 156 records
# check them for typos
tail(oldps[(min(which(oldps$boat == 4077))-5):(max(which(oldps$boat == 4077))+5),])
oldps[1012550:1012620,]
table(oldps[oldps$boat==4007 & year(oldps$fished)==1998,]$date, oldps[oldps$boat==4007 & year(oldps$fished)==1998,]$id)
table(oldps[oldps$boat==407,]$id, oldps[oldps$boat==4077,]$scalnum)
oldps[oldps$boat==4077,]
# pretty sure that 4077 should be 4007
oldps$boat[oldps$boat==4077] <- 4007

oldps[(min(which(oldps$boat == 4368))-5):(max(which(oldps$boat == 4368))+5),]
# should be 4369
oldps$boat[oldps$boat==4368] <- 4369

#join fleet to this
oldps$vesid <- oldps$boat
oldps2 <- left_join(select(oldps, -boat), off.fleet)
nrow(oldps)==nrow(oldps2)
oldps2 <- select(oldps2, -X, -date2, -vesid, -vrnum, -boat, -port, -ASM_date, -ring_size_2024, -gear_size_typical)

##############################################################################################################
# survey
load("C:/Users/keyserf/Documents/temp_data/Survey_all_results.Rdata")
mw.dat.all.gb <- list(GBa=mw.dat.all$GBa, 
                      GB=mw.dat.all$GB,
                      GBb=mw.dat.all$GBb)
all.surv.dat.gb <- all.surv.dat[all.surv.dat$bank %in% c("GB", "GBa", "GBb"),]

save(mw.dat.all.gb, file = "C:/Users/keyserf/Documents/temp_data/mwdatallgb.RData")
save(all.surv.dat.gb, file = "C:/Users/keyserf/Documents/temp_data/allsurvdatgb.RData")


