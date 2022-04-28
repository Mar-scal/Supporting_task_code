# Here's a script showing how to pull VMS data from Mike's Mar.utils package.
library(Mar.utils)
library(sf)
library(sp)
library(rgeos)
library(maptools)


# here are the vessel ID's we need
fleet <- read.csv("Y:/Offshore/Assessment/Data/Offshore_Fleet.csv")

# Next we grab the new VMS data. I'm pulling back to December 23rd as the 'old vms' data stops on the 22nd, probably because vessel is heading home.
start<-Sys.time()
vms.new <- VMS_get_recs(un.ID, pwd.ID, db.con,dateStart = '2015-12-23',dateEnd = '2022-04-28', 
                            usepkg = 'roracle',vrn = c(fleet$ID),rowNum =1e6,hrBuffer = 0)
print(Sys.time()-start)

vms.new <- vms.new %>% dplyr::select(-UPDATE_DATE)
names(vms.new) <- c("vessel.id","Latitude","Longitude","utc.date","speed.knots")
# Don't bother messing around with the time zone crap, just give utc.date as a character string.
#vms.new$atz.time <- with_tz(vms.new$utc.date,tz="America/Halifax") + hours(1)

# So in this load object the VMS data from 2000-2015 is found in the VMS.dat object.  This includes a wealth of data provided by Amy Glass
# from old CD's (do you know what a CD is dear reader?).
#load("F:/NAS/Projects/GB_time_area_closure_SPERA/Results/processed_VMS_2000_2015.RData")
#saveRDS(VMS.dat,file = "Y:/Offshore/Assessment/Data/Fishery_data/VMS/ALL_VMS_2000_to_2015.Rds")
# Now we can load the VMS data from 2000-2015
vms.old <- readRDS("Y:/Offshore/Assessment/Data/Fishery_data/VMS/ALL_VMS_2000_to_2015.Rds")
# Now we need to clean up the old VMS data to make it look nice and shinny like the new stuff.
vms.old <- vms.old %>% dplyr::select(vrn,lat,lon,vmstime,SPEED_KNOTS)
names(vms.old) <- c("vessel.id","Latitude","Longitude","utc.date","speed.knots")
#vms.old$utc.date <- strptime(vms.old$utc.date,format = "%Y-%m-%d %H:%M:%S")
#vms.old$utc.date <- with_tz(vms.old$utc.date,tz="Europe/London") + hours(1)

# Now combine the data
vms.all <- rbind(vms.old,vms.new)
vms.all.sf <- st_as_sf(vms.all,coords= c("Longitude","Latitude"),crs = 4326)
#saveRDS(vms.all.sf,file = "Y:/Offshore/Assessment/Data/Fishery_data/VMS/all_offshore_vms_2000_to_April_2022.Rds")


