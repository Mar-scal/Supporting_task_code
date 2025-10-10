# To get discards estimates for any species

# Go through this script line by line.

# Step 1: Set year, other report-specific things, and get observer trip metadata
require(lubridate)
require(plyr)
require(dplyr)
require(reshape2)


#direct <- "Y:/Bycatch/"
direct <- "C:/Users/keyserf/Documents/temp_data/"
direct_fns <- "C:/Users/keyserf/Documents/Github/"
direct_off <- "C:/Users/keyserf/Documents/temp_data/"
years <- 2024:2025
banks <- c("GBa","GBb")#, "BBn", "BBs", "Sab", "Ger", "Mid", "Ban")
fleet <- "offshore"
species <- c("all") ### MUST MATCH ISDB COMMON NAME EXACTLY
##### examples:
# "AMERICAN PLAICE"   "COD(ATLANTIC)"   "WINTER FLOUNDER", "WITCH FLOUNDER", "SUMMER FLOUNDER", "YELLOWTAIL FLOUNDER", 
# "FOURSPOT FLOUNDER", "FLOUNDER UNIDENTIFIED", "GULF STREAM FLOUNDER"
# "LITTLE SKATE", "THORNY SKATE", "SMOOTH SKATE", "WINTER SKATE", 
# "BARNDOOR SKATE", "WHITE SKATE", "ROUND SKATE", "JENSEN'S SKATE", 
# "SPINYTAIL SKATE", "SKATES (NS)", "SKATE UNID. EGGS", "SKATE/SHARK EGG PURSE - LIVE"
# "HADDOCK"
# OR "all"
# species <- "all"

## We use the sql pulled data to make sure that we captured every possible trip (UPDATE THE DATE!)
source(paste0(direct_fns, "Bycatch_fns/obs_metadata.R"))
obs_metadata(direct=paste0(direct, "data"), # where do you want the data to be saved?
             direct_off=direct_off,
             direct_off_fns = direct_fns,
             direct_fns=direct_fns,# where do you want the logs and assessment_fns to come from?
             year=years, # for what years?
             pullfromISDB=TRUE, # do you want to pull fresh data from the ISDB? (yes=true, no=false)
             un=un.ID, pw=pwd.ID, db.con="ptran", package="ROracle", # if you pullfromISDB, specify these arguments
             sqlexport = NULL, # if pullfromISDB is false, specify the name of the file you want to import
             writecsv=TRUE, # do you want to save out these results in CSVs? (yes=true, no=false)
             polys=fleet, # specify the fleet (inshore or offshore or both)
             checkspatial=TRUE) # do you want to run spatial checks? If TRUE, produces PDF images and also additional CSV files.

obs.trips <- read.table(paste0(direct, "data/Observed scallop trip metadata_", fleet, "_2025-08-21.csv",sep=""), sep=",",header=T,stringsAsFactors = F)

### tidy up the observer metadata
names(obs.trips) <- c("X", "trip", "vrnum", "date.sailed", "date.land", "latitude", "longitude", "comarea_id", "set_no")
obs.trips <- unique(select(obs.trips, trip, vrnum, date.sailed, date.land, comarea_id, set_no))
obs.trips <- obs.trips[year(obs.trips$date.sailed)%in% years,]
obs.trips <- arrange(obs.trips, date.land, trip, comarea_id)
head(obs.trips, n=20)

obs.trips <- arrange(obs.trips, date.land)
tail(obs.trips, n=20)

## Make any manual corrections now: 

## NOTE: trip J18-0058 was interrupted due to weather, and the wrong land date is in the ISDB.
# obs.trips$date.land[obs.trips$trip=="J18-0058"] <- "2018-03-12"

## NOTE: trip J18-0408 and J18-0337 are very weird. This doesn't need to go to Javitech because the ISDB dates are the full trip dates
# obs.trips$date.land[obs.trips$trip=="J18-0337" & obs.trips$comarea_id=="SF27B"] <- "2018-09-16"
# obs.trips$date.sailed[obs.trips$trip=="J18-0337" & obs.trips$comarea_id=="SF27A"] <- "2018-09-17"

## Note: trip J19-0075 COMAREA_ID should be SF27A
# obs.trips$comarea_id[obs.trips$trip=="J19-0075"] <- "SF27A"

# Note: Q3 2019. Trip J19-0449 has missing coordinates for many sets, but trip was on GBa. 
# obs.trips[obs.trips$trip == "J19-0449",]$comarea_id <- "SF27A"
# obs.trips <- unique(obs.trips)

## NOTE: Q4 2019. Trip J19-0477B has wrong land date is in the ISDB.
# obs.trips$date.land[obs.trips$trip=="J19-0477B"] <- "2019-10-27"

## NOTE: Q1 2020. Trip J20-0052 has wrong board date in the ISDB.
# obs.trips$date.sailed[obs.trips$trip=="J20-0052"] <- "2020-01-23" # this is in logs, and it makes a diff since it places the trip in Jan instead of Feb.
# 


# We'll use this df to help match old vessel IDs to new vnums (FOR BOTH FLEETS)
vessel <- read.csv(paste0(direct_off,"Data/Offshore_Fleet.csv"))

# FOR OFFSHORE ONLY: (skip to line 185 for Inshore. You'll have to work up your log data elsewhere!)
# Step 2: Calculate effort for each trip from log and slip data

## Join logs and slips. Please grab these from Y:\Offshore scallop\Assessment\Data\Fishery_data\Logs and \Slips and move them into 
## the Bycatch folder so that you don't accidentally ruin all Trish's hard work! 
## You need these to get the dates on each bank/area and to calculate effort properly

###############################
if(fleet=="offshore"){
  # source("C:/Documents/Offshore scallop/Assessment/Assessment_fns/Fishery/logs_and_fishery_data.r")
  # direct.log="Y:/Offshore scallop/Assessment/"
  # logs_and_fish(loc="offshore",year = 2004:2018,un=un.ID,pw=pwd.ID,db.con="ptran", direct=direct.log)
  
  ### compile log data
  source(paste0(direct_fns, "Assessment_fns/Fishery/logs_and_fishery_data.R"))
  logs_and_fish(loc="offshore",year = years, direct=direct_off, export = F, direct_fns=direct_fns_off, get.marfis = F) # from prelim log csvs
  
  new.log.dat$year <- year(new.log.dat$fished)
  new.log.dat$month <- month(new.log.dat$fished)
  new.log.dat$date <- ymd(new.log.dat$date)
  new.log.dat$ID<-1:nrow(new.log.dat)
  if(exists("old.log.dat")) {
    old.log.dat$date <- ymd(old.log.dat$date)
    old.log.dat$ID<-1:nrow(old.log.dat)
  }
  
  ## Get TRIP_IDs for observed trips based on fishing dates
  # If you get an issue here, it's likely because you're missing the log or slip for the observer trip. Open up 2018log.csv and look up the trip in question.
  TRIP_IDs <- NULL
  for (i in 1:length(unique(obs.trips$trip))){
    print(i)
    obs <- obs.trips[obs.trips$trip==unique(obs.trips$trip)[i],]
    
    if(unique(year(obs$date.sailed)) < 2009) {
      vesid <- vessel[vessel$ID == unique(obs$vrnum),]$Pre_2008_ID
      vrnum <- old.log.dat[old.log.dat$vesid == vesid,]
      aftersail <- vrnum[(vrnum$date > unique(obs$date.sailed)) | (vrnum$date == unique(obs$date.sailed)),]
      beforeland <- aftersail[(aftersail$date < unique(obs$date.land)) | (aftersail$date == unique(obs$date.land)),] 
      trip.id <- as.character(unique(beforeland$trip.id))
    }
    
    if(unique(year(obs$date.sailed)) > 2008) {
      vrnum <- new.log.dat[new.log.dat$vrnum == unique(obs$vrnum),]
      aftersail <- vrnum[(vrnum$fished > unique(obs$date.sailed)) | (vrnum$fished == unique(obs$date.sailed)),]
      beforeland <- aftersail[(aftersail$fished < unique(obs$date.land)) | (aftersail$fished == unique(obs$date.land)),] 
      trip.id <- as.character(unique(beforeland$trip.id))
    }
    
    if(length(trip.id) > 0){
      TRIP_IDs <- rbind(TRIP_IDs, data.frame(trip.id, trip=unique(obs.trips$trip)[i]))
    }
    
    if(length(trip.id) ==0){
      message("This TRIP ID does not have a matching log record:")
      print(unique(select(obs, trip, vrnum, date.sailed, date.land, comarea_id)))
    }  
  }
  
  ### warning messages are ok as long as:
  length(unique(TRIP_IDs$trip.id[!is.na(TRIP_IDs$trip.id)]))# is similar to:
  length(unique(TRIP_IDs$trip)) 
  dups2 <- data.frame(table(TRIP_IDs$trip))[data.frame(table(TRIP_IDs$trip))$Freq>1,]
  arrange(TRIP_IDs[TRIP_IDs$trip %in% dups2$Var1,], trip)
  dups3 <- data.frame(table(TRIP_IDs$trip.id))[data.frame(table(TRIP_IDs$trip.id))$Freq>1,]
  arrange(TRIP_IDs[TRIP_IDs$trip.id %in% dups3$Var1,], trip.id)
  
  ## join observer trip IDs to efforts
  effort_obs_new <- join(new.log.dat, TRIP_IDs, type="left")
  effort_obs <- effort_obs_new[, c("trip", "trip.id", "vrnum", "year", "fished", "watch", "date.land", "date.sail", "bank", "sfa", "nafo", "hm")]
  if(exists("old.log.dat")) {
    effort_obs_old <- join(old.log.dat, TRIP_IDs, type="left")
    
    ## need to assign date.land based on date for old trips. 
    dates <- ddply(.data=effort_obs_old, .(trip.id),
                   summarize,
                   date.land=max(date),
                   date.sail = min(date))
    effort_obs_old <- join(effort_obs_old, dates, type="left")
    
    ## join these with the relevant columns only
    effort_obs <- join(effort_obs_new[, c("trip", "trip.id", "vrnum", "year", "fished", "watch", "date.land", "date.sail", "bank", "sfa", "nafo", "hm")], 
                       effort_obs_old[, c("trip", "trip.id", "vesid", "year", "date","date.land","date.sail", "bank", "nafo", "hm")], type="full")
    nrow(effort_obs_new) + nrow(effort_obs_old) == nrow(effort_obs)
  }
  # above must be TRUE to continue.
  
  effort_obs$fished[is.na(effort_obs$fished)] <- effort_obs$date[is.na(effort_obs$fished)]
  
  effort_obs$bank[effort_obs$bank %in% c("Sab", "Mid")] <- "SabMid"
  
  # ## spatial subset if needed
  # if(!is.null(banks)) {
  #   if(any(c("Sab", "Mid") %in% banks)) effort_obs <- effort_obs[effort_obs$bank %in% c(banks, "SabMid"),]
  #   if(!any(c("Sab", "Mid") %in% banks)) effort_obs <- effort_obs[effort_obs$bank %in% banks,]
  # }
  
  ## use this to make sure you don't have lots of duplicates (if you see 1's and 0's only, great. If you see 2's or other numbers, check them out):
  checklist <- list()
  for (i in 1:length(unique(obs.trips$trip))){
    trip <- unique(obs.trips$trip)[i]
    table <- table(effort_obs[effort_obs$trip %in% unique(obs.trips$trip)[i],]$fished, effort_obs[effort_obs$trip %in% unique(obs.trips$trip)[i],]$watch)
    checklist[[trip]] <- table
    
    if(any(apply(table, 2, FUN = function(x) any(x > 1))==TRUE)) {
      print(trip)
      print(table)
    }
  }
  
  length(checklist) == length(unique(obs.trips$trip))
  # must be true
  
  ## NOTE: deal with weird Q3 trips J18-0408 and J18-0337 with TRIP_ID 465670 (two observer trips for one trip ID). This doesn't require ISDB change.
  # effort_obs <- effort_obs[!(effort_obs$sfa %in% "27A" & effort_obs$trip %in% "J18-0408"),]
  # effort_obs <- effort_obs[!(effort_obs$trip %in% "J18-0408" & effort_obs$fished > "2018-09-07"),]
  # table(effort_obs[effort_obs$trip %in% c("J18-0337"),]$sfa, 
  #       effort_obs[effort_obs$trip %in% c("J18-0337"),]$fished)
  # table(effort_obs[effort_obs$trip %in% c("J18-0408"),]$sfa, 
  #       effort_obs[effort_obs$trip %in% c("J18-0408"),]$fished)
  
  # NOTE: deal with weird Q3 J18-0355 trip. We want to remove the A trip from the effort calculation since the log only had one trip (whereas ISDB had an A and B trip)
  # effort_obs <- effort_obs[!(effort_obs$trip %in% "J18-0355A" & effort_obs$sfa=="27A"),]
  
  if(exists("old.log.dat")){vesidvrnum <- unique(effort_obs[,c("trip", "vrnum", "vesid")])}
  if(!exists("old.log.dat")){vesidvrnum <- unique(effort_obs[,c("trip", "vrnum")])}
  
  ## aggregate to get an effort for each TRIP_ID 
  effort_obs <- arrange(
    aggregate(data=effort_obs, hm ~ trip + date.land + bank + nafo + year, sum),
    date.land, bank)
  
  ## overall effort by observed trip!
  effort_sum <- arrange(aggregate(data=effort_obs, hm ~ trip + year + bank, sum), trip)
  effort_sum <- join(effort_sum, vesidvrnum, type="left")
  ### CHECK THE RECORDS ABOVE FOR EACH OBSERVED TRIP IN EXCEL LOGS AND SLIPS!! MAKE SURE THEY MATCH!
  # gear width by trip to be used for manual checks:
  if(exists("effort_obs_old")) unique(effort_obs_old[effort_obs_old$trip %in% TRIP_IDs$trip, c("trip.id", "trip", "vesid", "gear.ft", "date.sail", "date.land")])
  unique(effort_obs_new[effort_obs_new$trip %in% TRIP_IDs$trip, c("trip.id", "trip", "vrnum", "gear.ft", "date.sail", "date.land")])
  
  effort_sum_yr <- ddply(.data=effort_sum, .(year),
                         summarize,
                         hm=sum(hm))
}
########################################

# FOR OFFSHORE AND INSHORE:
# step 3: read in discards data and run checks using detailed getdiscards output and gethooks output
source(paste0(direct_fns, "Assessment_fns/Other_functions/ScallopQuery.r"))
source(paste0(direct_fns, "Bycatch_fns/getdiscards.r"))

### reading the discards data from ISDB
discards <- NULL
#years <- c(year-1, year)
for (i in 1:length(years)) {
  discards_y <- getdiscards_long_detail(year=years[i], un = un.ID, pw = pwd.ID, db.con = "ptran", direct.off = direct_off, package="ROracle") # must use getdiscards_long in order to deal with split trips properly
  discards <- rbind(discards, discards_y)
}  

tail(discards, 20)

## quickly plot these up
# source(paste0(direct_fns, "Assessment_fns/Maps/pectinid_projector_sf.R"))
# plotted <- pecjector(repo="github", add_layer=list(
#   bathy=c(100, 'c', 1000),
#   eez="eez",
#   nafo="main",
#   sfa="offshore",
#   scale.bar = c("br", 0.3)),
#   direct_fns="C:/Users/keyserf/Documents/Github/FK/Assessment_fns/",
#   plot_package = "ggplot", plot = F)
# 
# if(!species=="all"){
#   plotted <- plotted + geom_point(data=discards, aes(-LONGITUDE, LATITUDE))
#   require(plotly)
#   ggplotly(plotted)
# }

### reading the hooks data from ISDB
hooks <- NULL
for (i in 1:length(years)) {
  hooks_y <- gethooks_long_detail(year=years[i], un = un.ID, pw = pwd.ID, db.con = "ptran", direct.off = direct, package="ROracle") # must use getdiscards_long in order to deal with split trips properly
  hooks <- rbind(hooks, hooks_y)
}  

tail(hooks, 20)

nahooks <- unique(hooks[is.na(hooks$TOTAL_HOOKS),c("TRIP", "SET_NO")])
nahooks$remove <- "bad"

discards <- left_join(discards, nahooks)
discards <- discards[is.na(discards$remove),]
hooks <- hooks[!is.na(hooks$TOTAL_HOOKS),]
dim(hooks)

###  this is the function we need to do the checks and easy fixes
source(paste0(direct_fns, "Bycatch_fns/checkdiscards.r"))
checkdiscards(data=discards, hooksdata = hooks, 
              getdiscards_type = "getdiscards_long_detail", gethooks_type = "gethooks_long_detail", species="all",
              fix=FALSE)

# manual fixes
# remove kept thunnus - suspect this should have been 4321, not 321.
discards <- discards[!(discards$TRIP=="J24-0185" & discards$SPECCD_ID==321),]
# the others don't really matter because I'm only reporting DISCARDS not kept. 

# To fix source and select other issues automatically set fix to TRUE: 
fixed <- checkdiscards(data=discards, hooksdata = hooks, 
                       getdiscards_type = "getdiscards_long_detail", gethooks_type = "gethooks_long_detail", species="all",#species="PORBEAGLE,MACKEREL SHARK",
                       fix=TRUE, output="same")

# source issues are fixed automatically!
# fixed$data_fixed[paste0(fixed$data_fixed$TRIP, "-", fixed$data_fixed$SET_NO) %in% fixed$data_fixed_trips,]
# TO INFORM JAVITECH (these trip-sets should be Source 0, observed, not source 1.): 
fixed$data_fixed_trips
fixed$hooksdata_fixed_trips

discards <- dplyr::select(fixed$data_fixed, -COMAREA_ID)
#only take pntcd 2 after checking that there is an equal number of records in 2 and 3
table(fixed$hooksdata_fixed$PNTCD_ID)[[1]]==table(fixed$hooksdata_fixed$PNTCD_ID)[[2]]
hooks <- fixed$hooksdata_fixed[fixed$hooksdata_fixed$PNTCD_ID==2,]

# enter species name here if you figured it out partway through
#species <- "WINTER FLOUNDER"

if(!species=="all"){discards <- filter(discards, COMMON %in% species)}

# remove trips that are not by offshore vessels
inshoretrips <- hooks[!(hooks$VRN %in% vessel$ID) & !(hooks$VRN %in% vessel$VMS_old_ID) & !(hooks$VRN %in% vessel$Pre_2008_ID), c("TRIP", "COMAREA_ID")]
offshoretrips <- hooks[(hooks$VRN %in% vessel$ID) | (hooks$VRN %in% vessel$VMS_old_ID) | (hooks$VRN %in% vessel$Pre_2008_ID), c("TRIP", "COMAREA_ID")]

if(fleet=="offshore"){ 
  discards <- discards[discards$TRIP %in% offshoretrips$TRIP,]
  hooks <- hooks[hooks$TRIP %in% offshoretrips$TRIP,]
}
if(fleet=="inshore"){ 
  discards <- discards[discards$TRIP %in% inshoretrips$TRIP,]
  hooks <- hooks[hooks$TRIP %in% inshoretrips$TRIP,]
}

# head(hooks)
# head(discards)

# join the spatially checked obs_metadata stuff to assign bank to comarea_ids
set_coord <- read.csv(paste0(direct, "data/Observed scallop trip metadata_tidysets_2025-08-21.csv"))

# do the manual edits from bycatchreport_7.R as needed
# I.e. review Spatial checks.pdf and remove sets that are outside of your desired area. 
set_coord <- set_coord[as.numeric(substr(set_coord$TRIP, start = 2, stop=3))>as.numeric(substr(years[1]-1, 3,4)),]
set_coord <- set_coord[as.numeric(substr(set_coord$TRIP, start = 2, stop=3))<as.numeric(substr(max(years)+1, 3,4)),]
set_coord$tripset <- paste0(set_coord$TRIP, "-",set_coord$SET_NO)
badspatial <- set_coord[(set_coord$TRIP=="J14-0111" & set_coord$area=="SFA26A.shp"),]$tripset
badspatial <- c(badspatial, set_coord[(set_coord$TRIP=="J14-0522" & set_coord$area=="SFA26B.shp"),]$tripset)
badspatial <- c(badspatial, set_coord[(set_coord$TRIP=="J14-0587" & set_coord$area=="SFA26A.shp"),]$tripset)
badspatial <- c(badspatial, set_coord[(set_coord$TRIP=="J16-0122" & set_coord$area=="SFA26A.shp"),]$tripset)
badspatial <- c(badspatial, set_coord[(set_coord$TRIP=="J16-0576" & set_coord$area=="SFA25A.shp"),]$tripset)
badspatial <- c(badspatial, set_coord[(set_coord$TRIP=="J17-0126" & set_coord$area=="SFA26B.shp"),]$tripset)

discards$tripset <- paste0(discards$TRIP, "-", discards$SET_NO)
hooks$tripset <- paste0(hooks$TRIP, "-", hooks$SET_NO)
discards <- discards[!discards$tripset %in% badspatial,]
hooks <- hooks[!hooks$tripset %in% badspatial,]

# discards that are not on GB even though fishing was supposedly on GB
discards <- discards %>%
  filter(!(TRIP=="J24-0007" & 
             SET_NO %in% c(1,2,3,37)))

hooks <- hooks %>%
  filter(!(TRIP=="J24-0007" & 
             SET_NO %in% c(1,2,3,37)))

discards$tripset <- as.character(paste0(discards$TRIP, ".", discards$SET_NO))
hooks$tripset <- as.character(paste0(hooks$TRIP, ".", hooks$SET_NO))
set_coord$tripset <- as.character(paste0(set_coord$TRIP, ".", set_coord$SET_NO))

discards_sets <- join(dplyr::select(set_coord[year(ymd(set_coord$LANDING_DATE)) %in% years,], -X), discards, by="tripset", type="full")
hooks_sets <- join(dplyr::select(set_coord[year(ymd(set_coord$LANDING_DATE)) %in% years,], -X), hooks, by="tripset", type="full")

# get observed and total hooks by area by trip
hooks_area_trip_obs <- arrange(ddply(.data=hooks_sets[hooks_sets$SOURCE==0,], .(TRIP, area, VESSEL_NAME),
                                     summarize,
                                     obshooks = sum(TOTAL_HOOKS, na.rm=T)), TRIP)
hooks_area_trip_tot <- arrange(ddply(.data=hooks_sets, .(TRIP, area, VESSEL_NAME),
                                     summarize,
                                     totalhooks = sum(TOTAL_HOOKS, na.rm=T)), TRIP)
hooks_area_trip_tot <- hooks_area_trip_tot[hooks_area_trip_tot$totalhooks>0,]

hooks_area_trip <- join(hooks_area_trip_tot, hooks_area_trip_obs, type="full")
if(dim(hooks_area_trip[is.na(hooks_area_trip$obshooks),])[1] >0) hooks_area_trip[is.na(hooks_area_trip$obshooks),]$obshooks <- 0

# use fishery data to fill in NA areas where possible
# fix fishery names first
if("vesid" %in% names(effort_sum)) names(effort_sum) <- c("TRIP", "year", "bank", "hm", "vrnum", "vesid")
if(!"vesid" %in% names(effort_sum)) names(effort_sum) <- c("TRIP", "year", "bank", "hm", "vrnum")
# get the na's only
hooks_area_trip_na <- left_join(hooks_area_trip[is.na(hooks_area_trip$area),], effort_sum[, c("TRIP", "bank")])
# careful with split trips that have NAs, impossible to attribute bank from log to specific set without lat/lon
splits <- hooks_area_trip_na$TRIP[duplicated(hooks_area_trip_na$TRIP)]
splits_na <- hooks_area_trip_na[hooks_area_trip_na$TRIP == splits,]
if(all(banks==c("GBa", "GBb"))) splits_na <- splits_na[1,] #assume GBa for these since they're going to be summed anyway
if(!all(banks==c("GBa", "GBb"))) splits_na <- NULL
hooks_area_trip_na <- rbind(hooks_area_trip_na[!hooks_area_trip_na$TRIP %in% splits,], splits_na)
hooks_area_trip <- left_join(hooks_area_trip, hooks_area_trip_na)
hooks_area_trip$area[is.na(hooks_area_trip$area)] <- hooks_area_trip$bank[is.na(hooks_area_trip$area)]
hooks_area_trip <- dplyr::select(hooks_area_trip, -bank)

#use bank for area
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA27A.shp", replacement = "GBa")
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA27B.shp", replacement = "GBb")
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA26A.shp", replacement = "BBn")
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA26B.shp", replacement = "BBs")
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA26C.shp", replacement = "Ger")
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA25A.shp", replacement = "SabMid")
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA25B.shp", replacement = "Ban")
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA10.shp", replacement = "SPB")
hooks_area_trip$area <- gsub(x=hooks_area_trip$area, pattern = "SFA11.shp", replacement = "SPB")

hooks_area_trip <- hooks_area_trip %>%
  group_by(TRIP, area, VESSEL_NAME) %>%
  summarize(totalhooks=sum(totalhooks, na.rm=T), 
            obshooks=sum(obshooks, na.rm=T))

#do it again to deal with bad coords (trust logs over ISDB)
hooks_area_trip <- left_join(hooks_area_trip, effort_sum)
hooks_area_trip$area <- hooks_area_trip$bank
  
hooks_area_trip <- hooks_area_trip %>%
  group_by(TRIP, area, VESSEL_NAME) %>%
  summarize(totalhooks=sum(totalhooks, na.rm=T), 
            obshooks=sum(obshooks, na.rm=T))

# do something similar for discards 
discards_sets_na <- left_join(discards_sets[is.na(discards_sets$area),], effort_sum[, c("TRIP", "bank")])
splits <- unique(discards_sets_na$TRIP[duplicated(dplyr::select(discards_sets_na, -bank))])
discards_sets_na <- discards_sets_na[!discards_sets_na$TRIP %in% splits,]
discards_sets <- left_join(discards_sets, discards_sets_na)
discards_sets[discards_sets$tripset %in% discards_sets_na$tripset,]$area <- discards_sets[discards_sets$tripset %in% discards_sets_na$tripset,]$bank
discards_sets <- dplyr::select(discards_sets, -bank)

# use bank for area
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA27A.shp", replacement = "GBa")
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA27B.shp", replacement = "GBb")
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA26A.shp", replacement = "BBn")
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA26B.shp", replacement = "BBs")
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA26C.shp", replacement = "Ger")
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA25A.shp", replacement = "SabMid")
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA25B.shp", replacement = "Ban")
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA10.shp", replacement = "SPB")
discards_sets$area <- gsub(x=discards_sets$area, pattern = "SFA11.shp", replacement = "SPB")

# Map for Heather
#discards_sets
## get EEZ
tem <- tempfile()
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/EEZ/EEZ.zip",tem)
tem2 <- tempfile()
unzip(zipfile = tem,exdir = tem2)
eez.CAN <- sf::st_read(paste0(tem2,"/EEZ.shp"))
rm(tem,tem2)
##land layer
land.all <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf",continent = "North America")
require(sf)
discards_sf <- st_as_sf(x = discards_sets, coords=c(X="LONGITUDE", Y="LATITUDE"), crs=4326)

require(ggplot2)
#png("Y:/Bycatch/requests/Porbeagle_DFO/Map_OffshoreScallop_Porbeagle.png", res =  400, height=3, width=4.5, units="in")
# ggplot() + 
#   geom_sf(data=eez.CAN, colour="lightblue")+
#   geom_sf(data=land.all) +
#   geom_sf(data=discards_sf[!is.na(discards_sf$COMMON),]) +
#   coord_sf(xlim = c(-70, -45), ylim = c(40, 49), expand=F) +
#   ggtitle("Offshore Scallop") 
#dev.off() 

# get discards by area by trip
discards_area_trip_common <- arrange(ddply(.data=discards_sets[discards_sets$SOURCE==0,], .(TRIP, area, COMMON),
                                           summarize,
                                           totaldiscards = sum(`SUM(EST_DISCARD_WT)`, na.rm=T)), TRIP)

#do it again to deal with bad coords (trust logs over ISDB)
discards_area_trip_common <- left_join(discards_area_trip_common, effort_sum)
discards_area_trip_common$area <- discards_area_trip_common$bank

discards_area_trip_common <- arrange(ddply(.data=discards_area_trip_common, .(TRIP, area, COMMON),
                                           summarize,
                                           totaldiscards = sum(totaldiscards)), TRIP)

discards_area_trip_common[discards_area_trip_common$TRIP=="J25-0075",]

# join discards and hooks
discards_hooks_sum <- join(hooks_area_trip, discards_area_trip_common, type="left")
names(discards_hooks_sum)[which(names(discards_hooks_sum)=="area")] <- "bank"
discards_hooks_sum <- discards_hooks_sum[!is.na(discards_hooks_sum$TRIP),]


# join to effort!
discards_effort <- join(effort_sum, discards_hooks_sum, type="left")
discards_effort_2 <- join(discards_hooks_sum, effort_sum, type="left")
discards_effort_2_lost <- unique(discards_effort_2[is.na(discards_effort_2$hm),]$TRIP)
lost <- NULL
for(i in discards_effort_2_lost){
  temp <- discards_effort_2[discards_effort_2$TRIP==i,]
  temp2 <- temp[is.na(temp$hm),]
  temp <- temp[!is.na(temp$hm),]
  if(length(unique(temp$hm[!is.na(temp$hm)])) == 1) {
    #assume that effort was all in one area, and bank with NA effort is wrong. 
    totalhooks <- sum(unique(temp2[,c("TRIP", "bank", "totalhooks")]$totalhooks))
    obshooks <- sum(unique(temp2[,c("TRIP", "bank", "obshooks")]$obshooks))
    temp$totalhooks <- temp$totalhooks + totalhooks
    temp$obshooks <- temp$obshooks + obshooks
    
    # temp$bank <- temp[!is.na(temp$hm),]$bank
    # temp$year <- temp[!is.na(temp$hm),]$year
    # temp$vrnum <- temp[!is.na(temp$hm),]$vrnum
    # temp <- temp %>% group_by(TRIP, bank, VESSEL_NAME, COMMON, year, vrnum) %>%
    #   summarize(totalhooks=sum(totalhooks),
    #             obshooks=sum(obshooks),
    #             totaldiscards=sum(totaldiscards, na.rm=T),
    #             hm=sum(hm, na.rm=T))
    lost <- rbind(lost, temp)
  }
  if(length(unique(temp$hm[!is.na(temp$hm)])) > 1) {
    # too complicated! can't do anything here
  }
}
discards_effort_2_keep <- discards_effort_2[!discards_effort_2$TRIP %in% lost$TRIP,]
discards_effort <- full_join(discards_effort_2_keep, lost)
#if(dim(discards_effort)[1] < dim(discards_effort_2)[1]) print("may be missing logs, unless you know that the ISDB contains more trips than you're doing in this report.")

# Step 5: join discards to hooks hauled
#names(discards_hooks_sets) <- c("vessel_name", "vrnum","trip", "comarea_id", "SOURCE", "TOTAL_HOOKS")

## NOTE: comarea_id for J18-0183 is missing. Should notify Javitech. Fill it in:
# hooks$comarea_id[hooks$trip=="J18-0183"] <- "SF27A"
# discards_effort$COMAREA_ID[discards_effort$TRIP=="J18-0183"] <- "SF27A"
# 
# ## NOTE: comarea_id for J19-0075 is missing. Should notify Javitech. Fill it in:
# hooks$comarea_id[hooks$trip=="J19-0075"] <- "SF27A"
# discards_effort$COMAREA_ID[discards_effort$TRIP=="J19-0075"] <- "SF27A"

# if(!is.null(comarea_ids)) hooks <- hooks[hooks$COMAREA_ID %in% comarea_ids,]

if("vesid" %in% names(discards_effort)) discards_report <- dplyr::select(discards_effort, VESSEL_NAME, bank, vesid, vrnum, TRIP, obshooks, totalhooks, COMMON, totaldiscards, hm)
if(!"vesid" %in% names(discards_effort)) discards_report <- dplyr::select(discards_effort, VESSEL_NAME, bank, vrnum, TRIP, obshooks, totalhooks, COMMON, totaldiscards, hm)
names(discards_report)[which(names(discards_report) == "totaldiscards")] <- "discards"

if(!is.null(banks)) {
  if(any(c("Sab", "Mid") %in% banks)) discards_report <- discards_report[discards_report$bank %in% c(banks, "SabMid"),]
  if(!any(c("Sab", "Mid") %in% banks)) discards_report <- discards_report[discards_report$bank %in% banks,]
}

# fill in the vesid and vrnum columns with the opposite, just cuz
discards_report$vrnum[is.na(discards_report$vrnum)] <- discards_report$vesid[is.na(discards_report$vrnum)]

if("vesid" %in% names(discards_effort)) discards_report <- select(discards_report, -vesid)

#discards_report$discards[is.na(discards_report$discards)] <- 0

# discards_report$year <- substr(discards_report$TRIP, 2,3)
# discards_report[discards_report$year==25 & discards_report$COMMON %in% c("YELLOWTAIL FLOUNDER","COD(ATLANTIC)", "HADDOCK"),] %>%
#   tidyr::pivot_wider(names_from=COMMON, values_from=discards) %>%
#   View()
# 
discards_report <- discards_report %>% 
   arrange(TRIP, COMMON) %>%
   tidyr::pivot_wider(names_from=COMMON, values_from=discards, names_sort = T)

# SUMMING UP THE DISCARDS BY TRIP
discards_report <- discards_report %>% 
  dplyr::group_by(VESSEL_NAME, vrnum, TRIP, bank) %>%
  summarize(across(everything(), function(x) sum(x, na.rm=T)))%>%
  arrange(TRIP) %>%
  as.data.frame()

discards_report$prophooks <- discards_report$obshooks/discards_report$totalhooks

save(discards_report, file = paste0(direct, "data/discards_report_", species, "_", Sys.Date(), ".Rdata"))

rm(discards_report)

load(paste0(direct, "data/discards_report_", species, "_", Sys.Date(), ".Rdata"))

# This should be pretty close to accurate. It takes some information from fishery logs to fill in bad lat/lons. 

# tail(discards_report,20)
## COMPARE THESE TO THE OTIS REPORTS!!!

# Step 5: get the dates

startdate <- aggregate(data=obs.trips, date.sailed ~ trip, min)
enddate <- aggregate(data=obs.trips, date.land ~ trip, max)

names(startdate)[which(names(startdate)=="trip")] <- "TRIP"
names(enddate)[which(names(enddate)=="trip")] <- "TRIP"

discards_report <- join(discards_report, startdate, type="left")
discards_report <- join(discards_report, enddate, type="left")

discards_report <- arrange(discards_report, date.land)

# Step 6: assign months to all trips in discards report

discards_report$startmonth <- month(discards_report$date.sailed)
discards_report$endmonth <- month(discards_report$date.land)
discards_report$ndays_1 <- ifelse((discards_report$startmonth==discards_report$endmonth)==F,
                                  difftime((floor_date(ymd(discards_report$date.land), unit="month")-days(1)),
                                           ymd(discards_report$date.sailed), units="day"), NA)
discards_report$ndays_2 <- ifelse((discards_report$startmonth==discards_report$endmonth)==F, day(ymd(discards_report$date.land)), NA)
discards_report$month <- as.numeric(ifelse((discards_report$startmonth==discards_report$endmonth)==T,
                                           month(ymd(discards_report$date.land)),
                                           ifelse(discards_report$ndays_1 > discards_report$ndays_2,
                                                  month(ymd(discards_report$date.sailed)),
                                                  month(ymd(discards_report$date.land)))))
discards_report$year <- year(discards_report$date.land)
discards_report <- dplyr::arrange(discards_report, year, date.land)

species_cols <- which(!names(discards_report) %in% c("VESSEL_NAME", "TRIP", "obshooks", "vrnum", "totalhooks", "hm", 
                                                     "bank","prophooks", "date.sailed", "date.land", "startmonth", "endmonth",
                                                     "ndays_1","ndays_2", "month", "year"))

monthly_eff <- discards_report %>%
  group_by(year, month, bank) %>%
  summarize(obshooks = sum(obshooks, na.rm=T),
            totalhooks = sum(totalhooks, na.rm=T),
            obseffort = sum(hm, na.rm=T),
            across(species_cols, sum)) %>%
  ungroup() %>% as.data.frame()

monthly_eff$prophooks <- monthly_eff$obshooks/monthly_eff$totalhooks

species_cols <- which(!names(monthly_eff) %in% c("bank","year", "month", "obshooks", "totalhooks", "obseffort", "prophooks"))
monthly_eff <- monthly_eff %>%
  mutate(across(species_cols, ~.x / prophooks)) %>%
  relocate(prophooks, .after = totalhooks)

write.csv(x = monthly_eff, file = "C:/Users/keyserf/Documents/temp_data/alldiscards.csv")

# Step 6: summarize fishing effort
#gba 2017 7 2018 3
new.log.dat$bank[new.log.dat$bank %in% c("Sab", "Mid")] <- "SabMid"
fleeteffort_new <- ddply(.data=new.log.dat,
                         .(year, month, bank),
                         summarize,
                         totaleffort = sum(hm, na.rm=T))
fleeteffort <- fleeteffort_new

if(exists("old.log.dat")) {
  old.log.dat$bank[old.log.dat$bank %in% c("Sab", "Mid")] <- "SabMid"
  fleeteffort_old <- ddply(.data=old.log.dat,
                           .(year, month, bank),
                           summarize,
                           totaleffort = sum(hm, na.rm=T))
  fleeteffort <- rbind(fleeteffort_old, fleeteffort_new)
}
fleeteffort$month <- as.numeric(fleeteffort$month)
fleeteffort$year <- as.numeric(fleeteffort$year)
monthly_eff$year <- as.numeric(monthly_eff$year)
monthly_eff$month <- as.numeric(monthly_eff$month)

monthly <- left_join(monthly_eff, fleeteffort)
if(all(length(banks)>1 | !banks=="all")) {
  monthly <- monthly[monthly$bank %in% banks,]
  monthly$bank <- "selected"
}
species_cols <- which(!names(monthly) %in% c("year", "month", "obshooks", "totalhooks", "obseffort", "prophooks", "bank", "totaleffort"))

monthly <- monthly %>%
  tidyr::pivot_longer(cols = species_cols, names_to="COMMON", values_to="discards")

monthly <- monthly %>% group_by(year, month, bank, COMMON, discards) %>%
  summarize(obshooks = sum(obshooks),
            totalhooks=sum(totalhooks),
            obseffort=sum(obseffort),
            totaleffort=sum(totaleffort)) %>% 
  mutate(discardrate = zoo::rollsum(discards,3, fill=NA)/zoo::rollsum(obseffort, 3, fill=NA))
##### got stuck with moving average stuff... to do!
monthly$totaldiscards <- monthly$discardrate*monthly$totaleffort

totaldiscards <- monthly %>% 
  select(-discards, -discardrate) %>%
  tidyr::pivot_wider(names_from=COMMON, values_from=totaldiscards)
totaldiscards$HADDOCK

## this code figures it out by trip, but we'll do this in the Rmd file later, so IGNORE:
# tripunique <- ddply(.data=effort, .(TRIP_ID, VR_NUMBER, FISHING_AREA),
#                     summarize,
#                     date.sailed=min(ymd(DATE_SAILED)),
#                     date.land=max(ymd(LANDING_DATE_TIME)))
# 
# # deal with split trips. Use date sailed and max date fished for the first portion, and min date fished and date land for the second portion.
# tripIDs <- data.frame(table(tripunique$TRIP_ID))
# splitIDs <- tripunique$TRIP_ID[tripunique$TRIP_ID %in% c(as.character(tripIDs$Var1[tripIDs$Freq>1]))]
# 
# tripunique <- tripunique[tripunique$TRIP_ID %in% c(as.character(tripIDs$Var1[tripIDs$Freq==1])),]
# 
# for(i in 1:length(unique(splitIDs))){
#   sub <- effort[effort$TRIP_ID == splitIDs[i],]
#   sub <- ddply(.data=sub, .(TRIP_ID, VR_NUMBER, FISHING_AREA),
#                summarize,
#                startdate = min(DATE_FISHED),
#                enddate = max(DATE_FISHED))
#   sub <- arrange(sub, enddate)
#   sub[1,]$startdate <- unique(effort$DATE_SAILED[effort$TRIP_ID == splitIDs[i]])
#   sub[2,]$enddate <- unique(effort$LANDING_DATE_TIME[effort$TRIP_ID == splitIDs[i]])
#   names(sub) <- c("TRIP_ID", "VR_NUMBER", "FISHING_AREA", "date.sailed", "date.land")
#   tripunique <- rbind(tripunique, sub)
# }
# 
# tripunique$startmonth <- month(tripunique$date.sailed)
# tripunique$endmonth <- month(tripunique$date.land)
# tripunique$ndays_1 <- ifelse((tripunique$startmonth==tripunique$endmonth)==F,
#                                   difftime((floor_date(ymd(tripunique$date.land), unit="month")-days(1)),
#                                            ymd(tripunique$date.sailed), units="day"), NA)
# tripunique$ndays_2 <- ifelse((tripunique$startmonth==tripunique$endmonth)==F, day(ymd(tripunique$date.land)), NA)
# tripunique$month <- as.numeric(ifelse((tripunique$startmonth==tripunique$endmonth)==T,
#                                            month(ymd(tripunique$date.land)),
#                                            ifelse(tripunique$ndays_1 > tripunique$ndays_2,
#                                                   month(ymd(tripunique$date.sailed)),
#                                                   month(ymd(tripunique$date.land)))))
# 
# effort <- join(effort, unique(tripunique[,c("TRIP_ID", "VR_NUMBER", "FISHING_AREA", "month")]), type="left")
# 
# effort <- subset(effort, FISHING_AREA %in% c("27A", "27B"))
# 
# effort$year <- year(effort$DATE_SAILED)
# 
# fleeteffort <- ddply(.data=effort, .(year, month),
#                      summarize,
#                      totaleffort = sum(hm))


# Step 7: Fill out discards table for new months, and compare past months to previous table values (in table below)
# Also compare to OTIS reports!
View(discards_report[discards_report$year%in% years,])

# Step 8: Fill out Standard FThours column T using:
fleeteffort_NAFO_new <- ddply(.data=new.log.dat,
                              .(year, nafo),
                              summarize,
                              totaleffort = sum(hm, na.rm=T))
fleeteffort_NAFO_old <- ddply(.data=old.log.dat,
                              .(year, nafo),
                              summarize,
                              totaleffort = sum(hm, na.rm=T))
fleeteffort_NAFO <- rbind(#fleeteffort_NAFO_old, 
  fleeteffort_NAFO_new)
dcast(fleeteffort_NAFO, year ~ nafo)
# compare this to CPUE.2018_almethod.r results!

# Step 10: Use Excel formulas to complete the table. Then go to Bycatch Report_Markdown_6.Rmd to do some final checks and get your data into nice tables.
