# To get discards estimates for any species

# Go through this script line by line.

require(lubridate)
require(plyr)
require(dplyr)
require(reshape2)

# Set year, other report-specific things, and get observer trip metadata
direct <- "Y:/Bycatch/"
direct_fns <- "C:/Users/keyserf/Documents/Github/"
direct_off_fns <- "C:/Users/keyserf/Documents/Github/"
direct_off <- "Y:/Offshore/Assessment/"
un.ID <- "ENTER UN HERE"
pwd.ID <- "ENTER PW HERE"
years <- 2022:2024
banks <- c("GBa", "GBb")
fleet <- "offshore"
species <- "WINTER FLOUNDER" ### MUST MATCH ISDB COMMON NAME EXACTLY
##### examples:
# "AMERICAN PLAICE"   "COD(ATLANTIC)"   "WINTER FLOUNDER", "WITCH FLOUNDER", "SUMMER FLOUNDER", "YELLOWTAIL FLOUNDER", 
# "FOURSPOT FLOUNDER", "FLOUNDER UNIDENTIFIED", "GULF STREAM FLOUNDER"
# "LITTLE SKATE", "THORNY SKATE", "SMOOTH SKATE", "WINTER SKATE", 
# "BARNDOOR SKATE", "WHITE SKATE", "ROUND SKATE", "JENSEN'S SKATE", 
# "SPINYTAIL SKATE", "SKATES (NS)", "SKATE UNID. EGGS", "SKATE/SHARK EGG PURSE - LIVE"
# "HADDOCK"

## We use the sql pulled data to make sure that we captured every possible trip (UPDATE THE DATE!)
source(paste0(direct_fns, "Bycatch_fns/obs_metadata.R"))
obs_metadata(direct=paste0(direct, "data"), direct_fns = paste0(direct_fns, "Bycatch_fns/"), direct_off=direct_off, direct_off_fns = direct_off_fns, sqlexport = NULL, pullfromISDB=TRUE, writecsv=TRUE, 
             polys="offshore", un=un.ID, pw=pwd.ID, db.con="ptran", checkspatial=TRUE, year=years,
             package="ROracle") 

# We'll use this df to help match old vessel IDs to new vnums
vessel <- read.csv(paste0(direct_off,"Data/Offshore_Fleet.csv"))

# read in discards data and run checks using detailed getdiscards output and gethooks output
source(paste0(direct_fns, "Bycatch_fns/getdiscards.r"))

### reading the discards data from ISDB
discards <- NULL
#years <- c(year-1, year)
for (i in 1:length(years)) {
  discards_y <- getdiscards_long_detail(year=years[i], un = un.ID, pw = pwd.ID, db.con = "ptran", direct.off = direct_off, package="ROracle") 
  discards <- rbind(discards, discards_y)
}  

tail(discards, 20)

### reading the hooks data from ISDB
hooks <- NULL
for (i in 1:length(years)) {
  hooks_y <- gethooks_long_detail(year=years[i], un = un.ID, pw = pwd.ID, db.con = "ptran", direct.off = direct_off, package="ROracle") 
  hooks <- rbind(hooks, hooks_y)
}

tail(hooks, 20)

# have to remove tows with NA hooks hauled
remove <- unique(hooks[is.na(hooks$TOTAL_HOOKS), c("TRIP", "SET_NO")])
remove$remove <- T
discards <- left_join(discards, remove)
discards <- discards[is.na(discards$remove),]
hooks <- hooks[!is.na(hooks$TOTAL_HOOKS),]
dim(hooks)
# correct source issues for J22-0030 and J24-0054
discards$SOURCE[discards$TRIP=="J22-0030" & discards$SET_NO %in% c(29,33)] <- 0
hooks$SOURCE[hooks$TRIP=="J22-0030" & hooks$SET_NO %in% c(29,33)] <- 0
discards[discards$TRIP=="J23-0027" & discards$SET_NO %in% c(30,31,34),]$SOURCE <- 0
hooks[hooks$TRIP=="J23-0027" & hooks$SET_NO %in% c(30,31,34),]$SOURCE <- 0
discards$SOURCE[discards$TRIP=="J24-0054" & discards$SET_NO %in% c(110,111)] <- 0
hooks$SOURCE[hooks$TRIP=="J24-0054" & hooks$SET_NO %in% c(110,111)] <- 0

# do the manual edits from bycatchreport_7.R as needed
# discards that are not on A, but the whole trip was supposed to be on A 
# separate analysis determined that the following sets are not on A (I think the lat/longs are just off by a degree, but we'll just drop them)
discards <- discards %>%
  filter(!(TRIP=="J23-0158" & 
             SET_NO %in% c(1,2,3,9,10,11,12,15,16,17,27,28,29,30)))

hooks <- hooks %>%
  filter(!(TRIP=="J23-0158" & 
             SET_NO %in% c(1,2,3,9,10,11,12,15,16,17,27,28,29,30)))

# discards that are not on A even though fishing was supposedly on A
discards <- discards %>%
  filter(!(TRIP=="J24-0007" & 
             SET_NO %in% c(1,2,3,37)))

hooks <- hooks %>%
  filter(!(TRIP=="J24-0007" & 
             SET_NO %in% c(1,2,3,37)))

# observed sets marked as unobserved, manually fix:
#discards[discards$TRIP=="J24-0007"& discards$SET_NO==50,]$SOURCE <- 0
hooks[hooks$TRIP=="J24-0007"& hooks$SET_NO==50,]$SOURCE <- 0
hooks[hooks$TRIP=="J24-0132" & hooks$SET_NO %in% 97:98,]$SOURCE <- 0

###  this is the function we need to do the checks and easy fixes
source(paste0(direct_fns, "Bycatch_fns/checkdiscards.r"))
checkdiscards(data=discards, hooksdata = hooks, 
              getdiscards_type = "getdiscards_long_detail", gethooks_type = "gethooks_long_detail", species=species,
              fix=FALSE)
# To fix them automatically set fix to TRUE: 
fixed <- checkdiscards(data=discards, hooksdata = hooks, 
                       getdiscards_type = "getdiscards_long_detail", gethooks_type = "gethooks_long_detail", species=species,
                       fix=TRUE, output="same")
table(fixed$hooksdata_fixed$PNTCD_ID)
hooks <- fixed$hooksdata_fixed[fixed$hooksdata_fixed$PNTCD_ID==2,]
discards <- dplyr::select(fixed$data_fixed, -COMAREA_ID)
# take a look... nbd. fixed$hooksdata_fixed_trips

# remove trips that are not by offshore vessels
#inshoretrips <- hooks[!(hooks$VRN %in% vessel$ID) & !(hooks$VRN %in% vessel$VMS_old_ID) & !(hooks$VRN %in% vessel$Pre_2008_ID), c("TRIP", "COMAREA_ID")]
offshoretrips <- hooks[(hooks$VRN %in% vessel$ID) | (hooks$VRN %in% vessel$VMS_old_ID) | (hooks$VRN %in% vessel$Pre_2008_ID), c("TRIP", "COMAREA_ID")]

if(fleet=="offshore"){ 
  discards <- discards[discards$TRIP %in% offshoretrips$TRIP,]
  hooks <- hooks[hooks$TRIP %in% offshoretrips$TRIP,]
  }
# if(fleet=="inshore"){ 
#   discards <- discards[discards$TRIP %in% inshoretrips$TRIP,]
#   hooks <- hooks[hooks$TRIP %in% inshoretrips$TRIP,]
#   }


# join the spatially checked obs_metadata stuff to assign bank to comarea_ids
set_coord <- read.csv(paste0(direct, "data/Observed scallop trip metadata_tidysets_2025-07-22.csv"))
set_coord<- set_coord[!is.na(set_coord$SET_NO),]
set_coord$LANDING_DATE <- ymd(set_coord$LANDING_DATE)

discards$tripset <- as.character(paste0(discards$TRIP, ".", discards$SET_NO))
set_coord$tripset <- as.character(paste0(set_coord$TRIP, ".", set_coord$SET_NO))

discards_sets <- join(dplyr::select(set_coord[year(ymd(set_coord$LANDING_DATE)) %in% years,], -X), discards, by="tripset", type="full")

# get discards by area by trip
discards_area_trip_common <- arrange(ddply(.data=discards_sets[discards_sets$SOURCE==0,], .(TRIP, area, COMMON),
                                     summarize,
                                     totaldiscards = sum(`SUM(EST_DISCARD_WT)`, na.rm=T)), TRIP)

# look at area and remove those not on GB
discards_area_trip_common <- discards_area_trip_common[!discards_area_trip_common$TRIP == "J23-0149",]
#discards_area_trip_common <- discards_area_trip_common[!is.na(discards_area_trip_common$area),]

discards_area_trip_common[discards_area_trip_common$COMMON=="WINTER FLOUNDER",] %>%
  left_join(unique(alltrips[, c("TRIP", "CFV", "BOARD_DATE", "LANDING_DATE")])) %>%
  arrange(LANDING_DATE) %>%
  select(CFV, TRIP, BOARD_DATE, LANDING_DATE, area, COMMON, totaldiscards)

arrange(discards_area_trip_common[discards_area_trip_common$COMMON=="WINTER FLOUNDER",], TRIP)
# copy and paste into the spreadsheet
# sum up for each trip if on GB

out <- discards_area_trip_common[discards_area_trip_common$COMMON=="WINTER FLOUNDER",] %>%
  left_join(unique(alltrips[, c("TRIP", "CFV", "BOARD_DATE", "LANDING_DATE")])) %>%
  arrange(LANDING_DATE) %>%
  select(CFV, TRIP, BOARD_DATE, LANDING_DATE, area, COMMON, totaldiscards)

ggplot() + geom_point(data=out, aes(LANDING_DATE, totaldiscards))
  

# from spreadsheet
WF <- read.csv("C:/Users/keyserf/Documents/temp_data/WFl_2025_month.csv")

ggplot() + geom_line(data=WF[WF$monthnum==12,], aes(year, as.numeric(Cum.Annual.Discard..mt.)))+
  geom_point(data=WF[WF$monthnum==12,], aes(year, as.numeric(Cum.Annual.Discard..mt.))) +
  ylab("Annual total discards (mt)")

WF$date <- dmy(paste0("01-", WF$monthnum, "-", WF$year))
ggplot() + geom_line(data=WF, aes(date, as.numeric(Cum.Annual.Discard..mt.)))

ggplot() + geom_line(data=WF, aes(date, as.numeric(ma.Discard..mt.))) +
  ylab("Monthly total discards (mt)")

ggplot() + geom_line(data=WF, aes(date, as.numeric(ma.Discard.Rate..kg.hm.))) +
  ylab("Monthly discard rate")

ggplot() + geom_line(data=WF, aes(date, as.numeric(Fleet.Effort..hm.)))

WF2 <- read.csv("C:/Users/keyserf/Documents/temp_data/WFl_2025_trip.csv")
WF2$Date <- ymd(WF2$Land.Date)

ggplot() + geom_point(data=WF2, aes(Date, Effort..hm.))
ggplot() + geom_point(data=WF2, aes(Date, as.numeric(Discard.Rate..kg.hm.))) + ylab("Trip discard rates (kg/hm)")

# take the rest from the discards spreadsheet but do some checks...
hooks %>% group_by(TRIP, COMAREA_ID, SOURCE) %>%
  dplyr::summarize(total_hooks=sum(TOTAL_HOOKS)) %>%
  tidyr::pivot_wider(id_cols=c(TRIP, COMAREA_ID),names_from = SOURCE, values_from = total_hooks) %>% #View() %>%
  dplyr::mutate(total=sum(`0`, `1`)) %>%
  View()

