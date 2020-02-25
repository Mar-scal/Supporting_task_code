load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")

gba <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2019/Summer/GBa/Survey1984-2019.csv")

head(bbn)


require(plyr)
require(dplyr)
require(ggplot2)
require(sf)
require(tidyverse)
require(tidyr)
require(ROracle)
require(reshape2)
require(lubridate)



options(scipen=999)

ggplot() +
  geom_point(data=bbn[bbn$state=="live",], aes(year, com)) +
  theme_bw()


bbn_shp <- st_read("Y:/Offshore/Assessment/Data/Maps/approved/GIS_layers/offshore_survey_strata/BBn.shp")


ggplot() + 
  geom_sf(data = st_cast(bbn_shp, "MULTIPOLYGON"), aes(fill=as.factor(ID)), colour=NA, alpha=0.4) +
  geom_point(data=bbn, aes(lon, lat)) +
  facet_wrap(~year) +
  theme_void()
  

bms <- bbn %>%
  filter(state=="live") %>%
  select(ID, year, tow, cruise, bank, date, lon, lat, state, pre.bm, rec.bm, com.bm, tot.bm) %>%
  pivot_longer(cols=c(pre.bm, rec.bm, com.bm, tot.bm))

bms$name <- factor(bms$name, levels=c("pre.bm", "rec.bm", "com.bm", "tot.bm"))

ggplot() + geom_histogram(data=bms[!bms$name %in% "tot.bm",], aes(value, fill=name), 
                          bins=50, position="stack") + 
  theme_bw() +
  scale_x_log10() 

unique(survey.obj$BBn$model.dat$RS)
unique(survey.obj$BBn$model.dat$CS)
# recruit size bin is 85-95 mm

ggplot() + geom_histogram(data=bms[bms$name %in% "tot.bm",], aes(value, fill=as.factor(year)), position="stack",
                          bins=50) + 
  theme_bw() +
  scale_x_log10(breaks=c(0,0.1, 1,2,5, 10, 20,50, 75, 100))

ggplot() + geom_histogram(data=bms[bms$name %in% "com.bm",], aes(value, fill=as.factor(year)), position="stack",
                          bins=50) + 
  theme_bw() +
  scale_x_log10(breaks=c(0,0.1, 1,2,5, 10, 20,50, 75, 100))



ggplot() + geom_histogram(data=bms[bms$name %in% "tot.bm",], aes(value), 
                          bins=50) + 
  theme_bw() 
  





basketdat <- NULL

for(i in 1:length(unique(gba[gba$year>1999,]$cruise))){
  
  cruise <- as.character(unique(gba[gba$year>1999,]$cruise)[i])
  print(cruise)
  ### read in OSSURVEYS, OSTOWS and OSHFREQ_SAMPLES
  chan <-dbConnect(dbDriver("Oracle"),username="scaloff", password="fgb256k","ptran")
  
  #####################################################################################################################
  # Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
  # Key is to import those tables and send it out of this file looking identical!  
  ######################################################################################################################
  db <- "SCALOFF" ### CHANGE HUMF TO SCALOFF!!!
  #message("reminder that this is pulling data from HUMF views, not production SCALOFF")
  
  #qu.strata <- "select * from SCALOFF.OSSTRATA"
  # DK Oct 29, 2015, don't need tow data either, we don't ever use it.... 
  qu.surveys <- paste0("select * from ", db, ".OSSURVEYS")
  qu.surveys<- dbGetQuery(chan, qu.surveys)
  
  survey_seq <- paste(as.character(unique(qu.surveys[qu.surveys$CRUISE == cruise,]$SURVEY_SEQ)), sep="' '", collapse=", ")
  qu.tows <- paste0("select * from ", db, ".OSTOWS WHERE SURVEY_SEQ in (", survey_seq, ")")
  qu.tows<- dbGetQuery(chan, qu.tows)
  
  tow_seq <- paste(as.character(unique(qu.tows$TOW_SEQ)), sep="' '", collapse=", ")
  qu.hfreq <- paste0("select * from ", db, ".OSHFREQSAMPLES WHERE TOW_SEQ in (", tow_seq, ")")
  qu.hfreq<- dbGetQuery(chan, qu.hfreq)
  
  hfreq_seq <- paste(as.character(unique(qu.hfreq$HFREQ_SAMPLE_SEQ)), sep="' '", collapse=", ")
  qu.heightfreq <- paste0("select * from ", db, ".OSHEIGHTFREQ WHERE HFREQ_SAMPLE_SEQ in (", hfreq_seq, ")")
  qu.heightfreq<- dbGetQuery(chan, qu.heightfreq)
  dbDisconnect(chan)
  
  surv_tows <- join(qu.tows, qu.surveys, type="left", by="SURVEY_SEQ")
  # surv_tows <- rbind(data.frame(surv_tows, LIVECODE="L"), data.frame(surv_tows, LIVECODE="D"))
  surv_tows_samp <- join(surv_tows, qu.hfreq, type="left", by="TOW_SEQ")
  surv_tows_samp_hf <- join(surv_tows_samp, qu.heightfreq, type="left", by="HFREQ_SAMPLE_SEQ")
  
  basketdat <- rbind(basketdat, surv_tows_samp_hf)
  
}

basketdat <- basketdat[,which(!names(basketdat) %in% "COMMENTS")]

basketdat <- basketdat[basketdat$MGT_AREA_CD == "GBa",]

basketdat <- select(basketdat, -TOW_SEQ, -SURVEY_SEQ, -BOTTOM_TYPE_ID, -BOTTOM_TEMP, -HFREQ_SAMPLE_SEQ, -HEIGHT_FREQ_SEQ, -BIN_ID, -NUMBER_IN_BIN)

basketdat <- unique(basketdat)

basketdat <- basketdat %>%
  filter(NUM_OF_CONTAINERS <99) %>%
  filter(!is.na(LIVECODE)) %>%
  filter(LIVECODE == "L") %>%
  filter(CONTAINER_TYPE_ID == 1)


basketdat$YEAR <- year(basketdat$START_DATE)

ggplot() + geom_point(data=basketdat[basketdat$YEAR > 2011,], aes(YEAR, NUM_OF_CONTAINERS, colour=as.factor(CONTAINER_TYPE_ID))) + facet_wrap(~LIVECODE)

ggplot() + geom_histogram(data=basketdat[basketdat$YEAR > 2011,], aes(NUM_OF_CONTAINERS), bins=40) + 
  theme_bw()+
  scale_x_continuous(breaks=seq(0,50,5))

