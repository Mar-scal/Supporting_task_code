# for DR2024_09

cruise <- "LE20"
yr <- 2024
require(ROracle) || stop("Package plyr cannot be found")
require(plyr) || stop("Package plyr cannot be found")
require(reshape2) || stop("Package reshape2 cannot be found")
require(tidyr) || stop("Package reshape2 cannot be found")
require(dplyr) || stop("Package reshape2 cannot be found")
if(is.null(cruise)) stop("Please specify cruise in order to run industry report; yr is also required.")
if(is.null(yr)) stop("Please specify yr in order to run industry report; cruise is also required.")

db.con <- "ptran"
un="scaloff"
pw=""

### read in OSSURVEYS, OSTOWS and OSHFREQ_SAMPLES
chan <-dbConnect(dbDriver("Oracle"), username=un, password=pw,db.con)

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

survey_seq <- paste(as.character(unique(qu.surveys[qu.surveys$CRUISE==cruise,]$SURVEY_SEQ)), sep="' '", collapse=", ")
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

# surv_tows_samp_hf$indreport_bin[surv_tows_samp_hf$BIN_ID <70] <- "0-70"
# surv_tows_samp_hf$indreport_bin[surv_tows_samp_hf$BIN_ID >65 & surv_tows_samp_hf$BIN_ID <100] <- "70-100"
# surv_tows_samp_hf$indreport_bin[surv_tows_samp_hf$BIN_ID >95] <- "100+"

# surv_tows_samp_hf$prorated_number <- surv_tows_samp_hf$NUMBER_IN_BIN / (surv_tows_samp_hf$SAMPLED/surv_tows_samp_hf$TOTAL)

# surv_tows_samp_hf$prorated_number[is.na(surv_tows_samp_hf$prorated_number)] <- 0

surv_tows_samp_hf <- dplyr::select(surv_tows_samp_hf, SURVEY_NAME, MGT_AREA_CD, TOW_NO, TOW_TYPE_ID, 
                            START_LAT, START_LON, END_LAT, END_LON, 
                            DEPTH_F, SPECIES_ID, LIVECODE, BIN_ID, NUMBER_IN_BIN)

surv_tows_samp_hf <- surv_tows_samp_hf %>%
  group_by(SURVEY_NAME, MGT_AREA_CD, TOW_NO, TOW_TYPE_ID, 
           START_LAT, START_LON, END_LAT, END_LON, 
           DEPTH_F, SPECIES_ID, LIVECODE, BIN_ID) %>%
  dplyr::summarize(NUMBER_IN_BIN = sum(NUMBER_IN_BIN))

# surv_tows_samp_hf <- surv_tows_samp_hf[!is.na(surv_tows_samp_hf$LIVECODE),]

surv_tows_samp_hf <- tidyr::pivot_wider(surv_tows_samp_hf, names_from = "LIVECODE", values_from = "NUMBER_IN_BIN")

surv_tows_samp_hf <- dplyr::arrange(surv_tows_samp_hf, SURVEY_NAME, TOW_NO, BIN_ID)

names(surv_tows_samp_hf)[names(surv_tows_samp_hf)== "L"] <- "LIVE_RAW"

names(surv_tows_samp_hf)[names(surv_tows_samp_hf)== "D"] <- "DEAD_RAW"

# adding 0s # may not be necessary...
tows <- unique(dplyr::select(surv_tows_samp_hf, SURVEY_NAME, MGT_AREA_CD, TOW_NO, TOW_TYPE_ID,
                             START_LAT, START_LON, END_LAT, END_LON,
                             DEPTH_F, SPECIES_ID))
bins <- expand.grid(BIN_ID = seq(0,195,5))
expanded <- NULL
for(i in 1:nrow(tows)){
  df <- data.frame(tows[i,],
             bins)
  expanded <- rbind(expanded, df)
}

surv_tows_samp_hf <- dplyr::left_join(expanded, surv_tows_samp_hf)

surv_tows_samp_hf$LIVE_RAW[is.na(surv_tows_samp_hf$LIVE_RAW)] <- 0
surv_tows_samp_hf$DEAD_RAW[is.na(surv_tows_samp_hf$DEAD_RAW)] <- 0

surv_tows_samp_hf <- dplyr::select(surv_tows_samp_hf, SURVEY_NAME, MGT_AREA_CD, TOW_NO, TOW_TYPE_ID, 
                                   START_LAT, START_LON, END_LAT, END_LON, 
                                   DEPTH_F, SPECIES_ID, BIN_ID, LIVE_RAW, DEAD_RAW)

surv_tows_samp_hf <- dplyr::arrange(surv_tows_samp_hf, SURVEY_NAME, TOW_NO, BIN_ID)

# And make the CSV... 
write.csv(surv_tows_samp_hf, "Y:/Offshore/Data requests/2024/DR2024_13_SPANS/DR2024_13_rev.csv",row.names=F)
