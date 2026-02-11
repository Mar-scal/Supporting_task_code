# ancient PS data
require(dplyr)
require(tidyr)
years <- list.files("Y:/Offshore/Assessment/Data/Archive/PortSampling/")
years <- 1981:2005
paths <- NULL
for(i in years){
  files <- list.files(paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i))
  if(length(files)==1) dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files)
  if(length(files)>1){
    if(any(grepl(pattern="reg.", x=files, fixed=T))) {
      dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files[grep(pattern="reg.", x=files, fixed=T)])
    }
    if(!any(grepl(pattern="reg.", x=files, fixed=T)) & any(grepl(pattern="all.", x=files, fixed = T))) {
      dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files[grep(pattern="all.", fixed=T, x=files)])
    }
    if(any(grepl(pattern="FT", x=dat))) {
      dat <- dat[-grep(pattern="FT", x=dat)]
    }
    if(any(grepl(pattern="WF", x=dat))) {
      dat <- dat[-grep(pattern="WF", x=dat)]
    }
  }
  print(i)
  print(dat)
  paths <- c(paths, dat)
}

ps <- NULL
for(i in 1:length(paths)){
  ps[[i]] <- read.csv(paths[i], header=F, sep="\t")
  if(ncol(ps[[i]])==1) ps[[i]] <- read.csv(paths[i], header=F, sep=" ")
}
save(ps, file="C:/Users/keyserf/Documents/temp_data/old_ps.RData")
load("C:/Users/keyserf/Documents/temp_data/old_ps.RData")
#fails: old.ps <- do.call("rbind",ps)
for(i in 1:length(ps)){
  ps[[i]][is.na(ps[[i]])] <- 0
  nas <- colSums(ps[[i]])
  ps[[i]] <- ps[[i]][which(nas>0)]
  names(ps[[i]])[1:5] <- c("date","boat","port","unknown", "id")
  ps[[i]] <- ps[[i]] %>%
    pivot_longer(
      cols = starts_with("V"),
      names_to = "scalnum",
      names_prefix = "V",
      values_to = "wmw",
      values_drop_na = TRUE
    ) %>%
    group_by(date, boat, port, unknown, id) %>%
    mutate(scalnum = seq(n())) %>%
    ungroup()
  ps[[i]] <- ps[[i]][ps[[i]]$wmw>0,]
  # print(i)
  ps[[i]]$date2 <- dmy(ps[[i]]$date)
  #print(any(is.na(ps[[i]]$date2)))
  if(all(is.na(ps[[i]]$date2)) | all(year(ps[[i]]$date2) < 1981)) ps[[i]]$date2 <- ymd(ps[[i]]$date)
  # ps[[i]][is.na(ps[[i]]$date2),]
  #print(any(is.na(ps[[i]]$date2)))
}

ps <- do.call("rbind", ps)

unique(ps[is.na(ps$date2),]$date)

# records on June 36 1989 and April 31 2002 need to be fixed
ps[(which(ps$date == 360689)[1]-5):(which(ps$date == 360689)[1]),]
table(ps[ps$date==260689,]$id)
hist(ps[ps$date==260689,]$id)
ps[ps$date==260689 & ps$id==111,]$wmw %in% ps[ps$date==360689 & ps$id==111,]$wmw
ps[ps$date==360689 & ps$id==111,]$wmw %in% ps[ps$date==260689 & ps$id==111,]$wmw
ps[ps$date==360689,]$date <- 260689
ps[ps$date==260689,]$date2 <- dmy(ps[ps$date==260689,]$date)
# changed June 36 1989 to June 26 1989

ps[(which(ps$date == 20020431)[1]-5):(max(which(ps$date == 20020431))),]
tail(ps[(which(ps$date == 20020431)[1]-5):(max(which(ps$date == 20020431)))+1,])
table(ps[ps$date==20020431,]$id)
table(ps[ps$date==20020420,]$id)
table(ps[ps$date==20020430,]$id)
hist(ps[ps$date==20020420,]$id)
ps[ps$date==20020420 & ps$id==111,]$wmw %in% ps[ps$date==20020431 & ps$id==111,]$wmw
ps[ps$date==20020431 & ps$id==111,]$wmw %in% ps[ps$date==20020420 & ps$id==111,]$wmw
ps[ps$date==20020431,]$date <- 20020420
ps[ps$date==20020420,]$date2 <- ymd(ps[ps$date==20020420,]$date)
# changed Apr 31 2022 to Apr 20 2002

unique(ps$boat)
unique(ps$port)
unique(ps$id)

unique(year(ps[ps$id<100,]$date2))

ps$fished <- ps$date2
ps$date2 <- NA

unique(year(ps$fished))
ps[year(ps$fished)==420,]

head(ps)
old_ps <- ps
write.csv(old_ps, file = "C:/Users/keyserf/Documents/temp_data/Old_PS_data_clean_1981-2005.csv")

oldps <- read.csv("C:/Users/keyserf/Documents/temp_data/Old_PS_data_clean_1981-2005.csv")

# Frist grab the directory to work from
direct <- "Y:/Offshore/Assessment/"
#direct <- "C:/Users/keyserf/Documents/temp_data/"
direct_fns <- "C:/Users/keyserf/Documents/Github/"
# Grab the years of port sampling data.  Note that Ginette says the Port Sampling data goes all the way back to 1996
# I haven't a clue where that data is, but would certainly be interesting to compare the last 20 years of data!!
years <- 1981:2005
options(stringsAsFactors = F) # Don't make anything a Factor as it screws up random crap.


# We will need our fishery log function to get the log data....
source(paste(direct_fns,"Assessment_fns/Fishery/logs_and_fishery_data.r",sep="")) #logs_and_fish is function call
# And we need the Offshore Scallop fleet name file so we can link the fishery data to these names
off.fleet <- read.csv("C:/Users/keyserf/Documents/temp_data/Data/Offshore_fleet.csv")
#logs_and_fish(loc = "offshore", year = years, get.local = T, get.marfis = F, direct=direct)
load("C:/Users/keyserf/Documents/temp_data/oldlogdat_1981-2024.RData")
if(exists("old.log.dat") & exists("new.log.dat")) fish.dat<-merge(new.log.dat,old.log.dat,all=T)
if(exists("old.log.dat") & !exists("new.log.dat")) {
  fish.dat<-old.log.dat
  fish.dat$vrnum <- NA
  fish.dat$fished <- NA
}
if(!exists("old.log.dat") & exists("new.log.dat")) {
  fish.dat<-new.log.dat
  fish.dat$vesid <- NA
}
fish.dat$ID<-1:nrow(fish.dat)
################  Section 1, processing the port sampling meat weight information  --  Section 1 ############################################################
################  Section 1, processing the port sampling meat weight information  --  Section 1 ############################################################
# Here we pull in all the port sampling data (right now 2006-2017) and tidy it up for later analysis, once happy with the results
# you can skip this Section
require(tidyverse)
oldps <- select(oldps, -X)
# Get ready for the loop...
index <- 0
dat <- NULL
split_days <- NULL
temp<-NULL
# Run this for all years we have data, this takes about 10 minutes...
for(i in 1:length(years))
{
  print(years[i])
  trips <- oldps[year(oldps$fished)==years[i],]
  trips$vesid <- trips$boat
  trips2 <- unique(select(trips, fished, vesid, port))
  trips2 <- arrange(trips2, vesid, port, fished)
  trips2$trip.id <- NA
  for(j in 1:nrow(trips2)){
    if(j==1) trips2$trip.id[j] <- 1
    if(j>1){
      if(all(trips2[j,c("fished", "vesid", "port")] == trips2[(j-1),c("fished", "vesid", "port")])){
        trips2$trip.id[j] <- trips2$trip.id[j-1]
      }
      if(all(trips2[j,c("fished", "vesid", "port")] == trips2[(j-1),c("fished", "vesid", "port")])==F){
        if((ymd(trips2[j,]$fished) - ymd(trips2[j-1,]$fished) == days(1)) & (trips2[j,]$vesid==trips2[j-1,]$vesid) & (trips2[j,]$port==trips2[j-1,]$port)) trips2$trip.id[j] <- (trips2$trip.id[j-1])
        if(!(ymd(trips2[j,]$fished) - ymd(trips2[j-1,]$fished) == days(1)) | (!trips2[j,]$vesid==trips2[j-1,]$vesid) | (!trips2[j,]$port==trips2[j-1,]$port)) trips2$trip.id[j] <- (trips2$trip.id[j-1] + 1)
      }
    } 
  }
  
  # need trip identifier because we don't have individual files
  trips2 <- trips2 %>%
    group_by(vesid) %>%
    mutate(trip.id = paste0(vesid, ".", seq(n())))
  trips <- left_join(trips, trips2)
  
  num.trips <- nrow(trips2)
  
  for(j in 1:num.trips)
  {
    print(index)
    index <- index + 1
    #if(index==204) browser()
    #This will pull the data from the Port Sampling file.
    dat[[index]] <- trips[trips$trip.id==trips2$trip.id[j],]
    
    # And now reorder the data by ID so the samples all stay together and the 0's come at the end so we can chuck those..
    dat[[index]] <- dat[[index]] %>% arrange(desc(wmw)) # First order the values from biggest to smallest
    dat[[index]] <- dat[[index]] %>% arrange(id) # Now order the ID's from smallest to largest
    # dat[[index]]$flag_datefished <- "N"
    
    # Now we can quickly add a sample ID to these in case we want it later...
    samp.ids <- unique(dat[[index]]$id)
    n.samp.ids <- length(samp.ids) # The first 1-2 digits of the ID is the day of the trip (i.e. 1 = first day, 2 = second day)
    # The last 2 digits decribe the location of the sample in the tub (i.e. middle/top/bottom/front/back etc.)
    for(r in 1:n.samp.ids)
    {
      dat[[index]]$sample_id[dat[[index]]$id == samp.ids[r]] <- 1:nrow(dat[[index]][dat[[index]]$id == samp.ids[r],])
    } # end for(r in 1:n.samp.ids)
    # And now we can chuck all the 0's
    dat[[index]] <- dat[[index]][which(dat[[index]]$wmw > 0),]
    temp[[index]] <- dat[[index]]
    
    #check dates
    # can't do much about these... we don't have original files! Some of the dates are date sampled, while others are date fished. Too bad. 
    # if(!years[i] == unique(year(ymd(dat[[index]]$fished)))) {
    #   message(paste0("corrected year fished in ", files[j], " based on folder year"))
    #   dat[[index]]$fished <- paste0(years[i], str_sub(dat[[index]]$fished, 5,8))
    #   #dat[[index]]$date <- paste0(years[i], str_sub(dat[[index]]$date, 5,8))
    # }
    # something might be fishy with date fished vs date sampled. They are months apart.
    # if(any(!unique(month(ymd(dat[[index]]$fished))) %in% (unique(month(ymd(dat[[index]]$date)))-1):(unique(month(ymd(dat[[index]]$date)))+1))) {
    #   message(paste0("In ", files[j], ", month fished is ", unique(month(ymd(dat[[index]]$fished))), " but PS date is ", unique(ymd(dat[[index]]$date)), ". You better check this manually."))
    #   dat[[index]]$flag_datefished <- "Y"
    # }
    
    # if(any(!year(ymd(dat[[index]]$date)) == year(ymd(dat[[index]]$fished)))){
    #   if(index==731) dat[[index]]$fished <- gsub(x=as.character(ymd(dat[[index]]$fished) + years(2)), "-", "")
    # }
    #browser()
    # compare to logs to assign bank
    # Get the vessel of interest
    vessel <- na.omit(off.fleet[off.fleet$Pre_2008_ID == unique(dat[[index]]$boat), c("Pre_2008_ID","VMS_old_ID","ID")])
    # Get all the fishery data for that vessel, checking all types of identifiers
    ves.fish.dat <- fish.dat[(fish.dat$vesid %in% unlist(vessel) | fish.dat$ves %in% unlist(vessel) | fish.dat$vrnum %in% unlist(vessel)),]
    # Get the fishery data for that vessel on the appropriate dates (check the day prior as well, since most of the old dates are land dates!)
    ves.fish.dat <- ves.fish.dat[ves.fish.dat$date %in% c(unique(ymd(dat[[index]]$fished)-days(1)),unique(ymd(dat[[index]]$fished))),]
    bank <- unique(na.omit(ves.fish.dat$bank))
    # land <- unique(na.omit(ves.fish.dat$date.land))
    # if(length(land)==0){
    #   land <- ves.fish.dat %>% dplyr::group_by(trip.id) %>%
    #     dplyr::summarize(land=max(date, na.rm=T))
    #   land <- land$land
    # }
    trip.id <- unique(na.omit(ves.fish.dat$trip.id))
    if(length(bank)==0) {
      #browser()
      dat[[index]]$bank <- NA
      dat[[index]]$trip.id <- NA
    }
    if(length(bank)==1) {
      dat[[index]]$bank <- bank
      if(length(trip.id)==1) dat[[index]]$trip.id <- trip.id
      if(length(trip.id)>1) {
        dat[[index]]$trip.id <- max(trip.id)
        message("there are two trip.ids in fish.dat for this PS trip, but they're on the same bank, so I'm just using the higher one")
      }
    }
    if(length(bank)>1) {
      # generally split trips, join specific days, and drop transition day if it gets double counted (e.g. index 2185 20140821 PRES)
      ves.fish.dat <- unique(ves.fish.dat[, c("date", "bank", "trip.id")])
      ves.fish.dat$fished <- as.character(gsub(x=ves.fish.dat$date, "-", ""))
      dat[[index]]$fished <- as.character(dat[[index]]$fished)
      test <- dplyr::left_join(dat[[index]], dplyr::select(ves.fish.dat, bank, fished, trip.id))
      if(dim(test)[1] == dim(dat[[index]])[1]) dat[[index]] <- test
      if(!dim(test)[1] == dim(dat[[index]])[1]) {
        message("dropping split day")
        test2 <- test %>% dplyr::group_by(fished) %>%
          dplyr::summarize(banks = length(unique(bank)))
        split_days[[index]] <- unique(dplyr::select(test[test$fished %in% test2$fished[test2$banks>1],], -bank))
        split_days[[index]]$banks <- paste(bank, collapse=" ")
        split_days[[index]]$file <- files[j]
        test <- test[test$fished %in% test2$fished[test2$banks<2],]
        dat[[index]] <- test
      }
    }
    
    if(!dim(dat[[index]])[1] == dim(temp[[index]])[1]) message(paste0("File ", files[j], " - difference of ", abs(nrow(temp[[index]])-nrow(dat[[index]])), " records"))
    if(!dim(dat[[index]][complete.cases(dat[[index]]),])[1] == dim(temp[[index]][complete.cases(temp[[index]]),])[1]) message(paste0("File ", files[j], " - difference of ", abs(nrow(temp[[index]][complete.cases(temp[[index]]),])-nrow(dat[[index]][complete.cases(dat[[index]]),])), " records"))
    # dat[[index]]$file <- files[j]
    
    #check for duplicates
    if(any(duplicated(dat))){
      message(paste0("dropping duplicated file ", files[j]))
      dat <- dat[!which(duplicated(dat))]
    }
    
  } # end for(j in 1:num.files)
  
} # end the for(i in 1:length(years))


length(dat)
dim(trips)
length(split_days)

old.port.dat <- do.call("rbind",dat)
old_split_days <- do.call("rbind",split_days)

dim(old.port.dat)
table(old.port.dat$bank)
old.port.dat$year <- year(old.port.dat$fished)
sum(table(old.port.dat[is.na(old.port.dat$bank),]$trip.id))

old.port.dat[is.na(old.port.dat$trip.id),][1,]
oldps[oldps$fished=="1981-06-17" & oldps$boat==3530,]
old.port.dat[old.port.dat$fished=="1981-06-17" & old.port.dat$boat==3530,]$trip.id

dim(oldps)
dim(old.port.dat)

write.csv(old.port.dat, "C:/Users/keyserf/Documents/temp_data/Old_PS_data_clean_1981-2005_bank.csv")
