## DR2022_09 Hydration data for Sian Bryson @ Dal
# date
# location
# tow info
# sh
# mw
# GB only

direct <- "Y:/Offshore/Assessment/"

funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/github_spatial_import.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/shwt.lme.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} # end for(un in funs)

offshore <- github_spatial_import(subfolder = "offshore", zipname = "offshore.zip", quiet=T)


require(ggplot2)
require(lubridate)
require(tidyverse)
require(sf)



hyd <- read.csv(paste0(direct, "Data/Hydration/ElizWork/Hydration_82-10_FINAL.csv"))

head(hyd)
unique(hyd$TOW_DATE)

hyd$date <- dmy(hyd$TOW_DATE)

hyd[is.na(hyd$date),]

hyd <- hyd[!is.na(hyd$DDSlat),]
hyd <- hyd[!is.na(hyd$DDSlon),]
hyd <- hyd[!is.na(hyd$date),]
hyd$year <- year(hyd$date)
hyd$month <- month(hyd$date)

head(hyd)

hyd_sf <- st_as_sf(hyd, coords=c(X="DDSlon", Y="DDSlat"), crs=4326)

hyd_sf_gb <- st_intersection(hyd_sf, offshore[offshore$ID %in% c("GBa", "GBb"),])

dim(hyd_sf_gb)

unique(hyd_sf$MGT_AREA_CD)

dim(hyd_sf[hyd_sf$MGT_AREA_CD %in% c("GB", "GBa", "GBb", "Georges", "Georges A", "Georges B"),])

hyd_sf_gb_tow <- unique(select(hyd_sf_gb, TOW_NUM, date))
hyd_sf_gb_tow_2 <- unique(select(hyd_sf[hyd_sf$MGT_AREA_CD %in% c("GB", "GBa", "GBb", "Georges", "Georges A", "Georges B"),], TOW_NUM, date))

ggplot() + geom_sf(data=hyd_sf_gb_tow) +
  geom_sf(data=hyd_sf_gb_tow_2, colour="red", alpha=0.5)

# ok so some samples from US GB were removed. That's a-ok.

hyd <- select(hyd_sf_gb, year, month, date, TOW_NUM, Depth, SCALLOP_NUM, WET_MEAT_WGT, SHELL_HEIGHT, Source)
coords <- st_coordinates(hyd)
st_geometry(hyd) <- NULL

hyd <- cbind(hyd, coords)

hyd <- select(hyd, year, month, date, TOW_NUM, X, Y, Depth, SCALLOP_NUM, WET_MEAT_WGT, SHELL_HEIGHT, Source)

hyd$file <- "eliz"
# 
# write.csv(hyd, "Y:/Offshore/Assessment/2022/Supporting_tasks/DR2022_09/Hydration_GB_1982-2010.csv")
# 


# take hydration data from Survey summary output instead!

load("Y:/Offshore/Assessment/Data/Survey_data/2022/Survey_summary_output/testing_results_commercial.Rdata")

# MW.dat contains commercial samples
dim(MW.dat)
head(MW.dat)
MW.dat$dataset <- "fishing"
MW.dat.sum <- MW.dat %>%
  dplyr::group_by(bank, cruise, year=as.numeric(year)) %>%
  dplyr::summarize(samples = n()) %>%
  dplyr::mutate(dataset="old")

# MW.dat.new contains all survey samples (incl historical)
# it does NOT include wgw, so need to borrow code from get.offshore.survey to get it back!

require(ROracle) || stop("Package ROracle cannot be found")
require(lubridate) || stop("Package lubridate cannot be found")

funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/convert.dd.dddd.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} # end for(un in funs)

#DK August 20, 2015 Note: Need this to open the channel, we need to get one more view, or more general access to the OSTOWS table
# so that the .rProfile method works, for now the workaround would be to put the general (admin?) un/pw into your rprofile...
# DK revised April 2018 to ROracle
un=""
pw=""
db.con="ptran"
chan <-dbConnect(dbDriver("Oracle"),username=un, password=pw,db.con)

#####################################################################################################################
# Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
# Key is to import those tables and send it out of this file looking identical!  
######################################################################################################################
db <- "SCALOFF" ### CHANGE HUMF TO SCALOFF!!!
#message("reminder that this is pulling data from HUMF views, not production SCALOFF")

#qu.strata <- "select * from SCALOFF.OSSTRATA"
# DK Oct 29, 2015, don't need tow data either, we don't ever use it.... 
qu.sample <- paste0("select * from ", db, ".OSSAMPLES_SS_VW")
qu.sample.ice <- paste0("select * from ", db, ".OSSAMPLES_ICE_VW")
#qu.tow <- "select * from HUMF.OSTOWS"

# Grab the SQL data from the respective database tables
#strata <- sqlQuery(chan, qu.strata)
# Revised to be ROracle query
samp <- dbGetQuery(chan, qu.sample)
sampice <- dbGetQuery(chan, qu.sample.ice)
#tow <- sqlQuery(chan, qu.tow)
dbDisconnect(chan)

samp$species <- "seascallop"

if(dim(sampice)[1] >0) sampice$species <- "icelandic"

samp <- rbind(samp, sampice)

# first we need to convert the locations into decimal degree and then calculate the mid-point of the tow as before.
#Source1 source("fn/Survey/convert.dd.dddd.r")
samp$slat<-convert.dd.dddd(samp$START_LAT)
samp$slon<-convert.dd.dddd(samp$START_LON)
samp$elat<-convert.dd.dddd(samp$END_LAT)
samp$elon<-convert.dd.dddd(samp$END_LON)

# Take the start/end postion and takes the mid-point of the tow as strata
samp$lon<-with(samp,apply(cbind(elon,slon),1,mean))
samp$lat<-with(samp,apply(cbind(elat,slat),1,mean))

# Convert the depth from fathoms to meters.
samp$depth<-samp$DEPTH_F*1.8288

# correct for time zone
samp$TOW_DATE <- ymd_hms(samp$TOW_DATE)

if(tz(samp$TOW_DATE) == "UTC"){
  samp$TOW_DATE <- samp$TOW_DATE + hours(4)
}
if(!tz(samp$TOW_DATE) == "UTC"){
  stop("In get.offshore.survey, data were read in using a timezone other than UTC. You need to correct time zone in get.offshore.survey.")
}

MWs <- samp
names(MWs)[which(names(MWs) %in% c("TOW_NO", "slat", "slon", "elat", "elon", 
                                   "DEPTH_F", "YEAR", "lon", "lat", "depth", "CRUISE", 
                                   "SCALLOP_NUM", "WET_MEAT_WGT", "SHELL_HEIGHT", "SPECIES_ID"))] <- c("year", "cruise","tow", 
                                                                                                       "species", "depth.f", "scalnum",
                                                                                                       "sh", "wmw", 
                                                                                                       "slat","slon","elat","elon",
                                                                                                       "lon","lat",
                                                                                                       "depth")
names(MWs)[which(names(MWs) %in% c("SEX_ID", "MATURITY_ID", "WET_GONAD_WGT", "DRY_MEAT_WGT", 
                                   "DRY_GONAD_WGT", "WET_SOFT_PARTS_WGT"))] <- c("sex", "mat", "wgw", "dmw", "dgw", "wspw")
need <- unique(c(names(MW.dat.new), names(MW.dat), "AREA_CD"))
need <- need[!need == "bank"]
need <- need[!need %in% c("slat", "slon", "elat", "elon")]
MWs <- dplyr::select(MWs, names(MWs)[which(names(MWs) %in% need)])
MWs$dataset <- "survey"
MWs$species[MWs$species==1] <- 'seascallop'
MWs$species[MWs$species==2] <- 'icelandic'
dim(MWs)

dim(MW.dat.new)
head(MW.dat.new)
MW.dat.new$dataset <- "survey"

# table(MWs$year, is.na(MWs$wgw))
# table(MWs$year, is.na(MWs$dmw))
# table(MWs$year, is.na(MWs$sex))
# table(MWs$year, MWs$tow==0)
# table(MW.dat.new$year, MW.dat.new$tow==0)

# join database results to MW.dat.new
MW.dat.new <- left_join(MW.dat.new, MWs)

MW.dat.new.sum <- MW.dat.new %>%
  dplyr::group_by(bank, cruise, year=as.numeric(year)) %>%
  dplyr::summarize(samples = n()) %>%
  dplyr::mutate(dataset="new")

# combine to compare
MW.dat.sum <- rbind(MW.dat.sum, MW.dat.new.sum)

MW.dat.sum %>%
  group_by(bank, year, cruise) %>%
  dplyr::summarize(dataset=length(unique(dataset))) %>%
  filter(dataset>1)

# Sable 1984 has tows in both datasets because 4 tows could not be found for inclusion in db. This is fine.
MW.dat.sum[MW.dat.sum$bank=="Sab" & MW.dat.sum$cruise=="P306",]
unique(MW.dat[MW.dat$bank=="Sab" & MW.dat$cruise=="P306",]$tow)
unique(MW.dat.new[MW.dat.new$bank=="Sab" & MW.dat.new$cruise=="P306",]$tow)

ggplot() + 
  geom_text(data=MW.dat.sum, aes(year, samples, label=cruise, colour=dataset)) +
  facet_wrap(~bank, scales="free_y")

MW.dat$tow <- as.numeric(MW.dat$tow)
MW.dat.new$tow <- as.numeric(MW.dat.new$tow)
MW.dat$year <- as.numeric(MW.dat$year)
MW.dat.new$year <- as.numeric(MW.dat.new$year)

# join em together
mw_dat_all <- full_join(MW.dat, MW.dat.new)

dim(mw_dat_all)
dim(MW.dat)[1] + dim(MW.dat.new)[1]

head(mw_dat_all)

table(mw_dat_all$dataset, mw_dat_all$year)
# compare this to table(hyd$Source, hyd$year)

# spatial next, need to exclude samples with missing coords
mw_dat_all <- mw_dat_all[!is.na(mw_dat_all$lon),]
# 30 samples from GB removed due to no spatial information

mw_dat_all_sf <- st_as_sf(x=mw_dat_all, coords=c(X="lon", Y="lat"), crs=4326)

# intersect with GBa/GBb polys (subset to GB)
mw_dat_all_sf_gb <- st_intersection(mw_dat_all_sf, offshore[offshore$ID %in% c("SFA27A", "SFA27B"),])

head(mw_dat_all_sf_gb)

# summaries for a little QA/QC. Number of tows by month/year
month_yr_tow <- mw_dat_all_sf_gb %>%
  group_by(cruise, year, month, dataset) %>%
  summarize(tows = length(unique(tow))) %>%
  mutate(date=ymd(paste0(year, "-", month, "-01")))

# number of samples by month/year
month_yr_samples <- mw_dat_all_sf_gb %>%
  group_by(cruise, year, month, dataset) %>%
  summarize(samples=n()) %>%
  mutate(date=ymd(paste0(year, "-", month, "-01")))

ggplot() + geom_line(data=month_yr_tow, aes(date, tows, colour=dataset, group=dataset), alpha=0.5) +
  geom_text(data=month_yr_tow, aes(label=month, date, tows))
ggplot() + geom_line(data=month_yr_samples[month_yr_samples$year<2011,], aes(date, samples, colour=dataset, group=dataset), alpha=0.5) +
  geom_text(data=month_yr_samples[month_yr_samples$year<2011,], aes(label=month, date, samples)) 


# Convert shell heights to decimeters
mw_dat_all_gb <- mw_dat_all_sf_gb
st_geometry(mw_dat_all_gb) <- NULL
mw_dat_all_gb <- cbind(mw_dat_all_gb, st_coordinates(mw_dat_all_sf_gb))

mw_dat_all_gb_hons <- mw_dat_all_gb %>%
  select(cruise, year, month, day, tow, X, Y, bank, depth, scalnum, sh, wmw, wgw, dmw, wspw, sex, mat, dataset)

mw_dat_all_gb_hons$file <- "ss"

# crop to just 1982-2009 because of consistency in sampling
mw_dat_all_gb_hons <- mw_dat_all_gb_hons[mw_dat_all_gb_hons$year<2010,]

mw_dat_all_gb_hons <- mw_dat_all_gb_hons %>%
  select(cruise, year, month, day, tow, X, Y, bank, depth, scalnum, sh, wmw, wgw, dmw, wspw, sex, mat, dataset)

# For honours student:
write.csv(mw_dat_all_gb_hons, "Y:/Offshore/Assessment/2022/Supporting_tasks/DR2022_09/Hydration_GB_1982-2009_rev.csv")

ggplot() + geom_point(data=mw_dat_all_gb_hons, aes(wmw, wspw))

head(mw_dat_all_gb_hons[!is.na(mw_dat_all_gb_hons$wspw),])


month_yr_tow <- mw_dat_all_gb_hons %>%
  group_by(cruise, year, month, dataset) %>%
  summarize(tows = length(unique(tow))) %>%
  mutate(date=ymd(paste0(year, "-", month, "-01")))

png("Y:/Offshore/Assessment/2022/Supporting_tasks/DR2022_09/summary_plot.png", height=10, width=20, units="in", res=400) 
print(ggplot() + 
  geom_line(data=month_yr_tow, aes(date, tows))+
  geom_text(data=month_yr_tow, aes(date, tows, label=month)) + facet_wrap(~dataset, scales="free_y", nrow=2) +
  scale_x_date(breaks="2 years") +
  ggtitle("Hydration data overview",subtitle = "number labels correspond to months"))
dev.off()


db_table <- as.data.frame(table(mw_dat_all_gb$month, mw_dat_all_gb$year))
eliz_table <- as.data.frame(table(hyd$month, hyd$year))

names(db_table) <- c("month", "year", "number_in_db")
names(eliz_table) <- c("month", "year", "number_in_eliz")

compare <- full_join(db_table, eliz_table)
compare$match <- ifelse(compare$number_in_db==compare$number_in_eliz, "yes", "no")

write.csv(compare, "Y:/Offshore/Assessment/2022/Supporting_tasks/DR2022_09/Hydration_comparison3.csv")

db_table2 <- as.data.frame(table(mw_dat_all_gb$dataset, mw_dat_all_gb$year))
eliz_table2 <- as.data.frame(table(hyd$Source, hyd$year))

levels(eliz_table2$Var1) <- c("fishing", "survey")

names(db_table2) <- c("source", "year", "number_in_db")
names(eliz_table2) <- c("source", "year", "number_in_eliz")

compare2 <- full_join(db_table2, eliz_table2)
compare2$match <- ifelse(compare2$number_in_db==compare2$number_in_eliz, "yes", "no")

write.csv(compare2, "Y:/Offshore/Assessment/2022/Supporting_tasks/DR2022_09/Hydration_comparison4.csv")




# MISSING FISHERY SAMPLES FROM 2001-2005

### compare elizabeth's to what is used for SS (post database update)



names(mw_dat_all_gb_hons)
names(hyd) <- c("year", "month", "date", "tow", "X", "Y", "depth", "scalnum", "wmw", "sh", "source", "file")
mw_dat_all_gb_hons$tow <- as.character(mw_dat_all_gb_hons$tow)
combined <- full_join(mw_dat_all_gb_hons, hyd)

head(combined)

combined$dataset[is.na(combined$dataset) & combined$source=="Survey"] <- "survey"
combined$dataset[is.na(combined$dataset) & combined$source=="Commercial"] <- "fishing"

comb_sum <- combined %>%
  group_by(year, file, dataset) %>%
  summarize(n = n(),
            med_sh = median(sh),
            med_wmw = median(wmw))

ggplot() + geom_point(data=comb_sum, aes(year, n, size=file, colour=file), shape=21) + theme_minimal() + facet_wrap(~dataset, ncol=1)
ggplot() + geom_point(data=comb_sum, aes(year, med_sh, size=file, colour=file), shape=21) + theme_minimal() + facet_wrap(~dataset)
ggplot() + geom_point(data=comb_sum, aes(year, med_wmw, size=file, colour=file), shape=21) + theme_minimal() + facet_wrap(~dataset)

combined$tow <- gsub(x=combined$tow, pattern="C", replacement="", fixed=T)

tow_comb_sum <- combined %>%
  group_by(year, file, dataset, tow) %>%
  summarize(n = n(),
            med_sh = median(sh),
            med_wmw = median(wmw))


#1982
comb_sum[comb_sum$year==1982,] # fine. Elizabeth coded some commercial tows as survey tows
table(combined[combined$year==1982,]$cruise)

#1987
comb_sum[comb_sum$year==1987,] # SS has one more survey sample than elizabeth
combined %>% filter(year==1987) %>% group_by(dataset, file, tow) %>%
  summarize(n=n()) %>%
  ggplot() + 
  geom_point(aes(tow, n, colour=file, size=file), shape=21) + facet_wrap(~dataset, ncol=1) + coord_flip()

combined[combined$year==1987 & combined$tow %in% 602:606,]

#1988
comb_sum[comb_sum$year==1988,] # Elizabeth has more commercial tows than the database
table(combined[combined$year==1988,]$tow, combined[combined$year==1988,]$file)

tows <- unique(combined[, c("cruise", "year", "month", "day", "tow", "X", "Y", "bank", "depth", "dataset", "file")])

ggplot() + geom_point(data=tows[tows$year==1988,], aes(X, Y, size=file, colour = dataset), shape=21) #+
 # geom_text(data=tows[tows$year==1988,], aes(X, Y, label=tow)) +
  # xlim(-66.4, -66.32)+
  # ylim(41.55, 41.65)
  #geom_point(data=tows[tows$year==1988 & tows$tow==306,], aes(X, Y))

ggplot() + geom_histogram(data=combined[combined$year==1988,], aes(Y, fill=file), position="dodge") 

ggplot() + geom_point(data=tow_comb_sum[tow_comb_sum$year==1988,], aes(tow, n, size=file, colour = dataset), shape=21) + facet_wrap(~dataset)

tow_comb_sum[tow_comb_sum$file=="eliz" & tow_comb_sum$year==1988 & tow_comb_sum$tow==100,]

ggplot() + geom_point(data=combined[combined$file=="eliz" & combined$year==1988 & combined$tow==100,], aes(X, Y), colour="red") + 
  geom_point(data=combined[combined$file=="ss" & combined$year==1988 & combined$tow==100,], aes(X, Y), colour="blue") 

comb_sum <- combined %>%
  group_by(year, file) %>%
  summarize(n = n(),
            med_sh = median(sh),
            med_wmw = median(wmw))


# following the same methods as for Survey Summary (and survey indices!)
# Calculate the meat weight shell height relationship, remember if b.par = 3 this assumes an allometric realtionship.
# Notice that we use a different random effect here, it is ID not tow, this is done since we may have the same tow # in different years.
mw_dat_all_sf_gb$sh<-mw_dat_all_sf_gb$sh/100
SpatHtWt.fit<-shwt.lme(mw_dat_all_gb[!is.na(mw_dat_all_gb$wmw),],random.effect='ID',b.par=3,verbose=F)
names(SpatHtWt.fit$fit) <- c("ID", "fit")
mw_dat_all_gb <- left_join(mw_dat_all_gb, SpatHtWt.fit$fit)

mw_dat_all_gb$day[is.na(mw_dat_all_gb$day)] <- 1
mw_dat_all_gb$date <- ymd(paste0(mw_dat_all_gb$year, "-", mw_dat_all_gb$month, "-", mw_dat_all_gb$day))

mw_dat_all_gb <- mw_dat_all_gb[mw_dat_all_gb$year<2001,]

ggplot() + 
  geom_boxplot(data=mw_dat_all_gb, aes(floor_date(date, unit = "month"), fit, group=floor_date(date, unit = "month"))) +
  theme_minimal()

ggplot() + 
  geom_boxplot(data=mw_dat_all_gb, aes(as.factor(month), fit, group=as.factor(month))) + 
  theme_minimal()

ggplot() + 
  geom_smooth(data=mw_dat_all_gb, aes(month, fit)) + 
  theme_minimal() +
  scale_x_continuous(breaks=1:12)

ggplot() +
  geom_point(data=mw_dat_all_gb, aes(as.factor(month), fit)) +
  geom_smooth(data=mw_dat_all_gb, aes(month, fit), method="gam") + 
  coord_polar(theta="x", start=0) + 
  theme_minimal()

cond_annual <- mw_dat_all_gb %>%
  group_by(year) %>%
  summarize(annual_med_CF = median(fit),
            annual_mean_CF = mean(fit))

mw_dat_all_gb <- left_join(mw_dat_all_gb, cond_annual)

mw_dat_all_gb$rel_month_CF <- mw_dat_all_gb$fit/mw_dat_all_gb$annual_med_CF
mw_dat_all_gb$rel_month_CF_mean <- mw_dat_all_gb$fit/mw_dat_all_gb$annual_mean_CF

ggplot() + 
  geom_boxplot(data=mw_dat_all_gb, aes(as.factor(month), rel_month_CF, group=as.factor(month))) + 
  geom_hline(data=mw_dat_all_gb, aes(yintercept=1)) +
  theme_minimal()

ggplot() + 
  geom_smooth(data=mw_dat_all_gb, aes(x = month, y=rel_month_CF), method="gam") +
  geom_hline(data=mw_dat_all_gb, aes(yintercept=1)) +
  theme_minimal() +
  scale_x_continuous(breaks=1:12) +
  ylab("Monthly Condition relative to annual median") +
  xlab("Month")

ggplot() + 
  geom_boxplot(data=mw_dat_all_gb, aes(as.factor(month), rel_month_CF_mean, group=as.factor(month))) + 
  geom_hline(data=mw_dat_all_gb, aes(yintercept=1)) +
  theme_minimal()

ggplot() + 
  geom_smooth(data=mw_dat_all_gb, aes(x = month, y=rel_month_CF_mean), method="gam") +
  geom_hline(data=mw_dat_all_gb, aes(yintercept=1)) +
  theme_minimal() +
  scale_x_continuous(breaks=1:12)


# Jan, Feb, Mar, Jun, July
