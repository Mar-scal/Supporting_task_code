sets <- read.csv("Y:/Bycatch/data/Observed scallop trip metadata_tidysets_2023-03-07.csv")

head(sets)

require(dplyr)
require(tidyverse)
require(lubridate)
require(ggplot2)

trip_summary <- sets %>% 
  mutate(year=year(ymd(LANDING_DATE))) %>%
  group_by(area, TRIP, year) %>%
  summarize(number_of_sets = n()) %>%
  arrange(year)

trip_summary <- trip_summary %>% 
  filter(number_of_sets > 1) %>%
  select(TRIP, area, year) %>%
  distinct() 

trip_summary$area <- gsub(trip_summary$area, pattern="GBa", replacement="GB")
trip_summary$area <- gsub(trip_summary$area, pattern="GBb", replacement="GB")
trip_summary <- distinct(trip_summary)

trip_summary <- trip_summary %>%
  group_by(area, year) %>%
  summarize(number_of_trips = n()) %>%
  arrange(year) %>%
  filter(year>2015)

all <- expand.grid(area=unique(trip_summary$area), year=unique(trip_summary$year)) %>%
  filter(!is.na(area))

trip_summary <- left_join(all, trip_summary[!is.na(trip_summary$area),])
  
SFA <- data.frame(SFA=c(25, 25, 25, 26, 26, 26, 27), area = c("Sab", "Mid", "Ban", "Ger", "BBn", "BBs", "GB"))

trip_summary<- left_join(trip_summary, SFA)

trip_summary_sfa <- trip_summary %>%
  group_by(SFA, year) %>%
  summarize(number_of_trips_sfa=sum(number_of_trips, na.rm=T)) %>%
  filter(!is.na(SFA))

trip_summary <- left_join(trip_summary, trip_summary_sfa) %>%
  mutate(bank=area)

trip_summary <- trip_summary %>%
  ungroup() %>%
  select(-area) %>%
  select(bank, year, number_of_trips, SFA, number_of_trips_sfa)

trip_summary <- trip_summary %>% arrange(year, SFA, bank)


source("C:/Users/keyserf/Documents/Github/Assessment_fns/Fishery/logs_and_fishery_data.r")

logs_and_fish(loc="offshore", year=2016:2022, get.marfis=T, direct="Y:/Offshore/Assessment/")
pre23 <- new.log.dat

logs_and_fish(loc="offshore", year=2023, get.local=F, get.marfis=T, un = un.ID, pw = pwd.ID, direct="Y:/Offshore/Assessment/")

head(pre23)
head(marfis.log.dat)
marfis.log.dat$watch <- as.numeric(substr(marfis.log.dat$watch, 1,1))
marfis.log.dat$numbags <- as.numeric(marfis.log.dat$numbags)
marfis.log.dat$numshuck <- as.numeric(marfis.log.dat$numshuck)

new.log.dat <- full_join(pre23, marfis.log.dat)

new.log.dat$bank <- gsub(x=new.log.dat$bank, pattern="GBa", replacement ="GB")
new.log.dat$bank <- gsub(x=new.log.dat$bank, pattern="GBb", replacement ="GB")

fishing <- new.log.dat %>%
  group_by(bank, year) %>%
  summarize(n_fishing_trips = length(unique(tripnum)))

trip_summary <- left_join(trip_summary, fishing)

trip_summary$n_fishing_trips[is.na(trip_summary$n_fishing_trips)] <- 0
trip_summary$number_of_trips[is.na(trip_summary$number_of_trips)] <- 0
trip_summary$number_of_trips_sfa[is.na(trip_summary$number_of_trips_sfa)] <- 0

tail(trip_summary)
write.csv(trip_summary, "Y:/Bycatch/Requests/OffshoreScallop_MSC/Trips_by_bank_2023.csv")

png("Y:/Bycatch/Requests/OffshoreScallop_MSC/Trips_by_bank_2023.png", width=11, height=8.5, units="in", res=420)
ggplot() + geom_bar(data=trip_summary[!is.na(trip_summary$bank),], aes(x=year, y=number_of_trips), stat="identity") + facet_wrap(~bank) +
  theme_bw() +
  ylab("Number of trips") + xlab("Year")
dev.off()


# meat count check (also for MSC purposes)
load("Y:/Offshore/Assessment/Data/Survey_data/2022/Survey_summary_output/Survey_all_results.Rdata")

for (i in c("Mid", "Sab","BBn", "Ger", "GBa", "GBb")){
  print(ggplot() + geom_boxplot(data=surv.Live[[i]], aes(year, meat.count, group=year), outlier.shape = NA) + ylim(0,500) +
          ggtitle(i))
}


trip_summary <- read.csv("Y:/Bycatch/Requests/OffshoreScallop_MSC/Trips_by_bank_2023.csv")

trip_summary$bank[trip_summary$bank %in% c("GBa", "GBb")] <- "GB"
trip_summary <- trip_summary %>%
  group_by(bank, year) %>%
  summarize(number_of_trips=sum(number_of_trips),
            number_of_trips_sfa = sum(number_of_trips_sfa),
            n_fishing_trips=sum(n_fishing_trips))

ggplot() + geom_point(data=trip_summary, aes(year, number_of_trips/n_fishing_trips)) +
  facet_wrap(~bank, scales="free")

trip_summary$coverage <- trip_summary$number_of_trips/trip_summary$n_fishing_trips
trip_summary[trip_summary$year==2022,]
trip_summary %>% group_by(bank) %>% summarize(mean(coverage, na.rm=T), median(coverage, na.rm=T))

discards <- read.csv("Y:/Bycatch/data/Bycatch monthly historical.csv")
ggplot() + geom_point(data=discards[discards$monthnum==12,], aes(year, CA.Discard.YT)) +
  geom_smooth(data=discards[discards$monthnum==12,], aes(year, CA.Discard.YT), method="lm") 

ggplot() + geom_point(data=discards[discards$monthnum==12,], aes(year, CA.Discard.Cod)) +
  geom_smooth(data=discards[discards$monthnum==12,], aes(year, CA.Discard.Cod), method="lm") 

ggplot() + geom_point(data=discards[discards$monthnum==12,], aes(year, CA.Discard.Haddock)) +
  geom_smooth(data=discards[discards$monthnum==12,], aes(year, CA.Discard.Haddock), method="lm") 
