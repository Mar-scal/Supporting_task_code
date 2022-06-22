sets <- read.csv("Y:/Bycatch/data/Observed scallop trip metadata_tidysets_2022-05-13.csv")

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
  distinct() %>%
  group_by(area, year) %>%
  summarize(number_of_trips = n()) %>%
  arrange(year) %>%
  filter(year>2015)

all <- expand.grid(area=unique(trip_summary$area), year=unique(trip_summary$year)) %>%
  filter(!is.na(area))

trip_summary <- left_join(all, trip_summary[!is.na(trip_summary$area),])
  
SFA <- data.frame(SFA=c(25, 25, 25, 26, 26, 26, 27, 27), area = c("Sab", "Mid", "Ban", "Ger", "BBn", "BBs", "GBa", "GBb"))

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

logs_and_fish(loc="offshore", year=2016:2022, get.marfis=F, direct="Y:/Offshore/Assessment/")

fishing <- new.log.dat %>%
  group_by(bank, year) %>%
  summarize(n_fishing_trips = length(unique(tripnum)))

trip_summary <- left_join(trip_summary, fishing)

trip_summary$n_fishing_trips[is.na(trip_summary$n_fishing_trips)] <- 0
trip_summary$number_of_trips[is.na(trip_summary$number_of_trips)] <- 0
trip_summary$number_of_trips_sfa[is.na(trip_summary$number_of_trips_sfa)] <- 0

tail(trip_summary)
write.csv(trip_summary, "Y:/Bycatch/Requests/OffshoreScallop_MSC/Trips_by_bank.csv")

png("Y:/Bycatch/Requests/OffshoreScallop_MSC/Trips_by_bank.png", width=11, height=8.5, units="in", res=420)
ggplot() + geom_bar(data=trip_summary[!is.na(trip_summary$bank),], aes(x=year, y=number_of_trips), stat="identity") + facet_wrap(~bank) +
  theme_bw() +
  ylab("Number of trips") + xlab("Year")
dev.off()


# meat count check (also for MSC purposes)
load("Y:/Offshore/Assessment/Data/Survey_data/2021/Survey_summary_output/Survey_all_results - 2021FINAL.Rdata")

for (i in c("Mid", "Sab", "BBs", "BBn", "Ger", "GBa", "GBb")){
  print(ggplot() + geom_boxplot(data=surv.Live[[i]], aes(year, meat.count, group=year), outlier.shape = NA) + ylim(0,500) +
          ggtitle(i))
}
