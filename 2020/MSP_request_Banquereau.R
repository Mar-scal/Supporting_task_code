### Banquereau survey data for MSP (DR2020_04) 2020-08-20

load("C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")

require(tidyverse)

ban.surv <- surv.dat$Ban %>% 
  dplyr::select(ID, year, bank, tow, lon, lat, state, species, pre, rec, com, tot) %>%
  filter(state=="live") %>%
  arrange(year, tow)%>%
  rename(pre_recruit_npertow = pre,
         recruit_npertow = rec,
         commercial_npertow = com,
         total_npertow = tot)

head(ban.surv)

banice.surv <- surv.dat$BanIce %>% 
  dplyr::select(ID, year, bank, tow, lon, lat, state, species, pre, rec, com, tot) %>%
  filter(state=="live") %>%
  mutate(species="icelandic") %>%
  arrange(year, tow)%>%
  rename(pre_recruit_npertow = pre,
         recruit_npertow = rec,
         commercial_npertow = com,
         total_npertow = tot)

write.csv(ban.surv, "C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Data Requests/2020/MSP_request/Banquereau_seascallop_survey_1989-2019.csv ")
write.csv(banice.surv, "C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Data Requests/2020/MSP_request/Banquereau_icelandscallop_survey_2006-2019.csv ")


############ fishery too... covered by CDD request though!

# set your directory
direct <- "V:/Offshore/Assessment/"

source(paste0(direct, "Assessment_fns/Fishery/logs_and_fishery_data.r"))

# to export csv (adjust years to whatever you want):
logs_and_fish(loc="offshore", year=1989:2019, get.marfis = F, export = T, direct = direct, direct_fns="V:/Offshore/Assessment/Assessment_fns/")

names(new.log.dat)
names(old.log.dat)

ban.new <- new.log.dat %>%
  filter(bank=="Ban") %>%
  dplyr::select(year, date, tripnum, nafo, sfa, bank, lon, lat, watch, pro.repwt, hm) %>%
  rename(kg=pro.repwt) %>% ### DATES ARE BREAKING IN CSV
  arrange(year, date, tripnum)

ban.new$date <- as.character(ban.new$date)

ban.old <- old.log.dat %>%
  filter(bank=="Ban") %>%
  dplyr::select(year, date, trip.id, nafo, bank, lon, lat, pro.repwt, hm) %>%
  rename(kg=pro.repwt, tripnum=trip.id) %>%
  mutate(sfa=NA, watch=NA) %>%
  dplyr::select(year, date, tripnum, nafo, sfa, bank, lon, lat, watch, kg, hm) %>%
  arrange(year, date, tripnum)

ban.fish <- rbind(ban.old, ban.new)

min(ban.fish$year)

write.csv(ban.fish, "C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Data Requests/2020/MSP_request/Banquereau_scallop_fishery_1989-2019.csv")


