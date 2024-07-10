source("C:/Users/keyserf/documents/github/assessment_fns/Fishery/logs_and_fishery_data.r")
logs_and_fish("offshore", 2022:2024, export=F, get.local=T, get.marfis = F, direct="Y:/Offshore/Assessment/")
names(new.log.dat)

new.log.dat %>%
  distinct(ves, licence)

company <- new.log.dat %>% filter(licence==142070)
trips <- company %>% 
  group_by(year, tripnum, date.land, date.sail, ves) %>%
  summarise() %>%
  mutate(duration = ymd(date.land)-ymd(date.sail), year.fac=as.factor(year)) %>%
  ungroup()

ggplot() + geom_boxplot(data=trips, aes(x=year.fac, y=duration, fill=ves)) + 
  geom_jitter(data=trips, aes(as.factor(year), y=duration, shape=ves), position=position_jitterdodge(jitter.width = 0.1, jitter.height = 0))

str(trips)

