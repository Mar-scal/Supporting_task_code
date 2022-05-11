direct <- "Y:/Offshore/Assessment/"

hyd <- read.csv(paste0(direct, "Data/Hydration/ElizWork/Hydration_82-10_FINAL.csv"))

require(ggplot2)
require(lubridate)
require(tidyverse)

head(hyd)

hyd$date <- dmy(hyd$TOW_DATE)

hyd <- hyd[!is.na(hyd$date),]
hyd$year <- year(hyd$date)
hyd$month <- month(hyd$date)

ggplot() + geom_bar(data=hyd, aes(year)) + facet_wrap(~MGT_AREA_CD) + theme_bw() 

ggplot() + geom_bar(data=hyd[hyd$MGT_AREA_CD == "Georges",], aes(month)) + facet_wrap(~year)+ theme_bw() + 
  ggtitle("Georges samples only")


sample_summary <- hyd %>%
  group_by(month, year, MGT_AREA_CD) %>%
  summarize(n=n())

time <- expand.grid(year=unique(hyd$year), month=unique(hyd$month), MGT_AREA_CD = unique(hyd$MGT_AREA_CD))

sample_summary <- left_join(time, sample_summary)

sample_summary$n[is.na(sample_summary$n)] <- 0

ggplot() + geom_boxplot(data=sample_summary, aes(y=n, group=month)) + theme_bw() + 
  ggtitle("Georges samples only")

ggplot() + geom_line(data=sample_summary, aes(y=n, x=month, colour=year, group=year)) + theme_bw() + 
  ggtitle("Georges samples only")

ggplot() + geom_tile(data=sample_summary[sample_summary$n>0,], aes(x=year, y=month, fill=n), colour="black") + theme_bw() +
  geom_tile(data=sample_summary[sample_summary$n==0,], aes(x=year, y=month), fill="white", colour="black") + 
  scale_fill_gradient(low="grey", high="black") +
  scale_y_continuous(breaks=1:12)+
  facet_wrap(~MGT_AREA_CD)

hyd.gb <- hyd[hyd$MGT_AREA_CD=="Georges",]

ggplot() + geom_boxplot(data=hyd.gb, aes(x=month, y=WET_MEAT_WGT, group=month))
ggplot() + geom_point(data=hyd.gb, aes(x=month, y=WET_MEAT_WGT)) + 
  geom_smooth(data=hyd.gb, aes(x=month, y=WET_MEAT_WGT)) +
  theme_bw()

ggplot() + geom_point(data=hyd.gb, aes(x=month, y=WET_MEAT_WGT, group=month)) + 
  geom_smooth(data=hyd.gb, aes(x=month, y=WET_MEAT_WGT), method="gam") +
  theme_bw()

hyd.gb$CF <- hyd.gb$WET_MEAT_WGT/(hyd.gb$SHELL_HEIGHT/100)
hyd.gb <- hyd.gb %>% 
  group_by(year) %>%
  mutate(median=median(CF))

require(mgcv)
gam.mod <- gam(CF ~ s(month),data= hyd.gb)

ggplot() + geom_point(data=hyd.gb, aes(x=month, y=CF)) + 
  geom_smooth(data=hyd.gb, aes(x=month, y=CF), method="gam") +
  theme_bw()

ggplot() + geom_point(data=hyd.gb, aes(x=month, y=CF/median)) + 
  geom_smooth(data=hyd.gb, aes(x=month, y=CF/median)) +
  theme_bw()

ggplot() + geom_point(data=hyd.gb, aes(x=month, y=CF-median)) + 
  geom_smooth(data=hyd.gb, aes(x=month, y=CF-median)) +
  theme_bw()


# condition is g/dm3 aka g/100mm


