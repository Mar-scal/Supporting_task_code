direct <- "Y:/Offshore/Assessment/"

hyd <- read.csv("Y:/Offshore/Assessment/2022/Supporting_tasks/DR2022_09/Hydration_GB_1982-2009.csv")

require(ggplot2)
require(lubridate)
require(tidyverse)

head(hyd)
hyd$day[is.na(hyd$day)] <- 1
hyd$date <- dmy(paste0(hyd$day, "-", hyd$month, "-", hyd$year))

hyd$year <- year(hyd$date)
hyd$month <- month(hyd$date)

ggplot() + geom_bar(data=hyd, aes(year)) + facet_wrap(~bank) + theme_bw() 

ggplot() + geom_bar(data=hyd, aes(month)) + facet_wrap(~year)+ theme_bw() + 
  ggtitle("Georges samples only")


sample_summary <- hyd %>%
  group_by(month, year) %>%
  summarize(n=n())

time <- expand.grid(year=unique(hyd$year), month=unique(hyd$month))

sample_summary <- left_join(time, sample_summary)

sample_summary$n[is.na(sample_summary$n)] <- 0

ggplot() + geom_boxplot(data=sample_summary, aes(y=n, group=month)) + theme_bw() + 
  ggtitle("Georges samples only") 

ggplot() + geom_line(data=sample_summary, aes(y=n, x=month, colour=year, group=year)) + theme_bw() + 
  ggtitle("Georges samples only")

ggplot() + geom_tile(data=sample_summary[sample_summary$n>0,], aes(x=year, y=month, fill=n), colour="black") + theme_bw() +
  geom_tile(data=sample_summary[sample_summary$n==0,], aes(x=year, y=month), fill="white", colour="black") + 
  scale_fill_gradient(low="grey", high="black", name="Number of samples") +
  scale_y_continuous(breaks=1:12) +
  xlab("Year") +
  ylab("Month") +
  ggtitle("Georges Bank meat weight-shell height samples")

hyd.gb <- hyd

means <- hyd %>%
  group_by(month) %>%
  summarize(mean.wmw = mean(wmw),
            sd.wmw = sd(wmw))

ggplot() + geom_point(data=means, aes(x=month, mean.wmw)) +
  geom_errorbar(data=means, aes(x=month, ymin=mean.wmw-(1.96*sd.wmw), ymax=mean.wmw+(1.96*sd.wmw))) +
  ylab("Meat weight (g)") + 
  xlab("Month") +
  scale_x_continuous(breaks=1:12) + 
  theme_minimal()
  

ggplot() + geom_boxplot(data=hyd.gb, aes(x=month, y=wmw, group=month))
ggplot() + geom_point(data=hyd.gb, aes(x=month, y=wmw)) + 
  geom_smooth(data=hyd.gb, aes(x=month, y=wmw)) +
  theme_bw()

ggplot() + geom_point(data=hyd.gb, aes(x=month, y=wmw, group=month)) + 
  geom_smooth(data=hyd.gb, aes(x=month, y=wmw), method="gam") +
  theme_bw()

hyd.gb$CF <- hyd.gb$wmw/(hyd.gb$sh/100)
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

hyd$sh<-hyd$sh/100
funs <- c(#"https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/github_spatial_import.R",
          "https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Survey_and_OSAC/shwt.lme.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} # end for(un in funs)

SpatHtWt.fit<-shwt.lme(hyd[!is.na(hyd$wmw),],random.effect='ID',b.par=3,verbose=F)
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
  scale_x_continuous(breaks=1:12)

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

