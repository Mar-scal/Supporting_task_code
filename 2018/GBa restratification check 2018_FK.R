# Checking historical survey allocations on GBa

load("Y:/Offshore scallop/Assessment/Data/Survey_data/2017/Survey_summary_output/Survey_all_results.Rdata") 

options(scipen=999)
## totals
means <- aggregate(tot ~ Strata_ID + year, surv.Rand$GBa, mean)
names(means)[3] <- "means"
vars <- aggregate(tot ~ Strata_ID + year, surv.Rand$GBa, function(x) sd(x)^2)
names(vars)[3] <- "vars"
tows <- aggregate(tot ~ Strata_ID + year, surv.Rand$GBa, length)
names(tows)[3] <- "tow"

stats <- plyr::join(means, vars, type="full")
stats <- plyr::join(stats, tows, type="full")

stats$Strata_lab <- as.factor(stats$Strata_ID)
levels(stats$Strata_lab) <- c("Low", "Medium North", "High North", "Very High North",
                               "Medium South", "High South", "Very High South")

str(stats)
stats$year <- as.numeric(stats$year)

strata_info <- read.csv("Y:/Offshore scallop/Assessment/Data/Survey_data/survey_information.csv")
strata_info <- strata_info[strata_info$label=="GBa",]

stats <- plyr::join(stats, strata_info, type="left")
stat_sum <- plyr::ddply(.data=stats[stats$year > 2000,], .(year),
                  summarize,
                  totaltows = sum(tow),
                  totalarea = sum(area_km2))

stats <- join(stats, stat_sum, type="left")


require(ggrepel)
pdf(file = "Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Exploratory_figures/GBa/GBa stratification checks_ALL.pdf", onefile=T,
    width=10, height=8)
ggplot() + 
  geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year <2010,], aes(means, vars)) +
  geom_point(data=stats[stats$year %in% 2010:2017,], aes(means, vars), colour="red") +
  geom_text_repel(data=stats[stats$year %in% 2010:2017,], aes(means, vars, label=year), colour="red", size=3) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)")+
  ggtitle("All years, all strata")

ggplot() + 
  geom_smooth(data=stats[stats$Strata_ID %in% 2:4,], aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25, lwd=1.5) +
  geom_smooth(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, colour=Strata_lab), method="lm", se=F, lwd=1.5) +
  geom_point(data=stats[stats$Strata_ID %in% 2:4 & stats$year <2009,], aes(means, vars), colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, colour=Strata_lab)) +
  geom_text_repel(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, label=year, colour=Strata_lab), size=3, point.padding = 0.25, box.padding = 0.5) +
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)")+
  ggtitle("2009-2017 years and north strata highlighted")

ggplot() + 
  geom_smooth(data=stats[stats$year %in% 2009:2017,], aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year %in% 2009:2017,], aes(means, vars)) +
  geom_text_repel(data=stats[stats$year %in% 2009:2017,], aes(means, vars, label=year), size=3) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)") +
  ggtitle("2009-2017 only, all strata")

ggplot() + 
  #geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats, aes(year, sqrt(vars)/means)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Year") +
  ylab("Relative Std. Error (sd/mean)") +
  ggtitle("All years, all strata")

ggplot() + 
  #geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year > 2008,], aes(year, sqrt(vars)/means)) +
  geom_line(data=stats[stats$year>2008,], aes(year, 10*(tow/totaltows)))+
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Tows in strata / Tows in survey"))+
  scale_x_continuous(breaks=seq(2009, 2017, 2))+
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Year") +
  ylab("Relative Std. Error (sd/mean)") +
  ggtitle("2009-2017 only, all strata")

dev.off()

## full recruits only
means <- aggregate(com ~ Strata_ID + year, surv.Rand$GBa, mean)
names(means)[3] <- "means"
vars <- aggregate(com ~ Strata_ID + year, surv.Rand$GBa, function(x) sd(x)^2)
names(vars)[3] <- "vars"
tows <- aggregate(com ~ Strata_ID + year, surv.Rand$GBa, length)
names(tows)[3] <- "tow"

stats <- plyr::join(means, vars, type="full")
stats <- plyr::join(stats, tows, type="full")

stats$Strata_lab <- as.factor(stats$Strata_ID)
levels(stats$Strata_lab) <- c("Low", "Medium North", "High North", "Very High North",
                              "Medium South", "High South", "Very High South")

str(stats)
stats$year <- as.numeric(stats$year)

stats <- plyr::join(stats, strata_info, type="left")
stat_sum <- plyr::ddply(.data=stats[stats$year > 2000,], .(year),
                        summarize,
                        totaltows = sum(tow),
                        totalarea = sum(area_km2))

stats <- join(stats, stat_sum, type="left")

require(ggrepel)
pdf(file = "Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Exploratory_figures/GBa/GBa stratification checks_FullRecruits.pdf", onefile=T,
    width=10, height=8)
ggplot() + 
  geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year <2010,], aes(means, vars)) +
  geom_point(data=stats[stats$year %in% 2010:2017,], aes(means, vars), colour="red") +
  geom_text_repel(data=stats[stats$year %in% 2010:2017,], aes(means, vars, label=year), colour="red", size=3) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)")+
  ggtitle("All years, all strata")

ggplot() + 
  geom_smooth(data=stats[stats$Strata_ID %in% 2:4,], aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25, lwd=1.5) +
  geom_smooth(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, colour=Strata_lab), method="lm", se=F, lwd=1.5) +
  geom_point(data=stats[stats$Strata_ID %in% 2:4 & stats$year <2009,], aes(means, vars), colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, colour=Strata_lab)) +
  geom_text_repel(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, label=year, colour=Strata_lab), size=3, point.padding = 0.25, box.padding = 0.5) +
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)")+
  ggtitle("2009-2017 years and north strata highlighted")

ggplot() + 
  geom_smooth(data=stats[stats$year %in% 2009:2017,], aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year %in% 2009:2017,], aes(means, vars)) +
  geom_text_repel(data=stats[stats$year %in% 2009:2017,], aes(means, vars, label=year), size=3) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)") +
  ggtitle("2009-2017 only, all strata")

ggplot() + 
  #geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats, aes(year, sqrt(vars)/means)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Year") +
  ylab("Relative Std. Error (sd/mean)") +
  ggtitle("All years, all strata")

ggplot() + 
  #geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year > 2008,], aes(year, sqrt(vars)/means)) +
  geom_line(data=stats[stats$year>2008,], aes(year, 10*(tow/totaltows)))+
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Tows in strata / Tows in survey"))+
  scale_x_continuous(breaks=seq(2009, 2017, 2))+
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Year") +
  ylab("Relative Std. Error (sd/mean)") +
  ggtitle("2009-2017 only, all strata")

dev.off()



## full recruits + recruits only
means <- aggregate(com + rec ~ Strata_ID + year, surv.Rand$GBa, mean)
names(means)[3] <- "means"
vars <- aggregate(com + rec ~ Strata_ID + year, surv.Rand$GBa, function(x) sd(x)^2)
names(vars)[3] <- "vars"
tows <- aggregate(com + rec ~ Strata_ID + year, surv.Rand$GBa, length)
names(tows)[3] <- "tow"

stats <- plyr::join(means, vars, type="full")
stats <- plyr::join(stats, tows, type="full")

stats$Strata_lab <- as.factor(stats$Strata_ID)
levels(stats$Strata_lab) <- c("Low", "Medium North", "High North", "Very High North",
                              "Medium South", "High South", "Very High South")

str(stats)
stats$year <- as.numeric(stats$year)

stats <- plyr::join(stats, strata_info, type="left")
stat_sum <- plyr::ddply(.data=stats[stats$year > 2000,], .(year),
                        summarize,
                        totaltows = sum(tow),
                        totalarea = sum(area_km2))

stats <- join(stats, stat_sum, type="left")

require(ggrepel)
pdf(file = "Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Exploratory_figures/GBa/GBa stratification checks_RecruitsandFullRecruits.pdf", onefile=T,
    width=10, height=8)
ggplot() + 
  geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year <2009,], aes(means, vars)) +
  geom_point(data=stats[stats$year %in% 2009:2017,], aes(means, vars), colour="red") +
  geom_text_repel(data=stats[stats$year %in% 2009:2017,], aes(means, vars, label=year), colour="red", size=3) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)")+
  ggtitle("All years, all strata")

ggplot() + 
  geom_smooth(data=stats[stats$Strata_ID %in% 2:4,], aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25, lwd=1.5) +
  geom_smooth(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, colour=Strata_lab), method="lm", se=F, lwd=1.5) +
  geom_point(data=stats[stats$Strata_ID %in% 2:4 & stats$year <2009,], aes(means, vars), colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, colour=Strata_lab)) +
  geom_text_repel(data=stats[stats$Strata_ID %in% 2:4 & stats$year %in% 2009:2017,], aes(means, vars, label=year, colour=Strata_lab), size=3, point.padding = 0.25, box.padding = 0.5) +
  theme_bw() + theme(panel.grid=element_blank()) +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)")+
  ggtitle("2009-2017 years and north strata highlighted")

ggplot() + 
  geom_smooth(data=stats[stats$year %in% 2009:2017,], aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year %in% 2009:2017,], aes(means, vars)) +
  geom_text_repel(data=stats[stats$year %in% 2009:2017,], aes(means, vars, label=year), size=3) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Mean number per tow") +
  ylab("Variance in number per tow (sd^2)") +
  ggtitle("2009-2017 only, all strata")

ggplot() + 
  #geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats, aes(year, sqrt(vars)/means)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Year") +
  ylab("Relative Std. Error (sd/mean)") +
  ggtitle("All years, all strata")

ggplot() + 
  #geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_point(data=stats[stats$year > 2008,], aes(year, sqrt(vars)/means)) +
  geom_line(data=stats[stats$year>2008,], aes(year, 10*(tow/totaltows)))+
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Tows in strata / Tows in survey"))+
  scale_x_continuous(breaks=seq(2009, 2017, 2))+
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Year") +
  ylab("Relative Std. Error (sd/mean)") +
  ggtitle("2009-2017 only, all strata")

dev.off()

## tows only
pdf(file = "Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Exploratory_figures/GBa/GBa stratification checks_NumTowAnalysis.pdf", onefile=T,
    width=10, height=8)
ggplot() + 
  #geom_smooth(data=stats, aes(means, vars), method="lm", se=F, colour="grey", alpha=0.25) +
  geom_line(data=stats, aes(year, tow))+
  geom_point(data=stats, aes(year, tow)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  facet_wrap(~Strata_lab, scales="free") +
  xlab("Year") +
  ylab("Number of tows") +
  ggtitle("All years, all strata")

ggplot() + 
  geom_line(data=stats[!stats$Strata_ID %in% 2:4,], aes(as.numeric(as.character(year)), tow, group=Strata_lab), alpha=0.2, show.legend = F) +
  geom_line(data=stats[stats$Strata_ID %in% 2:4,], aes(as.numeric(as.character(year)), tow, colour=Strata_lab)) +
  geom_point(data=stats[stats$Strata_ID %in% 2:4,], aes(as.numeric(as.character(year)), tow, colour=Strata_lab)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  scale_colour_discrete(name="Strata")+
  xlab("Year") +
  ylab("Number of tows") +
  ggtitle("All years, north strata highlighted")

ggplot() + 
  geom_line(data=stats[stats$year >2008 & stats$Strata_ID %in% 2:4,], aes(year, tow, colour=Strata_lab)) +
  geom_point(data=stats[stats$year >2008 & stats$Strata_ID %in% 2:4,], aes(year, tow, colour=Strata_lab)) +
  theme_bw() + theme(panel.grid=element_blank()) +
  scale_x_continuous(breaks=seq(2009, 2017, 1))+
  scale_colour_discrete(name="Strata")+
  xlab("Year") +
  ylab("Number of tows") +
  ggtitle("2009-2017 only, north strata only")

ggplot() + 
  geom_point(data=stats, aes(area_km2/totalarea, tow/totaltows, colour=Strata_lab)) +
  #geom_text_repel(data=stats[stats$year >2008 & stats$Strata_ID %in% 2:4,], aes(area_km2/totalarea, tow/totaltows, label=year), size=2, box.padding = 0.15, point.padding = 0.2, min.segment.length = 0.1) +
  geom_abline(data=stats, slope=1, intercept=0) +
  annotate(geom="text", x=0.3-0.01, y=0.3, hjust=1, label="1:1")+
  theme_bw() + theme(panel.grid=element_blank()) +
  # scale_x_continuous(breaks=seq(2009, 2017, 1))+
  scale_colour_discrete(name="Strata")+
  xlab("Strata area (km2) /\nTotal survey area (km2)") +
  ylab("Number of tows /\nTotal survey tows") +
  ggtitle("All years, north strata only") +
  ylim(0.10, 0.31)

ggplot() + 
  geom_point(data=stats, aes(area_km2/totalarea, tow/totaltows, colour=Strata_lab)) +
  geom_abline(data=stats, slope=1, intercept=0) +
  annotate(geom="text", x=0.3-0.01, y=0.3, hjust=1, label="1:1")+
  geom_text_repel(data=stats[stats$year >2008,], aes(area_km2/totalarea, tow/totaltows, label=year), size=3, box.padding = 0.15, point.padding = 0.2, min.segment.length = 0.1) +
  theme_bw() + theme(panel.grid=element_blank()) +
  # scale_x_continuous(breaks=seq(2009, 2017, 1))+
  scale_colour_discrete(name="Strata")+
  xlab("Strata area (km2) /\nTotal survey area (km2)") +
  ylab("Number of tows /\nTotal survey tows") +
  ylim(0.10, 0.31) +
  ggtitle("2009-2017 only, north strata only")

ggplot() + 
  geom_point(data=stats[stats$year > 2001 & stats$Strata_ID %in% 2:4,], aes(year, tow/totaltows)) + 
  geom_text(data=stats[stats$year > 2001 & stats$Strata_ID %in% 2:4,], aes(year+0.2, tow/totaltows, label=substr(year, 3,4)), size=3, hjust=0) + 
  geom_hline(data=stats[stats$year == 2017 & stats$Strata_ID %in% 2:4,], aes(yintercept=area_km2/totalarea)) +
  geom_text(data=stats[stats$year == 2017 & stats$Strata_ID %in% 2:4,], aes(y=area_km2/totalarea, x=2010), vjust=1, label="proportional area", size=3)+
  theme_bw() + theme(panel.grid=element_blank()) +
  # scale_x_continuous(breaks=seq(2009, 2017, 1))+
  xlab("Year") +
  ylab("Number of tows /\nTotal survey tows") +
  ggtitle("2002-2017, north strata only") +
  facet_wrap(~Strata_lab)

ggplot() + 
  geom_point(data=stats[stats$year > 2001 & stats$Strata_ID %in% 2:4,], aes(year, tow/totaltows)) + 
  geom_line(data=stats[stats$year > 2001 & stats$Strata_ID %in% 2:4,], aes(year, tow/totaltows)) + 
  geom_hline(data=stats[stats$year == 2017 & stats$Strata_ID %in% 2:4,], aes(yintercept=area_km2/totalarea)) +
  geom_text(data=stats[stats$year == 2017 & stats$Strata_ID %in% 2:4,], aes(y=area_km2/totalarea, x=2010), vjust=1, label="proportional area", size=3)+
  theme_bw() + theme(panel.grid=element_blank()) +
  # scale_x_continuous(breaks=seq(2009, 2017, 1))+
  xlab("Year") +
  ylab("Number of tows /\nTotal survey tows") +
  ggtitle("2002-2017, north strata only") +
  facet_wrap(~Strata_lab)

dev.off()

