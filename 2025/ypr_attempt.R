# biomass per recruit/yield per recruit analysis

# biomass per recruit just uses survey data
load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2025/Survey_summary_output/Survey_all_results.Rdata")

model.dat <- survey.obj$GBa$model.dat


model.dat$ypr_0y <- c(model.dat$I[1:length(model.dat$I)]/model.dat$NR[1:(length(model.dat$NR))])

ggplot() + geom_point(data=model.dat, aes(year, ypr_0y)) +
  geom_smooth(data=model.dat, aes(year, ypr_0y), method = "lm")



model.dat$ypr_1y <- c(NA, model.dat$I[2:length(model.dat$I)]/model.dat$NR[1:(length(model.dat$NR)-1)])

ggplot() + geom_point(data=model.dat, aes(year, ypr_1y)) +
  geom_smooth(data=model.dat, aes(year, ypr_1y), method = "lm")



model.dat$ypr_2y <- c(NA, NA, model.dat$I[3:length(model.dat$I)]/model.dat$NR[1:(length(model.dat$NR)-2)])

ggplot() + geom_point(data=model.dat, aes(year, ypr_2y)) +
  geom_smooth(data=model.dat, aes(year, ypr_2y), method = "lm")


#YPR needs catch data...
load("C:/Users/keyserf/Documents/temp_data/Data/Fishery_data/Summary/2025/OSAC_tidy_logs.RData")
catch <- fish.dat %>% 
  dplyr::group_by(year, bank) %>%
  dplyr::summarize(kg=sum(pro.repwt, na.rm=T)) %>%
  filter(bank=="GBa")

ypr <- left_join(catch, model.dat[, c("year", "I", "N", "IR", "NR")])
ypr$ypr <- ypr$kg/ypr$NR
ypr$ypr_1y <- c(NA, ypr$kg[2:nrow(ypr)]/ypr$NR[1:(nrow(ypr)-1)])

ggplot() + geom_point(data=ypr, aes(year, ypr)) +
  geom_smooth(data=ypr, aes(year, ypr), method = "lm")

ggplot() + geom_point(data=ypr, aes(year, ypr_1y)) +
  geom_smooth(data=ypr, aes(year, ypr_1y), method = "lm")


# another extra thing
shf.by.year <- as.data.frame(survey.obj$GBa$shf.dat$n.yst)

names(shf.by.year) <- paste("H",seq(0,195,by=5),sep="_")
which(names(shf.by.year) == "H_95")
lt.rec <- shf.by.year[,1:(which(names(shf.by.year) == "H_85")-1)]
frs.rec <- shf.by.year[,which(names(shf.by.year) == "H_85"):40]
df <- data.frame(tot.fr.rec = rowSums(frs.rec), tot.lt.rec = rowSums(lt.rec), year=survey.obj$GBa$model.dat$year)
df$prop.small <- df$tot.lt.rec/(df$tot.lt.rec+df$tot.fr.rec)

png("Y:/Offshore/Assessment/2025/Presentations/Survey_summary/Exploratory_figures/proportion_small_GBa.png", height=5, width=6, res=400, units="in")
ggplot() + geom_point(data=df, aes(year, prop.small)) +
  geom_line(data=df, aes(year, prop.small)) + theme_bw() +
  ylab("Proportion of pre-recruits\n(survey catch <85mm / all survey catch)") +
  xlab("Year")
dev.off()
# catch rate thing

# survey lows
survey.obj$GBa$bankpertow %>% arrange(N) %>% head() # 1998
survey.obj$GBa$bankpertow %>% arrange(NR) %>% head() # 1994
survey.obj$GBa$bankpertow %>% arrange(NPR) %>% head() # lowest ever
survey.obj$GBa$bankpertow %>% arrange(I) %>% head() # 1998
survey.obj$GBa$bankpertow %>% arrange(IR) %>% head() # 1994
survey.obj$GBa$bankpertow %>% arrange(IPR) %>% head() # 1993
surv.Clap.Rand$GBa %>% dplyr::group_by(year) %>% dplyr::summarize(meanC=mean(clap.propPre)) %>% dplyr::arrange(-meanC) %>% head()
#2024 was highest since 2002; 2025 is slightly better
surv.Clap.Rand$GBa %>% dplyr::group_by(year) %>% dplyr::summarize(meanC=mean(clap.propRec)) %>% dplyr::arrange(-meanC) %>% head()
#highest since 2002
surv.Clap.Rand$GBa %>% dplyr::group_by(year) %>% dplyr::summarize(meanC=mean(clap.propCom)) %>% dplyr::arrange(-meanC) %>% head()
#highest since 1988

survey.obj$GBb$bankpertow %>% arrange(N) %>% head() # lowest ever
survey.obj$GBb$bankpertow %>% arrange(NR) %>% head() # lowest ever
survey.obj$GBb$bankpertow %>% arrange(NPR) %>% head() # lowest ever
survey.obj$GBb$bankpertow %>% arrange(I) %>% head() # lowest ever
survey.obj$GBb$bankpertow %>% arrange(IR) %>% head() # lowest ever
survey.obj$GBb$bankpertow %>% arrange(IPR) %>% head() # lowest ever
surv.Clap.Rand$GBb %>% dplyr::group_by(year) %>% dplyr::summarize(meanC=mean(clap.propPre)) %>% dplyr::arrange(-meanC) %>% head()
#highest ever
surv.Clap.Rand$GBb %>% dplyr::group_by(year) %>% dplyr::summarize(meanC=mean(clap.propRec)) %>% dplyr::arrange(-meanC) %>% head()
#highest ever
surv.Clap.Rand$GBb %>% dplyr::group_by(year) %>% dplyr::summarize(meanC=mean(clap.propCom)) %>% dplyr::arrange(-meanC) %>% head()
#highest ever

load("C:/Users/keyserf/Documents/temp_data/Data/Fishery_data/Summary/2025/OSAC_summary.RData")

cpue.dat$GBa[which(cpue.dat$GBa$cpue<cpue.dat$GBa$cpue[cpue.dat$GBa$year==2025]),] %>% arrange(year) # lowest since 2005
cpue.dat$GBb[which(cpue.dat$GBb$cpue<cpue.dat$GBb$cpue[cpue.dat$GBb$year==2025]),] %>% arrange(year) # lowest since 2010

cpue.dat2 <- rbind(cbind(cpue.dat$GBa, bank="GBa"), cbind(cpue.dat$GBb, bank="GBb"))

cpue.dat2 <- cpue.dat2 %>%
  dplyr::select(year, bank, contains("cpue"))%>%
  pivot_longer(cols = c(cpue, wf.cpue, ft.cpue)) %>% 
  rename(fleet="name", cpue="value") %>%
  pivot_longer(cols = contains("var")) %>%
  rename(fleet.var="name", var="value") %>%
  pivot_longer(cols = contains("se")) %>%
  rename(fleet.se="name", se="value")

cpue.dat2$fleet <- gsub(x = cpue.dat2$fleet, pattern="wf.cpue", "wf")
cpue.dat2$fleet <- gsub(x = cpue.dat2$fleet, pattern="ft.cpue", "ft")
cpue.dat2$fleet <- gsub(x = cpue.dat2$fleet, pattern="cpue", "both")
cpue.dat2$fleet.var <- gsub(x = cpue.dat2$fleet.var, pattern="wf.cpue", "wf")
cpue.dat2$fleet.var <- gsub(x = cpue.dat2$fleet.var, pattern="ft.cpue", "ft")
cpue.dat2$fleet.var <- gsub(x = cpue.dat2$fleet.var, pattern="cpue", "both")
cpue.dat2$fleet.var <- gsub(x = cpue.dat2$fleet.var, pattern=".var", "")
cpue.dat2$fleet.se <- gsub(x = cpue.dat2$fleet.se, pattern="wf.cpue", "wf")
cpue.dat2$fleet.se <- gsub(x = cpue.dat2$fleet.se, pattern="ft.cpue", "ft")
cpue.dat2$fleet.se <- gsub(x = cpue.dat2$fleet.se, pattern="cpue", "both")
cpue.dat2$fleet.se <- gsub(x = cpue.dat2$fleet.se, pattern=".se", "")

cpue.dat2 <- cpue.dat2[cpue.dat2$fleet==cpue.dat2$fleet.var & cpue.dat2$fleet==cpue.dat2$fleet.se,] %>%
  dplyr::select(-fleet.var, -fleet.se)

survey.dat <- rbind(cbind(survey.obj$GBa$model.dat, bank="GBa"), cbind(survey.obj$GBb$model.dat, bank="GBb"))
biomass_cpue <- left_join(survey.dat, cpue.dat2, by=c("year", "bank"))

biomass_cpue$fleet <- gsub(x = biomass_cpue$fleet, pattern="wf", "wetfish")
biomass_cpue$fleet <- gsub(x = biomass_cpue$fleet, pattern="ft", "freezer trawler")

require(ggrepel)
png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_cpue_GBa_1.png", height=5, width=8, res=400, units="in")
ggplot() + 
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBa",], aes(cpue, I), shape=16, colour="orange", size=10) +
  geom_text(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, I, label=year)) + 
  # geom_point(data=biomass_cpue, aes(cpue, I)) +
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), colour="orange", size=5) +
  # geom_text_repel(data=biomass_cpue, aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) + 
  #geom_path(data=biomass_cpue, aes(cpue, I, label=year)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey biomass estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_cpue_GBa_2.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  geom_point(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, I)) +
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBa",], aes(cpue, I), colour="orange", size=5) +
  geom_text_repel(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) +
  #geom_path(data=biomass_cpue, aes(cpue, I, label=year)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey biomass estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_cpue_GBa_3.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  geom_point(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, I)) +
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBa",], aes(cpue, I), colour="orange", size=5) +
  #geom_text_repel(data=biomass_cpue, aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) +
  geom_path(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, I)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey biomass estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_cpue_GBb_1.png", height=5, width=8, res=400, units="in")
ggplot() + 
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBb",], aes(cpue, I), shape=16, colour="orange", size=10) +
  geom_text(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, I, label=year)) + 
  # geom_point(data=biomass_cpue, aes(cpue, I)) +
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), colour="orange", size=5) +
  # geom_text_repel(data=biomass_cpue, aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) + 
  #geom_path(data=biomass_cpue, aes(cpue, I, label=year)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey biomass estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_cpue_GBb_2.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  geom_point(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, I)) +
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBb",], aes(cpue, I), colour="orange", size=5) +
  geom_text_repel(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) +
  #geom_path(data=biomass_cpue, aes(cpue, I, label=year)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey biomass estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_cpue_GBb_3.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  geom_point(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, I)) +
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBb",], aes(cpue, I), colour="orange", size=5) +
  #geom_text_repel(data=biomass_cpue, aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) +
  geom_path(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, I)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey biomass estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_N_cpue_GBa_1.png", height=5, width=8, res=400, units="in")
ggplot() + 
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBa",], aes(cpue, N), shape=16, colour="orange", size=10) +
  geom_text(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, N, label=year)) + 
  # geom_point(data=biomass_cpue, aes(cpue, I)) +
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), colour="orange", size=5) +
  # geom_text_repel(data=biomass_cpue, aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) + 
  #geom_path(data=biomass_cpue, aes(cpue, I, label=year)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey abundance estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_N_cpue_GBa_2.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  geom_point(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, N)) +
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBa",], aes(cpue, N), colour="orange", size=5) +
  geom_text_repel(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, N, label=year), min.segment.length = 0, max.overlaps=120) +
  #geom_path(data=biomass_cpue, aes(cpue, I, label=year)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey abundance estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_N_cpue_GBa_3.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  geom_point(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, N)) +
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBa",], aes(cpue, N), colour="orange", size=5) +
  #geom_text_repel(data=biomass_cpue, aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) +
  geom_path(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(cpue, N)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey abundance estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_N_cpue_GBb_1.png", height=5, width=8, res=400, units="in")
ggplot() + 
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBb",], aes(cpue, N), shape=16, colour="orange", size=10) +
  geom_text(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, N, label=year)) + 
  # geom_point(data=biomass_cpue, aes(cpue, I)) +
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), colour="orange", size=5) +
  # geom_text_repel(data=biomass_cpue, aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) + 
  #geom_path(data=biomass_cpue, aes(cpue, I, label=year)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey abundance estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_N_cpue_GBb_2.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  geom_point(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, N)) +
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBb",], aes(cpue, N), colour="orange", size=5) +
  geom_text_repel(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, N, label=year), min.segment.length = 0, max.overlaps=120) +
  #geom_path(data=biomass_cpue, aes(cpue, I, label=year)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey abundance estimate")
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/survey_N_cpue_GBb_3.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  geom_point(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, N)) +
  geom_point(data=biomass_cpue[biomass_cpue$year==2025 & biomass_cpue$bank=="GBb",], aes(cpue, N), colour="orange", size=5) +
  #geom_text_repel(data=biomass_cpue, aes(cpue, I, label=year), min.segment.length = 0, max.overlaps=120) +
  geom_path(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(cpue, N)) + 
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Catch per unit effort (kg/hm)") +
  ylab("Survey abundance estimate")
dev.off()

#biomass_cpue
png("Y:/Offshore/Assessment/2025/Presentations/OSAC/two_axes_GBa.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  #geom_point(data=biomass_cpue, aes(year, I/500), colour="red") +
  geom_line(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(year, I/250), colour="red") +
  #geom_point(data=biomass_cpue, aes(year, cpue)) +
  geom_line(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(year, cpue)) +
  scale_y_continuous(sec.axis = sec_axis(~ .*250, name="Survey biomass estimate (t)"))+
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Year") +
  ylab("Catch per unit effort (kg/hm)") +
  theme(axis.title.y.right = element_text(colour="red"), axis.text.y.right = element_text(colour="red"))
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/two_axes_GBb.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  #geom_point(data=biomass_cpue, aes(year, I/500), colour="red") +
  geom_line(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(year, I/50), colour="red") +
  #geom_point(data=biomass_cpue, aes(year, cpue)) +
  geom_line(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(year, cpue)) +
  scale_y_continuous(sec.axis = sec_axis(~ .*50, name="Survey biomass estimate (t)"))+
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Year") +
  ylab("Catch per unit effort (kg/hm)") +
  theme(axis.title.y.right = element_text(colour="red"), axis.text.y.right = element_text(colour="red"))
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/two_axes_GBa_N.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  #geom_point(data=biomass_cpue, aes(year, I/500), colour="red") +
  geom_line(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(year, N/10), colour="red") +
  #geom_point(data=biomass_cpue, aes(year, cpue)) +
  geom_line(data=biomass_cpue[biomass_cpue$bank=="GBa",], aes(year, cpue)) +
  scale_y_continuous(sec.axis = sec_axis(~ .*10, name="Survey abundance estimate (t)"))+
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Year") +
  ylab("Catch per unit effort (kg/hm)") +
  theme(axis.title.y.right = element_text(colour="red"), axis.text.y.right = element_text(colour="red"))
dev.off()

png("Y:/Offshore/Assessment/2025/Presentations/OSAC/two_axes_GBb_N.png", height=5, width=8, res=400, units="in")
ggplot() + 
  # geom_point(data=biomass_cpue[biomass_cpue$year==2025,], aes(cpue, I), shape=16, colour="orange", size=10) +
  # geom_text(data=biomass_cpue, aes(cpue, I, label=year)) + 
  #geom_point(data=biomass_cpue, aes(year, I/500), colour="red") +
  geom_line(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(year, N/5), colour="red") +
  #geom_point(data=biomass_cpue, aes(year, cpue)) +
  geom_line(data=biomass_cpue[biomass_cpue$bank=="GBb",], aes(year, cpue)) +
  scale_y_continuous(sec.axis = sec_axis(~ .*5, name="Survey abundance estimate (t)"))+
  facet_wrap(~fleet)+
  theme_bw() + 
  xlab("Year") +
  ylab("Catch per unit effort (kg/hm)") +
  theme(axis.title.y.right = element_text(colour="red"), axis.text.y.right = element_text(colour="red"))
dev.off()

# decline in catch rates on GBa and GBb from 2023-2024 and 2024-2025

biomass_cpue <- arrange(biomass_cpue, bank, fleet, year)
biomass_cpue$diff <- NA
for(i in 1:nrow(biomass_cpue)){
  if(i>1){
    if(biomass_cpue$bank[i]==biomass_cpue$bank[i-1]){
      if(biomass_cpue$fleet[i]==biomass_cpue$fleet[i-1]){
        biomass_cpue$diff[i] <- biomass_cpue$cpue[i]/biomass_cpue$cpue[i-1]
      }
    }
  } 
}

biomass_cpue <- biomass_cpue %>%
  group_by(bank, fleet) %>%
  summarize(ltm_cpue=median(cpue,na.rm=T)) %>%
  right_join(biomass_cpue) %>%
  mutate(diff=1-diff)

biomass_cpue[biomass_cpue$year %in% 2024:2025, c("year", "bank", "fleet", "diff")] %>%
  pivot_wider(names_from="fleet", values_from = "diff") 

biomass_cpue <- arrange(biomass_cpue, bank, fleet, year)
biomass_cpue$Idiff <- NA
biomass_cpue$Ndiff <- NA
for(i in 1:nrow(biomass_cpue)){
  if(i>1){
    if(biomass_cpue$bank[i]==biomass_cpue$bank[i-1]){
      if(biomass_cpue$fleet[i]==biomass_cpue$fleet[i-1]){
        biomass_cpue$Idiff[i] <- biomass_cpue$I[i]/biomass_cpue$I[i-1]
        biomass_cpue$Ndiff[i] <- biomass_cpue$N[i]/biomass_cpue$N[i-1]
      }
    }
  } 
}
biomass_cpue[biomass_cpue$year %in% 2024:2025, c("year", "bank", "fleet", "Idiff")] %>%
  pivot_wider(names_from="fleet", values_from = "Idiff") 
biomass_cpue[biomass_cpue$year %in% 2024:2025, c("year", "bank", "fleet", "Ndiff")] %>%
  pivot_wider(names_from="fleet", values_from = "Ndiff") 
