# PR to Rec analysis

load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2025/Survey_summary_output/Survey_all_results.Rdata")

pr_rec <- NULL
for(bnk in c("GBa", "GBb", "BBn", "Ger", "Sab")){
df <- data.frame(year_pr=survey.obj[[bnk]]$model.dat$year[1:(nrow(survey.obj[[bnk]]$model.dat)-1)], 
           pr=survey.obj[[bnk]]$model.dat$NPR[1:(nrow(survey.obj[[bnk]]$model.dat)-1)],
           pr65=survey.obj[[bnk]]$model.dat$`mean_65-85`[1:(nrow(survey.obj[[bnk]]$model.dat)-1)],
           rec=survey.obj[[bnk]]$model.dat$NPR[2:(nrow(survey.obj[[bnk]]$model.dat))],
           rec2=c(survey.obj[[bnk]]$model.dat$NPR[3:(nrow(survey.obj[[bnk]]$model.dat))],NA),
           rec3=c(survey.obj[[bnk]]$model.dat$NPR[4:(nrow(survey.obj[[bnk]]$model.dat))],NA, NA),
           rec85=c(survey.obj[[bnk]]$model.dat$`mean_85-95`[2:(nrow(survey.obj[[bnk]]$model.dat))]),
           fr95=c(survey.obj[[bnk]]$model.dat$`mean_95-120`[3:(nrow(survey.obj[[bnk]]$model.dat))],NA),
           bank=bnk)
if(bnk=="Ger") {
  df <- data.frame(year_pr=merged.survey.obj$year[1:(nrow(merged.survey.obj)-1)], 
                 pr=merged.survey.obj$NPR[1:(nrow(merged.survey.obj)-1)],
                 pr65=merged.survey.obj$`mean_65-85`[1:(nrow(merged.survey.obj)-1)],
                 rec=merged.survey.obj$NPR[2:(nrow(merged.survey.obj))],
                 rec2=c(merged.survey.obj$NPR[3:(nrow(merged.survey.obj))],NA),
                 rec3=c(merged.survey.obj$NPR[4:(nrow(merged.survey.obj))],NA, NA),
                 rec85=c(merged.survey.obj$`mean_85-95`[2:(nrow(merged.survey.obj))]),
                 fr95=c(merged.survey.obj$`mean_95-120`[3:(nrow(merged.survey.obj))],NA),
                 bank=bnk)
}
pr_rec <- rbind(pr_rec, df)
}
pr_rec$rec_combo <- pr_rec$rec+pr_rec$rec2
pr_rec$rec_combo3 <- pr_rec$rec+pr_rec$rec2+pr_rec$rec3
  
require(ggplot2)

ggplot() + geom_point(data=pr_rec, aes(pr, rec)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec, aes(pr, rec), method="lm")

mod <- lm(data=pr_rec, rec ~ pr)
summary(mod)

mod_0 <- lm(data=pr_rec, rec ~ pr+0)
summary(mod_0)

pr_rec$outlier <- NA
pr_rec$outlier[pr_rec$pr<1000 & pr_rec$rec>1000] <- "out"

mod_wo <- lm(data=pr_rec[is.na(pr_rec$outlier),], rec ~ pr)
summary(mod_wo)

mod_0_wo <- lm(data=pr_rec[is.na(pr_rec$outlier),], rec ~ pr+0)
summary(mod_0_wo)

ggplot() + geom_point(data=pr_rec, aes(pr, rec)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec, aes(pr, rec), method="lm") +
  geom_smooth(data=pr_rec[is.na(pr_rec$outlier),], aes(pr, rec), method="lm", colour="red")

pr_rec$boom <- NA
pr_rec$boom[pr_rec$pr>1000 | pr_rec$rec>1000] <- "boom"

mod_wo <- lm(data=pr_rec[is.na(pr_rec$boom),], rec ~ pr)
summary(mod_wo)

mod_0_wo <- lm(data=pr_rec[is.na(pr_rec$boom),], rec ~ pr+0)
summary(mod_0_wo)

ggplot() + geom_point(data=pr_rec, aes(pr, rec)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec, aes(pr, rec), method="lm") +
  geom_smooth(data=pr_rec[is.na(pr_rec$outlier),], aes(pr, rec), method="lm", colour="red")+
  geom_smooth(data=pr_rec[is.na(pr_rec$boom),], aes(pr, rec), method="lm", colour="forestgreen")

ggplot() + geom_point(data=pr_rec[is.na(pr_rec$boom),], aes(pr, rec)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec[is.na(pr_rec$boom),], aes(pr, rec), method="lm", colour="forestgreen")

#######
ggplot() + geom_point(data=pr_rec, aes(pr, rec_combo)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec, aes(pr, rec_combo), method="lm")

mod <- lm(data=pr_rec, rec_combo ~ pr)
summary(mod)

mod_0 <- lm(data=pr_rec, rec_combo ~ pr+0)
summary(mod_0)

########
ggplot() + geom_point(data=pr_rec, aes(pr, rec2)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec, aes(pr, rec2), method="lm")

mod <- lm(data=pr_rec, rec2 ~ pr)
summary(mod)

mod_0 <- lm(data=pr_rec, rec2 ~ pr+0)
summary(mod_0)

#########
ggplot() + geom_point(data=pr_rec, aes(pr, rec_combo3)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec, aes(pr, rec_combo3), method="lm")

mod <- lm(data=pr_rec, rec_combo3 ~ pr)
summary(mod)

mod_0 <- lm(data=pr_rec, rec_combo3 ~ pr+0)
summary(mod_0)

########
ggplot() + geom_point(data=pr_rec, aes(pr, rec3)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec, aes(pr, rec3), method="lm")

mod <- lm(data=pr_rec, rec3 ~ pr)
summary(mod)

mod_0 <- lm(data=pr_rec, rec3 ~ pr+0)
summary(mod_0)

#########
ggplot() + geom_point(data=pr_rec, aes(pr65, rec85)) + 
  theme_bw() + 
  geom_smooth(data=pr_rec, aes(pr65, rec85), method="lm") +
  xlab("65-85mm year t") + 
  ylab("85-95mm year t+1")

mod <- lm(data=pr_rec, rec85 ~ pr65)
summary(mod)

mod_0 <- lm(data=pr_rec, rec85 ~ pr65+0)
summary(mod_0)

pr_rec$prop <- pr_rec$rec85/pr_rec$pr65

ggplot() + geom_point(data=pr_rec,aes(year_pr, prop))
summary(pr_rec$prop)
hist(pr_rec$prop, breaks = seq(0,9,0.5))
# pr's fail 25% of the time
# good survival 50% of the time
# new pr's get added to the population 25% of the time
ggplot() + geom_boxplot(data=pr_rec, aes(prop), outliers = F) +
  xlab("proportion of 65-85 mm that survive to 85-95 mm in the next year") + 
  facet_wrap(~bank)

mean(surv.Live$Ger[surv.Live$Ger$year==2024,]$`bin_65-85`)
pr_rec[pr_rec$bank=="Ger",]

ggplot() + geom_boxplot(data=pr_rec, aes(y=prop, x=bank, group=bank), outliers = F) +
  xlab("proportion of 65-85 mm that survive to 85-95 mm in the next year")

ggplot() + geom_point(data=pr_rec, aes(prop, rec85)) + 
  facet_wrap(~bank)

ggplot() + geom_point(data=pr_rec, aes(pr65, rec85)) + 
  facet_wrap(~bank)


pr_rec$prop_fr <- pr_rec$fr95/pr_rec$rec85

ggplot() + geom_point(data=pr_rec,aes(year_pr, prop_fr))
summary(pr_rec$prop_fr)
hist(pr_rec$prop_fr, breaks = seq(0,9,0.5))
# pr's fail 25% of the time
# good survival 50% of the time
# new pr's get added to the population 25% of the time
ggplot() + geom_boxplot(data=pr_rec, aes(prop_fr), outliers = F) +
  xlab("proportion of 85-95 mm that survive to 95-120 mm in the next year")
