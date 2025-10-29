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
