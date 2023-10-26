load("Y:/Offshore/Assessment/Data/survey_data/2023/Survey_summary_output/testing_results_spring2023.Rdata")
require(dplyr)
require(tidyverse)
require(PEDstrata)

# data prep
test <- surv.Rand$BBn
test <- pivot_longer(test, cols=starts_with("h"), values_to = "count") %>%
  select(year, date, tow, Strata_ID, name, count) %>%
  filter(year==2023)

unique(test$name)

test$name <- gsub(test$name, pattern="h", replacement="")
test$name <- as.numeric(test$name)
test$age[test$name<90] <- "pre"
test$age[test$name>95] <- "com"
test$age[test$name %in% c(90, 95)] <- "rec"

table(test$age, test$name)

# make a strata table with number of tows
strata <- data.frame(Strata_ID=c(201:205), ntows=c(43,18,18,12,9))
test <- left_join(test, strata)
test <- test %>% group_by(tow, Strata_ID, age) %>%
  summarize(count=sum(count))

# make a strata table for PEDstrata (this mirrors what is used in current survey summary)
# Why do we divide towable area by 800???
HSIstrata.obj <- data.frame(Strata_ID=survey.info$Strata_ID[survey.info$label=="BBn" & survey.info$startyear==2021],
                            NH=survey.info$towable_area[survey.info$label=="BBn" & survey.info$startyear==2021])
# from survey summary, but using the test data from above
N <- PEDstrata(test[test$age=="com",], HSIstrata.obj,'Strata_ID',test$count[test$age=="com"])
NR <- PEDstrata(test[test$age=="rec",], HSIstrata.obj,'Strata_ID',test$count[test$age=="rec"])
NPR <- PEDstrata(test[test$age=="pre",], HSIstrata.obj,'Strata_ID',test$count[test$age=="pre"])
# Wh for all three ages are the same

N.tmp <- summary(N,effic=T)

# Take Wh from PEDstrata result, you're going to multiply things by Wh later
Wh <- data.frame(Wh = N$Wh, Strata_ID=N$Strata)

means <- test %>%
  group_by(Strata_ID, age) %>%
  summarize(mean=mean(count)) 
means <- left_join(means, Wh)
means$prorate <- means$mean * means$Wh

# stratified estimates!
means %>% group_by(age) %>%
  summarize(sum(prorate))
1       201 com    45.0 0.431    19.4 
2       202 com    41.5 0.181     7.53
3       203 com    93.9 0.180    16.9 
4       204 com    29.3 0.120     3.52
5       205 com    94.3 0.0879    8.29

survey.info[survey.info$label=="BBn" & survey.info$startyear==2021,]

# 
# 
# 
# 
# yh <- as.vector(sapply(object$yhi, mean))
# yst <- sum(object$Wh * yh, na.rm = TRUE)
# 
# 
# 
# test <- test %>% group_by(tow,Strata_ID, age) %>%
#                   summarize(total=mean(count))
# 
# 
# 
# test$prorate <- test$total*(test$ntows/100)
# 
# test %>% group_by(age) %>%
#   summarize(total=sum(prorate))
# 
# 
# survey.obj$BBn$model.dat[survey.obj$BBn$model.dat$year==2023,]
# 
# 
# 
# sum(survey.obj$BBn$Strata.obj$N[[32]]$Wh * yh, na.rm = TRUE)
# 
# all <- surv.Rand$BBn
# 
# all <- all %>% group_by(year, Strata_ID) %>%
#   summarize(ntows=n()) 
# 
# ggplot() + geom_point(data=all, aes(year, ntows)) + facet_wrap(~Strata_ID) +
#   ggtitle("BBn number of tows per strata over time")
