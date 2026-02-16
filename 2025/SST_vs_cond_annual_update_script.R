# This script grabs the latest GB SST data from the datashop, fits the Condition-SST relationship to this data up to 2018
# and then plots the most recent data onto the figure.

#####################################################################################################

library(tidyverse)
library(data.table)
library(MASS)
library(readxl)
library(bbmle)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(mgcv)
library(lubridate)
library(pander)
library(scales)
library(R.matlab)
library(cowplot)
library(visreg)

#####################################################################################################
#Import Files from shared drive
#####################################################################################################

GB_scallops_1985_2013_sst <- "Y:/Offshore/Assessment/Data/Environmental/Temperature/GBscallopWeekly.IML.csv"
# The 2023 version
#GB_scallops_new_sst_file <- "D:/Github/Paper_2_Cond_Env/Data/GBscallopWeekly_Oct_2023.dat"
# This file will need to be updated annually
GB_scallops_new_sst_file <- "Y:/Offshore/Assessment/Data/Environmental/Temperature/GBscallopWeekly_Apr_2025.csv"

#setwd("Y:/Projects/Condition_Environment/Data/")
setwd("Y:/Projects/Condition_Environment/")
# directory you have the survey results stored in...
direct <- "Y:/Offshore scallop/Assessment/Data/Survey_data/2018/Survey_summary_output/"
#direct <- "E:/R/Data/Survey_data/2018/Survey_summary_output/"

# Looking a relationship between Condition and SST and phytoplankton


# Here we load in the data and get everything ready for further analyses.
#----------------------------------------------------------------------------------

# Scallop condition data, this file will need to be updated annually.
dat.cf <- read.csv("Y:/Offshore/Assessment/Data/Condition/Full_may_aug_condition_ts.csv")

#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

sst_1985 <- read.table(GB_scallops_1985_2013_sst, sep = ";" , header = TRUE, na.strings ="")
names(sst_1985) <- c("year",'week','start_date','end_date','sst',"sd_sst",'pgrid',"pixels")
sst_1985$sst[sst_1985$sst == -99] <- NA
sst_1985$sd_sst [sst_1985$sd_sst  == -99] <- NA
sst_1985$weight <- 100*(sst_1985$pixels/max(sst_1985$pixels))
sst_1985$start_date <- as_date(sst_1985$start_date)
sst_1985$end_date <- as_date(sst_1985$end_date)
sst_1985$mid_date <- sst_1985$end_date-3
sst_1985$month <- tolower(lubridate::month(sst_1985$mid_date,label=T,abbr=T))
sst_1985$monthID <- paste0(sst_1985$year,sst_1985$month)

sst_new <- read.table(GB_scallops_new_sst_file, sep = ";" , header = TRUE, na.strings ="")
names(sst_new) <- c("year",'week','start_date','end_date','sst',"sd_sst",'pgrid',"pixels")
sst_new$sst[sst_new$sst == -99] <- NA
sst_new$sd_sst [sst_new$sd_sst  == -99] <- NA
sst_new$weight <- 100*(sst_new$pixels/max(sst_new$pixels))
sst_new$start_date <- as_date(sst_new$start_date, format = '%Y-%m-%d')
sst_new$end_date <- as_date(sst_new$end_date, format = '%Y-%m-%d')
sst_new$mid_date <- sst_new$end_date-3
sst_new$month <- tolower(lubridate::month(sst_new$mid_date,label=T,abbr=T))
sst_new$monthID <- paste0(sst_new$year,sst_new$month)

# Can see these data are very similar, overlap from 2000 to 2013, it is suggested to use the 1985 (IML) dataset for years with overlap
# but it likely doesn't matter.
ggplot(sst_1985,aes(x=start_date,y=sst)) + geom_point() + geom_line() +
                                           geom_point(data=sst_new,aes(x=start_date,y=sst),color='blue') +
                                           geom_line(data=sst_new,aes(x=start_date,y=sst),color='blue')      

#####################################################################################################
#Everything Good?
#####################################################################################################

#Calculate all the weights for each variable for each observation

#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

sst_1985_summary <- sst_1985 %>% group_by(monthID) %>% 
                                 mutate(w_mean_new = weighted.mean(sst, weight)) %>%
                                 mutate(w_sd_new = weighted.mean(sd_sst, weight))


sst_new_summary <- sst_new %>% group_by(monthID) %>% 
                               mutate(w_mean_new = weighted.mean(sst, weight)) %>%
                               mutate(w_sd_new = weighted.mean(sd_sst, weight))


#####################################################################################################
#Remove Duplicates and get rid of non-essential columns for both data frames

#####################################################################################################
#GB_SCALLOPS_SST
#####################################################################################################

no_duplicates_GBS <- sst_1985_summary[!duplicated(sst_1985_summary$monthID),]
keeps <-c("w_mean_new", "w_sd_new", "monthID")
All_weights_1985 <- no_duplicates_GBS[keeps]
names(All_weights_1985) <- c("IML_wgt_mn","IML_wgt_sd","monthID")

no_duplicates_GBS <- sst_new_summary[!duplicated(sst_new_summary$monthID),]
keeps <- c("w_mean_new", "w_sd_new", "monthID")
All_weights_new <- no_duplicates_GBS[keeps]

# Now to compare the two time series, they are very very similar.

sst_comparison <- left_join(All_weights_new,All_weights_1985,by = "monthID")

corel <- cor.test(sst_comparison$w_mean_new , sst_comparison$IML_wgt_mn)
corel

lm.res <- lm(IML_wgt_mn~w_mean_new-1,data=sst_comparison)
summary(lm.res)

windows(11,11)
ggplot(data= sst_comparison) + geom_point(aes(w_mean_new,y=IML_wgt_mn)) + 
                               geom_abline(slope=1,intercept=0) + xlab("New SST Data") + ylab("IML SST Data")


# PICK IT UP FROM HERE, I"LL NEED TO COMBINE THE TWO DATASEETS AT SOME POINT HERE.... 
# Suggestion from the SST folks is to use IML as much as possible, though it seems
# it probably doesn't matter...


#####################################################################################################
#Convert Month ID to date class (only works if you add the day).
#order by date
#####################################################################################################

All_weights_1985$date <- as.Date(paste(All_weights_1985$monthID,"1"), "%Y%b%d")
final.dat.1985 <- All_weights_1985[order(All_weights_1985$monthID),]
final.dat.1985$year <- year(final.dat.1985$date)
final.dat.1985$month <- month(final.dat.1985$date)

# New data...
All_weights_new$date <- as.Date(paste(All_weights_new$monthID,"1"), "%Y%b%d")
final.dat.new <- All_weights_new[order(All_weights_new$monthID),]
final.dat.new$year <- year(final.dat.new$date)
final.dat.new$month <- month(final.dat.new$date)


#####################################################################################################
#Everything Good?
#####################################################################################################

head(final.dat.1985)
final.dat.1985$date
write.csv(final.dat.1985, file = "D:/Github/Paper_2_Cond_Env/Data/dfo_sst_ts_1985_2013.csv")
write.csv(final.dat.new, file = "D:/Github/Paper_2_Cond_Env/Data/dfo_sst_ts_2000_2024.csv")
# For SST the paper says we use Jan to March from last year and Jan-Feb from this year.
# But my someday paper says Jan to April from last year and Jan to March from this year
# Seems to work better with the original SST paper months and using the median not the means so going for that :-)
SST.last.1985 <-  aggregate(IML_wgt_mn ~ year, data=final.dat.1985[final.dat.1985$month %in% 1:3,], FUN = sum)
names(SST.last.1985) <- c("year","SST.last")

SST.last.new <- aggregate(w_mean_new ~ year, data=final.dat.new[final.dat.new$month %in% 1:3,], FUN = sum)
names(SST.last.new) <- c("year","SST.last")

dat.1985 <- aggregate(IML_wgt_mn ~ year, data=final.dat.1985[final.dat.1985$month %in% 1:2,], FUN = sum)
names(dat.1985) <-  c("year","SST.cur")
dat.1985$SST.last <- c(NA,dat.1985[-nrow(dat.1985),names(dat.1985) != "year"])

# The new data...
dat.new <- aggregate(w_mean_new ~ year, data=final.dat.new[final.dat.new$month %in% 1:2,], FUN = sum)
names(dat.new) <-  c("year","SST.cur")
dat.new$SST.last <- c(NA,SST.last.new[-nrow(SST.last.new),names(SST.last.new) != "year"])

# Make this SST.sum piece which makes our model simple below
dat.new$SST.sum <- dat.new$SST.cur + dat.new$SST.last
dat.1985$SST.sum <- dat.1985$SST.cur + dat.1985$SST.last
#dat$SST.sum.median <-  dat$SST.last.median


# Merge in the condition data
dat.1985 <- left_join(dat.1985,dat.cf,by='year')
dat.new <- left_join(dat.new,dat.cf,by='year')
#write.csv(dat, file = "D:/Github/Paper_2_Cond_Env/Data/dfo_may_aug_sst_and_condition_time_series.csv")
# And make a 'data all' object that goes from 1985 to 2024, note that 2014 is screwy since I'm swtiching SST time series
# there, watch for an outlier with that year, if nothing fine to leave it...
dat.combo <- rbind(dat.1985,dat.new[dat.new$year>2013,])

# Plot some things...
ggplot(dat.combo,aes(x=SST.sum,y=aug,label=substr(year,3,4))) + geom_text()+ ylab("Aug Condition")
ggplot(dat.combo,aes(x=SST.last,y=aug,label=substr(year,3,4))) + geom_text()+ ylab("Aug Condition")
ggplot(dat.combo,aes(x=SST.cur,y=aug,label=substr(year,3,4))) + geom_text()+ ylab("Aug Condition")
# Same plot for May
ggplot(dat.combo,aes(x=SST.sum,y=may,label=substr(year,3,4))) + geom_text() + ylab("May Condition")
ggplot(dat.combo,aes(x=SST.last,y=may,label=substr(year,3,4))) + geom_text() + ylab("May Condition")
ggplot(dat.combo,aes(x=SST.cur,y=may,label=substr(year,3,4))) + geom_text() + ylab("May Condition")

# Make some models... using the data back to 1985 is very helpful! Not shown here anymore
# but doing this analysis on a subset of the data results in rather 
# poor fits and results that differ from what we saw before
# This gives me some more belief in the May condition being correlated to the spring SST generally.

# So if we go to 2024... only SST current matters, but SST last isn't nothing....
aug.mod.new <- lm(aug~SST.last+SST.cur,data=dat.combo %>% dplyr::filter(year < 2025))
summary(aug.mod.new)
# If we go to 2022...only SST last year matters... that's fun eh...
aug.mod.2022 <- lm(aug~SST.last+SST.cur,data=dat.combo %>% dplyr::filter(year < 2023))
summary(aug.mod.2022)
# If we look at May both pop out as would be expected from the above figure
# NOTE That the SST.last + SST.cur model has a bit higher R^2, but this 
# model is more in line with the figure being plotted below, so using this (the estimate from this model is a bit lower)
may.mod.new <- lm(may~SST.sum,data=dat.combo %>% dplyr::filter(year < 2025))
summary(may.mod.new)
# If we go to 2022...SST current is marignal when dropping 23/24, R2 is marginally better...
may.mod.2022 <- lm(may~SST.last+SST.cur,data=dat.combo %>% dplyr::filter(year < 2023))
summary(may.mod.2022)


# Now let's use the 2022 model to try and predict condition in 2023 and 2024
# The 2022 prediction... 18.6 measured was around 16.9, so not great
predict(may.mod.2022,newdata = data.frame(SST.last = 19.93,SST.cur =  14.76))
# The 2023 prediction... 19.3, a bit low as it  was around 20.8
predict(may.mod.2022,newdata = data.frame(SST.last = 20.92,SST.cur =  15.80))
# The 2024 prediction... 18.2 way too too high as it was 13.9
predict(may.mod.2022,newdata = data.frame(SST.last = 22.29,SST.cur =  11.39))
# What if we use the full model data to predict... 18.4 measured was around 16.9, so not great
predict(may.mod.new,newdata = data.frame(SST.last = 19.93,SST.cur =  14.76))
# The 2023 prediction... 19.3, a bit low as it  was around 20.8
predict(may.mod.new,newdata = data.frame(SST.last = 20.92,SST.cur =  15.80))
# The 2024 prediction... 17.1 way too too high as it was 13.9, tho better than the model to 2022, so that 2024 data point is pulling this one down.
predict(may.mod.new,newdata = data.frame(SST.last = 22.29,SST.cur =  11.39))
# The 2025 prediction... 17.1 way too too high as it was 13.9, tho better than the model to 2022, so that 2024 data point is pulling this one down.
may.2025.pred <- predict(may.mod.new,newdata = data.frame(SST.sum = 25.99))



# Now plot this shit...
p <- ggplot(dat.combo,aes(y=may,x=SST.sum)) + 
                                          geom_text(aes(label=substr(year,3,4)),color='red',size=4) + geom_smooth(method='lm',color='red',linetype='dashed') + 
                                          geom_text(data=dat.new %>% dplyr::filter(year >= 2023),aes(y=may,x=SST.sum,label = substr(year,3,4)),color='blue',size = 4) +
                                          #geom_text(aes(y=may.2025.pred,x=25.99),label = 25,color='black') +
                                          geom_vline(xintercept = dat.new$SST.sum[dat.new$year == 2026],size=1.5,alpha=0.2) +
                                          theme_bw(base_size = 18)+ xlab("Sea Surface Temperature Index (°C)") + ylab("May Scallop condition (g\u22C5m\u207B\u00B3)")
save_plot(filename = "Y:/Offshore/Assessment/2025/Supporting_tasks/temperature/Condition_SST_May_2025.png",p,base_height = 6,base_width = 8)
save_plot(filename = "Y:/Offshore/Assessment/2025/Supporting_tasks/temperature/Condition_SST_May_2025.tiff",p,base_height = 6,base_width = 8)
save_plot(filename = "D:/Github/GB_SEAM/Figures/Productivity_paper/Condition_SST_May_2025.png",p,base_height = 6,base_width = 8)
save_plot(filename = "D:/Github/GB_SEAM/Figures/Productivity_paper/Condition_SST_May_2025.tiff",p,base_height = 6,base_width = 8)

#####################################################################################################
#End script
#####################################################################################################