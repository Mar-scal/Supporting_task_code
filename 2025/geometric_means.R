load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2025/Survey_summary_output/Survey_all_results.Rdata")

# SFA 27B
surv.27b <- survey.obj$GBb$model.dat # Need to add missing 2020 survey in here.
surv.27b <- rbind(surv.27b,c(2020,rep(NA,ncol(surv.27b)-1)))
surv.27b <- surv.27b |> collapse::fsubset(year >=1994)
surv.27b <- surv.27b[order(surv.27b$year),] # get the years ordered correctly for the rolling mean
# q corrected
q.index <- 0.33
surv.27b$I <- as.numeric(surv.27b$I/q.index)
surv.27b$IR <- as.numeric(surv.27b$IR/q.index)
surv.27b$IG <- surv.27b$I
for(i in 1:nrow(surv.27b)){
  if(is.na(surv.27b$IG[i]) & i>1) surv.27b$IG[i] <- surv.27b$IG[i-1] 
}
surv.27b$gma <- exp(rollmean(log(surv.27b$IG),3,align='right', fill=NA))

ggplot() + geom_line(data=surv.27b, aes(year, I), colour='black') +
  geom_line(data=surv.27b, aes(year, gma), linetype="dashed")


ger.shape <- github_spatial_import(subfolder="other_boundaries", zipname="other_boundaries.zip", specific_shp = "WGS_84_German.shp")
#ger.shape <- st_read("C:/Users/keyserf/Documents/Github/GIS_layers/other_boundaries/WGS_84_German.shp")
ger.shape <-  ger.shape %>% st_make_valid() %>% st_transform(32619)
ger.area.km2 <- st_area(ger.shape)/1e6
# I should be using the "merged.survey.obj here....
# SFA 26C
#surv.26c <- lined.survey.obj$model.dat # Has NA's for missing 2020 so good to use as is
#surv.26c <- surv.26c |> collapse::fsubset(year >=1994)
surv.26c <- merged.survey.obj # Need to add in 2020 missing...
surv.26c[nrow(surv.26c)+1,] <- NA
surv.26c$year[nrow(surv.26c)] <- 2020
# and reorder
surv.26c <- surv.26c[order(surv.26c$year),]
# Now the units of german are....? If it is grams/tow... this puts us in total tonnes
surv.26c$I <- as.numeric(surv.26c$I /atow*ger.area.km2/1e6)
surv.26c$IR <- as.numeric(surv.26c$IR/atow*ger.area.km2/1e6)

# Q correct
q.index <- 0.33
surv.26c$I <- surv.26c$I/q.index
surv.26c$IR <- surv.26c$IR/q.index
surv.26c$IG <- surv.26c$I
for(i in 1:nrow(surv.26c)){
  if(is.na(surv.26c$IG[i]) & i>1) surv.26c$IG[i] <- surv.26c$IG[i-1] 
}
surv.26c$gma <- exp(rollmean(log(surv.26c$IG),3,align='right', fill=NA))

ggplot() + geom_line(data=surv.26c, aes(year, I), colour='black') +
  geom_line(data=surv.26c, aes(year, gma), linetype="dashed")
