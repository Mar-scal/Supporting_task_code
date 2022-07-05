refpts <- NULL
for(y in 2016:2020){
  print(y)
  if(y %in% c(2016, 2017, 2019, 2020)) load(paste0("Y:/Offshore/Assessment/Data/Model/", y, "/GBa/Results/Final_model_results.RData"))
  if(y == 2018) load(paste0("C:/Users/keyserf/Documents/Data/Model/", y, "/GBa/Results/Model_testing_results_mixed.RData"))
  
  if(!y==2016) refyears <- which(DD.out$GBa$data$year %in% 1986:2009)
  if(y==2016) refyears <- which(DD.out$GBa$data$iyr %in% 1986:2009)
  
  LRP <- mean(DD.out$GBa$median$B[refyears]) * 0.3 #4 724 # 4673
  USR <- mean(DD.out$GBa$median$B[refyears]) * 0.8 #12 597 # 12461
  
  refpts <- rbind(refpts, data.frame(y, LRP, USR))
}

y=2016
