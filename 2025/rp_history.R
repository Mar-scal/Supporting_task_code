#history of LRPs
year <- c(2016,2017,2019,2020,2022,2023,2024,2025)
#2018 fails
lrp<-NULL
for(y in year){
  print(y)
  if(!y==2025) file <- paste0("C:/Users/keyserf/Documents/temp_data/Data/Model/", y, "/Final_model_results.Rdata")
  if(y==2025) file <- paste0("C:/Users/keyserf/Documents/temp_data/Data/Model/", y, "/Model_testing_results.Rdata")
  if(file.exists(file)) {
    load(file)
    
    #30% of long-term mean modelled biomass from 1986 to 2009
    lrpyears <- which(DD.out$GBa$data$year%in%1986:2009)
    if(y==2016) lrpyears <- which(DD.out$GBa$data$iyr%in%1986:2009)
    bmsy30 <- mean(DD.out$GBa$median$B[lrpyears], na.rm=T) * 0.3 
    
    lrp <- rbind(lrp, data.frame(year=y, lrp=bmsy30))
  }   
}
