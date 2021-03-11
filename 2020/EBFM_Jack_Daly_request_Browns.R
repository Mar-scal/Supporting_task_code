# For Jack Daly EAFM Browns Request
fork <- "FK"
direct_fns <- paste0("C:/Users/keyserf/Documents/Github/Assessment_fns/") #, fork, "/")
direct <- "C:/Users/keyserf/Documents/Version_control_pandemic/Offshore/Assessment/"

load(paste(direct,"/Data/Model/2020/BBn/Results/Model_results_and_diagnostics.RData",sep=""))
load(paste(direct,"/Data/Model/2020/BBn/Results/Final_model_results.RData",sep=""))

jack <- data.frame(year= DD.out$BBn$data$year[DD.out$BBn$data$year>1999 & DD.out$BBn$data$year<2019],
           exploitation_rate= round(DD.out$BBn$median$mu[which(DD.out$BBn$data$year>1999 & DD.out$BBn$data$year<2019)], 3),
           fully_recruited_biomass_mt= round(DD.out$BBn$median$B[which(DD.out$BBn$data$year>1999 & DD.out$BBn$data$year<2019)], 3))

write.csv(jack, paste0(direct, "2020/Supporting_tasks/EAFM_Browns_request/BrownsNorthScallop_Exploitation_Biomass_2000-2018.csv"))          
          