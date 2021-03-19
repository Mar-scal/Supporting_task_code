two_year_projections <- function(
  area="1A",
  year=2019, 
  exploitation=0.15, 
  runtype=c("Decision tables and plots"), #"Decision tables only"),
  surplus = 0, # if NULL, then it runs the full model, using m, mR, g, gR, etc. For final years, it just repeats them. 
  sample=0.1,
  path="Repo working directory (fast)",
  save=F, 
  pred.eval=T)
{
  ## packages
  require(tidyverse)
  require(purrr)
  require(dplyr)
  
  if(area %in% c("GBa", "BBn")) folder <- "Offshore"
  if(area %in% c("1A", "1B", "3", "4", "6")) folder <- "BoF"
  if(area %in% c("29A", "29B", "29C", "29D")) folder <- "29W"
  
  #################### Load data ###################
  
  direct_bof <- "//DCNSBIONA01A/edc_v1_shr/MARFIS/Shares/ESS/INSHORE SCALLOP/BoF"
  
  if(path == "Network (slow)") {
    load(file = paste0(direct_bof,"/2020/Assessment/Data/Model/SPA", area, "/SPA", area, "_Model_2019.RData"))
  }
  
  if(path == "Repo working directory (fast)"){
    load(file = paste0("./", folder, "/SFA_", area, "_2019.RData"))
  } 
  
  mod.res <- get(ls()[which(grepl(x=ls(), paste0("mod.res")))])
  
  if(path == "Repo working directory (fast)") message_txt <- paste0("Loading RData file from ", getwd())
  if(path == "Network (slow)") message_txt <- paste0("Loading RData file from ESS or Sky")
  
  if(area %in% c("GBa", "BBn")) folder <- "Offshore"
  if(area %in% c("1A", "1B", "3", "4", "6")) folder <- "BoF"
  if(area %in% c("29A", "29B", "29C", "29D")) folder <- "29W"  
  
  
  ############### Set up new landings, exploitation, and reference point info ############################
  RP <- data.frame(area = c("1A", "1B", "3", "4", "6", "29A", "29B", "29C", "29D", "GBa", "BBn"),
                   LRP = c(480, 880, 600, 530, NA, NA, 1.12, 1.41, 1.3, 7137, NA),
                   USR = c(1000, 1800, 1000, 750, NA, NA, 2.24, 2.82, 2.6, 13284, NA))
  
  LRP <- RP$LRP[RP$area == as.character(area)]
  USR <- RP$USR[RP$area == as.character(area)]
  
  LRP <- RP$LRP[RP$area == as.character(area)]
  USR <- RP$USR[RP$area == as.character(area)]
  
  # for 2020 projections, use 2020 landings.
  if(max(mod.res$Years)==2019){
    landings2020 <- data.frame(area=c("1A", "1B", "3", "4", "6", "29A", "29B", "29C", "29D", "GBa", "BBn"), 
                               landings = c(415, 545, 108, 113, 164, 6.567, 54.939, 20.392, 65.213, 4096, 211))
    message(paste0("landings for 2019 (used for 2020 projection) are ", landings2020$landings[landings2020$area==area], "t"))
    
    mod.res$data$C[length(mod.res$data$C)+1] <- landings2020$landings[landings2020$area==as.character(area)]
  }
  
  # for 2021 projections, use 2021 TACs for BoF and Offshore, use 2020 landings for 29W.
  if(max(mod.res$Years)==2019){
    landings2021 <- data.frame(area=c("1A", "1B", "3", "4", "6", "29A", "29B", "29C", "29D", "GBa", "BBn"), 
                               landings = c(270, 400, 200, 175, 210, 6.567, 54.939, 20.392, 65.213, 4000, 300))
    message(paste0("landings for 2020 (used for 2021 projection) are 2021 TAC: ", landings2021$landings[landings2021$area==area], "t"))
    mod.res$data$C[length(mod.res$data$C)+1] <- landings2021$landings[landings2021$area==as.character(area)]
  }
  
  # for 29W, deal with the different strata and tidy up. We decided to only look at the ones that are compared to reference points
  if(area == "29A") strata <- which(mod.res$labels == "Medium")
  if(area %in% c("29B", "29C", "29D")) strata <- which(mod.res$labels == "High")
  
  # if(grepl(area, pattern="29")) {
  #   
  #   #this gets used for SSModeltest_predict_2y function (for decision table)
  #   mod.res.original <- mod.res
  #   
  #   # the rest is for the boxplots
  #   # make an NY variable in SFA29 data
  #   mod.res$data$NY <- length(unique(mod.res$Years))
  #   double <- map(mod.res$data, function(x) class(x) =="matrix")
  #   single <- map(mod.res$data, function(x) !class(x) == "matrix")
  #   #for these, only keep the one with the strata
  #   stratified <- names(double[which(double==TRUE)])
  #   stratified <- map(mod.res$data[stratified], function(x) x[, strata])
  #   single <- names(single[which(single==TRUE)])
  #   single <- mod.res$data[single]
  #   mod.res$data <- c(single, stratified)
  #   
  #   sims.names <- names(as.data.frame(mod.res$sims.matrix))
  #   sims.names.strat <- which(grepl(x=sims.names, pattern=paste0(",", strata, "]"), fixed=T))
  #   sims.names.no <- which(!grepl(x=sims.names, pattern="[", fixed=T))
  #   sims.names.strat2 <- which(grepl(x=sims.names, pattern=paste0("[", strata, "]"), fixed=T))
  #   mod.res$sims.matrix <- mod.res$sims.matrix[,c(sims.names.strat, sims.names.no, sims.names.strat2)]
  #   colnames(mod.res$sims.matrix) <- gsub(x=colnames(mod.res$sims.matrix), pattern=paste0("[", strata,"]"), replacement = "", fixed=T)
  #   colnames(mod.res$sims.matrix) <- gsub(x=colnames(mod.res$sims.matrix), pattern=paste0(",", strata,"]"), replacement = "]", fixed=T)
  #   colnames(mod.res$sims.matrix) <- gsub(x=colnames(mod.res$sims.matrix), pattern="h", replacement = "", fixed=T)
  #   
  #   names(mod.res$data) <- gsub(x=names(mod.res$data), pattern="h", replacement = "", fixed=T)
  #   names(mod.res$data) <- gsub(x=names(mod.res$data), pattern="Catc", replacement = "Catch", fixed=T)
  #   
  #   mod.res$summary <- mod.res$summary[which(row.names(mod.res$summary) %in% names(as.data.frame(mod.res$sims.matrix))),]
  #   rownames(mod.res$summary) <- gsub(x=rownames(mod.res$summary), pattern=paste0("[", strata,"]"), replacement = "", fixed=T)
  #   rownames(mod.res$summary) <- gsub(x=rownames(mod.res$summary), pattern=paste0(",", strata,"]"), replacement = "]", fixed=T)
  #   rownames(mod.res$summary) <- gsub(x=rownames(mod.res$summary), pattern="h", replacement = "", fixed=T)
  #   
  #   mod.res$sims.matrix <- as.data.frame(mod.res$sims.matrix)
  #   
  #   for(i in 1:length(mod.res$data$r)){
  #     mod.res$sims.matrix$r <- mod.res$data$r[i]
  #     names(mod.res$sims.matrix)[names(mod.res$sims.matrix)=="r"] <- paste0("r[", i,"]")
  #   }
  # }
  # 
  
  #################### loop through years from beginning of time series up to y, to get 1 and 2y projections for each year. We opt to do this instead of reading in all of the old model run RDatas.  ############################
  if(runtype=="Decision tables and plots") {
    years <- min(mod.res$Years):year
    minY <- length(years)-9
    maxY <- length(years)
    years <- years[minY:maxY]
  }
  
  if(runtype=="Decision tables only") years <- year
  
  decision1 <- NULL
  decision2 <- NULL
  decisiontable <- NULL
  decisiontable2 <- NULL
  out <- NULL
  projdata <- NULL
  
  
  # cut down the number of iterations to use in the projections in order to improve run time
  if(!sample == "100") {
    set.seed(1)
    ntokeep <- dim(mod.res$sims.matrix)[1]*as.numeric(as.character(sample))
    keeprows <- sample(1:dim(mod.res$sims.matrix)[1], ntokeep)
    mod.res$sims.matrix <- mod.res$sims.matrix[keeprows,]
  }
  
  if(sample == "100") ntokeep <- "all"
  
  
  
  
  ##################### boxplots ########################
  # run the process error projections ****for the boxplots***!
  # e.g. for y = 2010, this does a 1y projection for 2011 using 2011 landings, and 2y projection for 2012, re-using 2011 landings.
  # However for the final years (2019), it uses the 2020 landings (realized) for the 2020 projection, and 2021 TAC for 2021 projection.
  # to use "realized" catch values set: exp = c(NA, NA)
  # to use an exploitation value for the 1st year projection, set exp = c(0.15, NA)
  # to use an exploitation value for the 2nd year projection, set exp = c(NA, 0.15)
  # to use exploitation values for BOTH years, set exp = c(0.15, 0.15)
  
  if(pred.eval==T){
    message("running projection evaluation (incl. figures)")
    source(paste0("./", folder, "/proj_eval_plot.R"))
    # note, proj_eval_plot uses process_2y_proj inside!
    realized <- proj_eval_plot(object=mod.res, area=area, surplus=surplus, mu=c(NA, NA), ref.pts=RP, save=save)
  }
  if(pred.eval==F){
    message("skipping projection evaluation")
    realized <- NULL
  }
  
  # run projections using a range of exploitation values
  message("generating decision tables")
  source(paste0("./", folder, "/process_2y_proj.R"))
  exp.range <- seq(0, 0.2, 0.01)
  
  checktable <- map_df(exp.range, function(x) process_2y_proj(object=mod.res, area=area, surplus=surplus, mu=c(x, NA), lastyear=T, decisiontable=F, LRP=LRP, USR=USR))
  checktable0 <- checktable$B.next0
  checktable1 <- checktable$B.next1
  
  source(paste0(folder, "/overall_29_stats.R"))
  checktable1 <- overall_29_stats(object=checktable1, strata.labs = mod.res$labels) %>%
    dplyr::arrange(row, strata)
  checktable0 <- overall_29_stats(object=checktable0, strata.labs = mod.res$labels) %>%
    dplyr::arrange(row, strata)
  
  
    checktable1$B.change[is.na(checktable1$B.change)] <- (checktable1$Biomass[is.na(checktable1$B.change)]  - checktable0$Biomass[is.na(checktable1$B.change)]) / checktable0$Biomass[is.na(checktable1$B.change)] * 100
  # Probability of biomass increase.  What runs are less than 0.
  checktable1$pB0_increase[is.na(checktable1$pB0_increase)] <- 0 < (checktable1$Biomass[is.na(checktable1$pB0_increase)]-checktable0$Biomass[is.na(checktable1$pB0_increase)])
  
  areas <- data.frame(area = mod.res$data$Area, strata=mod.res$labels)
  
  decisiontable <- function(object, proj, year, LRP, USR){
    object <- object[object$proj==proj & object$year == year,]
    return(
      object %>%
        left_join(areas, by="strata") %>%
        dplyr::mutate(mu = round(mu,2)) %>%
        dplyr::group_by(year, proj, mu, strata) %>%
        dplyr::summarize(biomass=median(Biomass),
                         catch=median(catch),
                         mu=median(mu),
                         B.change=median(B.change),
                         pB0_increase = sum(pB0_increase)/length(pB0_increase),
                         p.LRP = round(sum(Biomass/area > LRP)/length(Biomass), 3),
                         p.USR = round(sum(Biomass/area > USR)/length(Biomass), 3))
    )
  }
  
  # source("./predict.ssmodeltest.r")
  # predict.ssmodeltest(A.mod.res)
  
  checktable <- decisiontable(checktable1, proj=1, year=2020, LRP=LRP, USR=USR)  
  
  
  decision <- process_2y_proj(object=mod.res, area=area, surplus=surplus, mu=c(exploitation, NA), lastyear=F, decisiontable=F, LRP=LRP, USR=USR)
  
  decision0 <- decision$B.next0
  decision1 <- decision$B.next1
  
  source(paste0(folder, "/overall_29_stats.R"))
  decision0 <- overall_29_stats(object=decision0, strata.labs = mod.res$labels) %>%
    dplyr::arrange(row, strata)
  decision1 <- overall_29_stats(object=decision1, strata.labs = mod.res$labels) %>%
    dplyr::arrange(row, strata)
  
  
  decision1$B.change[is.na(decision1$B.change)] <- (decision1$Biomass[is.na(decision1$B.change)]  - decision0$Biomass[is.na(decision1$B.change)]) / decision0$Biomass[is.na(decision1$B.change)] * 100
  # Probability of biomass increase.  What runs are less than 0.
  decision1$pB0_increase[is.na(decision1$pB0_increase)] <- 0 < (decision1$Biomass[is.na(decision1$pB0_increase)]-decision0$Biomass[is.na(decision1$pB0_increase)])

  # source("./predict.ssmodeltest.r")
  # predict.ssmodeltest(A.mod.res)
  
  # indexing of list is [[exp]][[year]][[strata]]
  decision2 <- map_df(exp.range, function(x) process_2y_proj(object=mod.res, area=area, surplus=surplus, mu=c(NA, x), lastyear=F, decisiontable=F, LRP=LRP, USR=USR))

  source(paste0(folder, "/overall_29_stats.R"))
  decision2.0 <- overall_29_stats(object=decision2$B.next0, strata.labs = mod.res$labels) %>%
    dplyr::arrange(row, strata)
  decision2.1 <- overall_29_stats(object=decision2$B.next1, strata.labs = mod.res$labels) %>%
    dplyr::arrange(row, strata)
  decision2.2 <- overall_29_stats(object=decision2$B.next2, strata.labs = mod.res$labels) %>%
    dplyr::arrange(row, strata)
  
  
  decision2.1$B.change[is.na(decision2.1$B.change)] <- (decision2.1$Biomass[is.na(decision2.1$B.change)]  - decision2.0$Biomass[is.na(decision2.1$B.change)]) / decision2.0$Biomass[is.na(decision2.1$B.change)] * 100
  # Probability of biomass increase.  What runs are less than 0.
  decision2.1$pB0_increase[is.na(decision2.1$pB0_increase)] <- 0 < (decision2.1$Biomass[is.na(decision2.1$pB0_increase)]-decision2.0$Biomass[is.na(decision2.1$pB0_increase)])
  
  decision2.2$B.change[is.na(decision2.2$B.change)] <- (decision2.2$Biomass[is.na(decision2.2$B.change)]  - decision2.1$Biomass[is.na(decision2.2$B.change)]) / decision2.1$Biomass[is.na(decision2.2$B.change)] * 100
  # Probability of biomass increase.  What runs are less than 0.
  decision2.2$pB0_increase[is.na(decision2.2$pB0_increase)] <- 0 < (decision2.2$Biomass[is.na(decision2.2$pB0_increase)]-decision2.1$Biomass[is.na(decision2.2$pB0_increase)])
  
  # source("./predict.ssmodeltest.r")
  # predict.ssmodeltest(A.mod.res)
  decision.1 <- map_df(2:length(unique(decision1$year)), function(x) 
    decisiontable(decision1, proj=1, year=unique(decision1$year)[x], LRP=LRP, USR=USR))
  
  decision.2 <- map_df(2:length(unique(decision2.2$year)), function(x) 
    decisiontable(decision2.2, proj=2, year=unique(decision2.2$year)[x], LRP=LRP, USR=USR))
  
  total <- decision.2[decision.2$strata == "Total" & decision.2$proj==2,]
  
  HCRscenario1 <- total %>%
    dplyr::group_by(year, strata) %>%
    dplyr::filter(mu == exploitation) %>%
    dplyr::select(year, mu, p.USR, strata) %>%
    dplyr::summarise(mu = max(mu)) %>%
    dplyr::left_join(., total, by=c("year", "mu", "strata")) %>%
    dplyr::full_join(., decision.1[decision.1$strata == "Total",], by = c("year", "mu", "proj", "strata", "biomass", "catch", "B.change", "pB0_increase", "p.LRP", "p.USR")) 
  
  HCRscenario2 <- total %>%
    dplyr::group_by(year, strata) %>%
    dplyr::filter(p.USR>0.75 | mu==0) %>%
    dplyr::filter(mu <= exploitation) %>%
    dplyr::select(year, mu, p.USR, strata) %>%
    dplyr::summarise(mu = max(mu)) %>%
    dplyr::left_join(., total, by=c("year", "mu", "strata")) %>%
    dplyr::full_join(., decision.1[decision.1$strata == "Total",], by = c("year", "mu", "proj", "strata", "biomass", "catch", "B.change", "pB0_increase", "p.LRP", "p.USR")) 
  
  ################## Decision impact ####################
  message("running decision impact analysis")
  source(paste0("./", folder, "/decision_impact.R"))
  impact_HCR1 <- decision_impact(HCRscenario1, save=save, area=area, surplus=surplus, HCRscenario = 1)
  if(!area=="BBn") impact_HCR2 <- decision_impact(HCRscenario2, save=save, area=area, surplus=surplus, HCRscenario = 2)
  if(area=="BBn") impact_HCR2 <- NULL
  
  print("*******Done this scenario run!*******")
  
  return(list(checktable=checktable,
              decision1=decision.1,
              decision.df=decision.2, 
              realized=realized,
              #exploit.based = exploit.based,
              impact_HCR1=impact_HCR1,
              impact_HCR2=impact_HCR2))
}
