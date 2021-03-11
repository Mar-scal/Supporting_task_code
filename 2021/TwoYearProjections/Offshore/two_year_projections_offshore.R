two_year_projections_offshore <- function(
  area="GBa",
  year=2019, 
  exploitation=0.15, 
  runtype=c("Decision tables and plots"), #"Decision tables only"),
  plot=c("Standard boxplot"), #"Functional boxplot"),
  surplus = 0, # if NULL, then it runs the full model, using m, mR, g, gR, etc. For final years, it just repeats them. 
  sample=0.1,
  path="Repo working directory (fast)",
  save=F)
{
  ## packages
  require(tidyverse)
  require(purrr)
  require(dplyr)
  
  
  #################### Load data ###################
  
  direct_off <- "//142.2.93.33/Offshore/Assessment/Data/Model"
  
  if(path == "Network (slow)") {
    if(area %in% c("GBa", "BBn")) mod.res <- get(load(file = paste0(direct_off,"/2020/", area, "/Results/Final_model_results.RData")))
  }
  
  if(path == "Repo working directory (fast)"){
    if(area %in% c("GBa", "BBn")) mod.res <- get(load(file = paste0("./Offshore/Final_model_results_", area, ".RData")))
  } 
  
  if(area %in% c("GBa", "BBn")) mod.res <- list(data=mod.res[[area]], summary = DD.out[[area]]$summary, sims.matrix=DD.out[[area]]$sims.list)
  
  if(path == "Repo working directory (fast)") message_txt <- paste0("Loading RData file from ", getwd())
  if(path == "Network (slow)") message_txt <- paste0("Loading RData file from ESS or Sky")
  
  
  
  
  
  ############### Set up new landings, exploitation, and reference point info ############################
  RP <- data.frame(area = c("GBa", "BBn"),
                   LRP = c(7137, NA),
                   USR = c(13284, NA))
  
  LRP <- RP$LRP[RP$area == as.character(area)]
  USR <- RP$USR[RP$area == as.character(area)]
  
  if(is.null(mod.res$Years)) mod.res$Years <- mod.res$data$year
  
  # NO DON'T DO THIS:
  # # for offshore only, add proj.catch[[bnk]] to the catches
  # if(area == "GBa") mod.res$data$C <- mod.res$data$C + proj.dat[[area]]$catch[proj.dat[[area]]$year %in% 1986:2019]
  # if(area == "BBn") mod.res$data$C <- mod.res$data$C + proj.dat[[area]]$catch
  
  # for 2020 projections, use 2020 landings.
  if(max(mod.res$Years)==2019){
    landings2020 <- data.frame(area=c("GBa", "BBn"), 
                               landings = c(4096, 211)) 
    message(paste0("landings for 2019 (used for 2020 projection) are ", landings2020$landings[landings2020$area==area], "t"))
    
    mod.res$data$C[length(mod.res$data$C)+1] <- landings2020$landings[landings2020$area==as.character(area)]
  }
  
  # for 2021 projections, use 2021 TACs for BoF and Offshore, use 2020 landings for 29W.
  if(max(mod.res$Years)==2019){
    landings2021 <- data.frame(area=c("GBa", "BBn"), 
                               landings = c(4000, 300))
    message(paste0("landings for 2020 (used for 2021 projection) are 2021 TAC: ", landings2021$landings[landings2021$area==area], "t"))
    mod.res$data$C[length(mod.res$data$C)+1] <- landings2021$landings[landings2021$area==as.character(area)]
  }
  
  
  
  
  
  ###################### Make the mod.res look like BoF ##########################
  if(area %in% "GBa") {
    mod.res$sims.matrix <- mod.res$sims.matrix[!names(mod.res$sims.matrix) %in% names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) == 21))]
    standard <- mod.res$sims.matrix[names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) > 1))]
    newnames <- paste0(rep(names(standard), each=mod.res$data$NY), "[", 1:mod.res$data$NY, "]")
    standard <- as.data.frame(do.call(cbind, standard))
    names(standard) <- newnames
    single <- mod.res$sims.matrix[names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) == 1))]
    mod.res$sims.matrix <- cbind(standard, data.frame(K = single$K, S=single$S, deviance=single$deviance, ikappa.rho2 = single$ikappa.rho2,
                                                      ikappa.tau2 = single$ikappa.tau2, logK = single$logK, q = single$q, qU = single$qU, sigma=single$sigma))
  }
  
  if(area %in% "BBn") {
    mod.res$sims.matrix <- mod.res$sims.matrix[!names(mod.res$sims.matrix) %in% names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) == 41))]
    standard <- mod.res$sims.matrix[names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) > 1))]
    newnames <- paste0(rep(names(standard), each=mod.res$data$NY), "[", 1:mod.res$data$NY, "]")
    standard <- as.data.frame(do.call(cbind, standard))
    names(standard) <- newnames
    single <- mod.res$sims.matrix[names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) == 1))]
    mod.res$sims.matrix <- cbind(standard, data.frame(K = single$K, S=single$S, deviance=single$deviance, ikappa.rho2 = single$ikappa.rho2,
                                                      ikappa.tau2 = single$ikappa.tau2, logK = single$logK, q = single$q, qU = single$qU, sigma=single$sigma))
  }
  
  
  
  
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
  message("running projection evaluation (incl. figures)")
  # run the process error projections ****for the boxplots***!
  # e.g. for y = 2010, this does a 1y projection for 2011 using 2011 landings, and 2y projection for 2012, re-using 2011 landings.
  # However for the final years (2019), it uses the 2020 landings (realized) for the 2020 projection, and 2021 TAC for 2021 projection.
  # to use "realized" catch values set: exp = c(NA, NA)
  # to use an exploitation value for the 1st year projection, set exp = c(0.15, NA)
  # to use an exploitation value for the 2nd year projection, set exp = c(NA, 0.15)
  # to use exploitation values for BOTH years, set exp = c(0.15, 0.15)
  source("./Offshore/proj_eval_plot.R")
  # note, proj_eval_plot uses process_2y_proj inside!
  realized <- proj_eval_plot(object=mod.res, area=area, surplus=surplus, mu=c(NA, NA), plot=plot, ref.pts=RP, save=save)

  
  # run projections using a range of exploitation values
  message("generating decision tables")
  source("./Offshore/process_2y_proj_offshore.R")
  exp.range <- seq(0, 0.3, 0.01)
  
  checktable <- do.call(rbind, map_df(exp.range, function(x) process_2y_proj(object=mod.res, area=area, surplus=surplus, mu=c(x, NA), decisiontable=T))$B.next1)
  
  decision <- map(exp.range, function(x) process_2y_proj(object=mod.res, area=area, surplus=surplus, mu=c(exploitation, x), decisiontable=F))
  
  # tidy up the output
  decision.df <- NULL
  for(i in 1:length(decision)){
    B.next0 <- do.call(rbind, decision[[i]]$B.next0)
    B.next1 <- do.call(rbind, decision[[i]]$B.next1)
    B.next2 <- do.call(rbind, decision[[i]]$B.next2)
    process <- rbind(B.next0, B.next1, B.next2)
    decision.df <- rbind(decision.df, process)
  }
  
  decisiontable <- function(object, proj, year, LRP, USR){
    object <- object[object$proj==proj & object$year == year,]
    return(
      object %>%
        dplyr::mutate(mu = round(mu,2)) %>%
        dplyr::group_by(year, proj, mu) %>%
        dplyr::summarize(biomass=median(Biomass),
                         catch=median(catch),
                         mu=median(mu),
                         Fmort = median(Fmort),
                         B.change=median(B.change),
                         pB0 = sum(pB0)/length(pB0),
                         p.LRP = round(sum(Biomass > LRP)/length(Biomass), 3),
                         p.USR = round(sum(Biomass > USR)/length(Biomass), 3))
    )
  }
  
  checktable <- decisiontable(checktable, proj=1, year=2020, LRP=LRP, USR=USR)
  
  decision.1 <- map_df(3:length(unique(decision.df$year)), function(x) 
    decisiontable(decision.df, proj=1, year=unique(decision.df$year)[x], LRP=LRP, USR=USR))
  
  decision.2 <- map_df(3:length(unique(decision.df$year)), function(x) 
    decisiontable(decision.df, proj=2, year=unique(decision.df$year)[x], LRP=LRP, USR=USR))
  
  
  HCRscenario1 <- decision.2 %>%
    dplyr::group_by(year) %>%
    dplyr::filter(mu == exploitation) %>%
    dplyr::select(year, mu, catch, p.USR) %>%
    dplyr::summarise(mu = max(mu)) %>%
    dplyr::left_join(., decision.2, by=c("year", "mu")) %>%
    dplyr::full_join(., decision.1, by = c("year", "mu", "proj", "biomass", "catch", "Fmort", "B.change", "pB0", "p.LRP", "p.USR"))
  
  
  HCRscenario2 <- decision.2 %>%
    dplyr::group_by(year) %>%
    dplyr::filter(p.USR>0.75) %>%
    dplyr::filter(mu <= exploitation) %>%
    dplyr::select(year, mu, catch, p.USR) %>%
    dplyr::summarise(mu = max(mu)) %>%
    dplyr::left_join(., decision.2, by=c("year", "mu")) %>%
    dplyr::full_join(., decision.1, by = c("year", "mu", "proj", "biomass", "catch", "Fmort", "B.change", "pB0", "p.LRP", "p.USR"))
  

  
  
  ################## Decision impact ####################
  message("running decision impact analysis")
  source("./Offshore/decision_impact.R")
  impact_HCR1 <- decision_impact(HCRscenario1, save=save, area=area, surplus=surplus, HCRscenario = 1)
  if(!area=="BBn") impact_HCR2 <- decision_impact(HCRscenario2, save=save, area=area, surplus=surplus, HCRscenario = 2)
  if(area=="BBn") impact_HCR2 <- NULL
  
  print("*******Done this scenario run!*******")
  
  return(list(checktable=checktable,
              decision.1=decision.1,
              decision.2=decision.2, 
              realized=realized,
              #exploit.based = exploit.based,
              impact_HCR1=impact_HCR1,
              impact_HCR2=impact_HCR2))
}