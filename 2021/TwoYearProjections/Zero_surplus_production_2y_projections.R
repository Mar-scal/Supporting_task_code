
# Bring in functions
direct_fns <- "C:/Users/keyserf/Documents/GitHub/BoF/"
source(paste0(direct_fns, "simple_2y_proj.R"))
source(paste0(direct_fns, "process_2y_proj.R"))

# packages
require(tidyverse)
require(ggplot2)
require(MALDIquant)
require(ggridges)

# choose your area
area <- "1B" #this would be the SPA, for entries options are to use: 1A, 1B, 3, 4, or 6  
assessmentyear <- 2020 #year in which you are conducting the assessment 
surveyyear <- 2019  #last year of survey data you are using, e.g. if max year of survey is survey from summer 2019, this would be 2019 

# load model file you're working on 
if(!area=="1A") load(paste0("Z:/INSHORE SCALLOP/BoF/", assessmentyear, "/Assessment/Data/Model/SPA", area, "/SPA", area, "_Model_", surveyyear, ".RData"))
if(area=="1A") load(paste0("Z:/INSHORE SCALLOP/BoF/", assessmentyear, "/Assessment/Data/Model/SPA", area, "/Model_Final_fullcatchInModelledAreain2019/SPA", area, "_Model_", surveyyear, ".RData"))

#or 
#load(paste0(direct_fns,"/SPA6_Model_2019.RData"))

# area 4 is 1983-2019
# area 3 is 1996-2019
# area 1A is 1997-2019
# area 1B is 1997-2019
# area 6 is 2006-2019 but actually only 2009?

# get the model data renamed
if(!area==3) assign(value = get(paste0("Spa", area, ".", surveyyear)), "model.obj")  #rename as model.obj 
if(area==3) assign(value = get(paste0("Spa", area, ".new.", surveyyear)), "model.obj")  #rename as model.obj 

# start by getting the simple projections
results_simple <- simple_2y_proj(model.obj) 

# tidy up and label the output
B.next_JS <- do.call(rbind, results_simple$B.next)
B.next_JS$proj <- 1
B.next_JS$type <- "simple"
B.next2_JS <- do.call(rbind, results_simple$B.next2)
B.next2_JS$proj <- 2
B.next2_JS$type <- "simple"

# pull the exploitation rates from the simple output for use in the process error version
rates <- arrange(unique(B.next2_JS[, c('year', 'exp')]), year)$exp

# prep the plotting function
plot_results <- function(){
  # combine with the simple projections output
  all <- rbind(B.next_JS, B.next_FK, B.next2_JS, B.next2_FK)
  
  # summarize it
  sum_all <- all %>%
    group_by(year, proj, type) %>%
    summarize(min=min(Biomass),
              med=median(Biomass),
              max=max(Biomass),
              catch=unique(catch), 
              exp = unique(exp)) 
  
  # subset to facilitate plotting
  keep <- sample(x=1:60000, 6000)
  all <- all %>%
    group_by(year, proj, type) %>%
    mutate(num = 1:60000)
  subset_all <- all[all$num %in% keep,]
  sum_all$orig.year <- ifelse(sum_all$proj==1, sum_all$year-1, 
                              ifelse(sum_all$proj==2, sum_all$year-2, NA))
  
  basic_2y_comparison <- ggplot() + geom_point(data=sum_all[sum_all$proj==2,], aes(factor(year), med, colour=type), position=position_dodge(0.5)) + 
    geom_errorbar(data=sum_all[sum_all$proj==2,], aes(ymin=min, ymax=max, x=factor(year), colour=type), position=position_dodge(0.5))+
    geom_text(data=sum_all[sum_all$proj==2,], aes(y=min, x=factor(year), group=type, label=round(catch, 0)), position=position_dodge(0.5), vjust=1)+
    geom_text(data=sum_all[sum_all$proj==2,], aes(y=max, x=factor(year), group=type, label=exp), position=position_dodge(0.5), vjust=-1)+
    facet_wrap(~proj) +
    theme_bw()
  
  basic_method_comparison <- ggplot() + geom_point(data=sum_all, aes(factor(year), med, colour=factor(proj)), position=position_dodge(0.5)) + 
    geom_errorbar(data=sum_all, aes(ymin=min, ymax=max, x=factor(year), colour=factor(proj)), position=position_dodge(0.5))+
    facet_wrap(~type) +
    theme_bw()
  
  # png("./Comparing_freya_jessica_2y_ridges.png", height=6, width=8, units="in", res=400)
  ridges_2y_comparison <- ggplot() + geom_density_ridges(data=subset_all[subset_all$proj == "2",], 
                                                         aes(x=Biomass, y=factor(year), fill=type, colour=type, lty=type, height=..density..), 
                                                         stat = "density", trim = TRUE, alpha=0.3, lwd=1) + 
    coord_flip()+
    theme_bw() + theme(panel.grid=element_blank()) +
    scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
    scale_colour_manual(values=c("#d7191c", "#2c7bb6"))
  #dev.off()
  
  #png("./Comparing_freya_jessica_2y_ridges_log.png", height=6, width=8, units="in", res=400)
  log_ridges_2y_comparison <- ggplot() + geom_density_ridges(data=subset_all[subset_all$proj == "2",], 
                                                             aes(x=log(Biomass), y=factor(year), fill=type, colour=type, lty=type, height=..density..), 
                                                             stat = "density", trim = TRUE, alpha=0.3, lwd=1) + 
    coord_flip()+
    theme_bw() + theme(panel.grid=element_blank()) +
    scale_fill_manual(values=c("#d7191c", "#2c7bb6")) +
    scale_colour_manual(values=c("#d7191c", "#2c7bb6"))
  #dev.off()
  
  return(output=list(basic_2y_comparison=basic_2y_comparison, basic_method_comparison=basic_method_comparison, 
                     ridges_2y_comparison=ridges_2y_comparison, log_ridges_2y_comparison=log_ridges_2y_comparison))
}

# run the process error projections for 0_surplus
results_process <- process_2y_proj(object=model.obj, scenario="0_surplus_simple", type="exp", rate=rates)

# tidy up the output
B.next_FK <- do.call(rbind, results_process$B.next)
B.next_FK$proj <- 1
B.next_FK$type <- "process"
B.next2_FK <- do.call(rbind, results_process$B.next2)
B.next2_FK$proj <- 2
B.next2_FK$type <- "process"

zero <- plot_results()
zero$basic_2y_comparison
zero$basic_method_comparison
zero$ridges_2y_comparison
zero$log_ridges_2y_comparison

# run the process error projections for full process error method
results_process <- process_2y_proj(object=model.obj, scenario="full", type="exp", rate=rates)

# tidy up the output
B.next_FK <- do.call(rbind, results_process$B.next)
B.next_FK$proj <- 1
B.next_FK$type <- "process"
B.next2_FK <- do.call(rbind, results_process$B.next2)
B.next2_FK$proj <- 2
B.next2_FK$type <- "process"

full <- plot_results()
full$basic_2y_comparison
full$basic_method_comparison
full$ridges_2y_comparison
full$log_ridges_2y_comparison

