decision_impact <- function(object, surplus, area, save, HCRscenario){
  
  require(patchwork)
  
  if(area %in% c("GBa", "BBn")) folder <- "Offshore"
  if(area %in% c("1A", "1B", "3", "4", "6")) folder <- "BoF"
  if(area %in% c("29A", "29B", "29C", "29D")) folder <- "29W"  
  
  decisions <- object %>%
    dplyr::select(proj, year, catch) %>%
    dplyr::group_by(proj, year) %>%
    dplyr::filter(proj %in% c(1,2))%>%
    dplyr::summarize(catch=median(catch)) %>%
    tidyr::pivot_wider(names_from=proj, values_from=catch) %>%
    dplyr::rename("real"=`1`, "proj"=`2`) %>%
    dplyr::mutate(missedout = proj - real,
                  missedout.p = (proj/real) - 1,
                  year=as.numeric(as.character(year)))
  
  # labelling
  if(!is.null(surplus[[1]])){
    if(surplus[[1]]==0) tag1 <- "surplus0"
    if(!surplus[[1]]==0) tag1 <- "surplus_other"
  }
  if(is.null(surplus[[1]])) tag1 <- "surplus_lastyear"
  
  # limits
  if(area=="GBa") {
    break1 <- seq(-8000, 8000, 2000)
    break2 <- seq(-1.5, 1.5, 0.5) 
    break3 <- seq(0, 20000, 5000) 
  }
  if(area=="BBn") {
    break1 <- seq(-800, 800, 200)
    break2 <- seq(-1, 1, 0.5) 
    break3 <- seq(0, 2000, 500) 
  }
  
  if(!area%in%c("BBn", "GBa", "1B", "4")) {
    break1 <- seq(-500, 500, 100)
    break2 <- seq(-1, 1, 0.5) 
    break3 <- seq(0, 1000, 200) 
  }
  
  if(area == "1B") {
    break1 <- seq(-500, 500, 100)
    break2 <- seq(-1, 1, 0.5) 
    break3 <- seq(0, 1000, 200) 
  }
  
  if(area == "4") {
    break1 <- seq(-500, 500, 100)
    break2 <- seq(-1, 1, 0.5) 
    break3 <- seq(0, 600, 200) 
  }
  
  if(area %in% c("29A", "29B", "29C", "29D")){
    break1 <- seq(-50, 50, 10)
    break2 <- seq(-2, 2, 1) 
    break3 <- seq(0, 50, 10) 
  }
  
  catchvals <- decisions[!is.na(decisions$missedout),] %>%
    dplyr::select(year, real, proj) %>%
    pivot_longer(cols = c("real", "proj"))
  
  evaluation_catch <- 
    ggplot() +
    geom_point(data=catchvals,
               aes(x=year, value, fill=name, shape=name), colour="black", size=4)+
    xlab("Year") +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18)) +
    scale_fill_brewer(type = "qual", palette = "Paired", name=NULL, labels=c(expression(C[y[2]]), expression(C[y[1]]))) +
    scale_shape_manual(values=c(21,24), name=NULL, labels=c(expression(C[y[2]]), expression(C[y[1]]))) + 
    ylab("Catch potential (mt)") +
    annotate(geom="text", label=HCRscenario, hjust=0) +
    ggtitle(paste0(area, " - ", tag1))+
    scale_y_continuous(breaks=break3, limits=c(min(break3), max(break3)))
  
  
  evaluation3 <- 
    ggplot() +
    geom_bar(data=decisions[!is.na(decisions$missedout),],
             aes(x=year, missedout),
             stat="identity", fill="grey", colour="black")+
    geom_hline(data = decisions[!is.na(decisions$missedout),], aes(yintercept=0))+
    xlab("Year") +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18)) +
    ylab("Difference in catch potential (mt)") +
    annotate(geom="text", label=HCRscenario, hjust=0) +
    ggtitle(paste0(area, " - ", tag1), subtitle=expression(C[y[2]]-C[y[1]]))+
    scale_y_continuous(breaks=break1, limits=c(min(break1), max(break1)))
  
  evaluation4 <- 
    ggplot() +
    geom_bar(data=decisions[!is.na(decisions$missedout.p) & decisions$missedout.p<Inf,],
             aes(x=year, missedout.p),
             stat="identity", fill="grey", colour="black")+
    geom_hline(data = decisions[!is.na(decisions$missedout.p) & decisions$missedout.p<Inf,], aes(yintercept=0))+
    xlab("Year") +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18)) +
    ylab("Proportional difference in catch potential") +
    annotate(geom="text", label=HCRscenario, hjust=0) +
    ggtitle(paste0(area, " - ", tag1), subtitle=expression((C[y[2]]/C[y[1]])-1))+
    scale_y_continuous(breaks=break2, limits=c(min(break2), max(break2)))
  
  if(Inf %in% decisions$missedout.p){
    Infinite.p <- decisions[decisions$missedout.p==Inf &!is.na(decisions$missedout.p),]
    Infinite.p$label <- "No\nrealized\ncatch"
    evaluation4 <- evaluation4 + geom_text(data=Infinite.p,
                            aes(x=year, y=max(break2)*0.1, label=label))
  }
  
  if(save==T){
    
    if(!dir.exists(paste0("./", folder, "/", area, "/", tag1))) dir.create(paste0("./", folder, "/", area, "/", tag1))
    
    png(filename = paste0("./", folder, "/", area, "/", tag1, "/catch_decision_", tag1, "_HCRScenario_", HCRscenario, ".png"), width=11, height=8.5, units="in", res=400)
    print(evaluation_catch)
    dev.off()
    
    png(filename = paste0("./", folder, "/", area, "/", tag1, "/decision_impact_", tag1, "_HCRScenario_", HCRscenario, ".png"), width=11, height=8.5, units="in", res=400)
    print(evaluation3)
    dev.off()
    
    png(filename = paste0("./", folder, "/", area, "/", tag1, "/decision_impact2_", tag1, "_HCRScenario_", HCRscenario, ".png"), width=11, height=8.5, units="in", res=400)
    print(evaluation4)
    dev.off()
    
  }
  
  
  return(list(decisions=decisions, evaluation_catch =  evaluation_catch, evaluation3 = evaluation3, evaluation4 = evaluation4))
}
