decision_impact <- function(object, surplus, area, save, HCRscenario){
  
  decisions <- object %>%
    dplyr::select(proj, year, catch) %>%
    dplyr::group_by(proj, year) %>%
    dplyr::filter(proj %in% c(1,2))%>%
    dplyr::summarize(catch=median(catch)) %>%
    tidyr::pivot_wider(names_from=proj, values_from=catch) %>%
    dplyr::rename("real"=`1`, "proj"=`2`) %>%
    dplyr::mutate(missedout = real - proj,
                  year=as.numeric(as.character(year)))
  
  evaluation3 <- 
    ggplot() +
    geom_bar(data=decisions[!is.na(decisions$missedout),],
             aes(x=year, missedout),
             stat="identity")+
    xlab("Year") +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18)) +
    ylab("Difference in catch (mt)") +
    annotate(geom="text", label=HCRscenario, hjust=0)
  
  if(save==T){
    if(!is.null(surplus[[1]])){
      if(surplus[[1]]==0) tag1 <- "surplus0"
      if(!surplus[[1]]==0) tag1 <- "surplus_other"
    }
    if(is.null(surplus[[1]])) tag1 <- "surplus_lastyear"
    
    if(!dir.exists(paste0("./", area, "/", tag1))) dir.create(paste0("./", area, "/", tag1))
    
    png(filename = paste0("./", area, "/", tag1, "/decision_impact_", tag1, "_HCRScenario_", HCRscenario, ".png"), width=11, height=8.5, units="in", res=400)
    print(evaluation3)
    dev.off()
    
  }
  
  
  return(list(decisions=decisions, evaluation3 = evaluation3))
}
