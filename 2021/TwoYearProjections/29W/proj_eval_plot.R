proj_eval_plot <- function(object, area, surplus, mu, plot, ref.pts, save){
 
  # this will loop through years to run projections for boxplots
  source("./process_2y_proj_offshore.R")
  results_process <- process_2y_proj(object=object, area=area, surplus=surplus, mu=mu, decisiontable=F)
  
  # tidy up the output
  B.next0 <- do.call(rbind, results_process$B.next0)
  B.next1 <- do.call(rbind, results_process$B.next1)
  B.next2 <- do.call(rbind, results_process$B.next2)
  process <- rbind(B.next0, B.next1, B.next2)

  # summarize it
  all_sum <- process %>%
    dplyr::group_by(year, proj) %>%
    dplyr::summarize(catch = median(catch),
                     mu=unique(mu),
                     min=quantile(Biomass, na.rm = T, c(0.05,0.25, 0.75, 0.95))[1],
                     lower=quantile(Biomass, na.rm = T,  c(0.05,0.25, 0.75, 0.95))[2],
                     med=median(Biomass, na.rm = T),
                     meanB = mean(Biomass, na.rm = T),
                     upper=quantile(Biomass, na.rm = T, c(0.05,0.25, 0.75, 0.95))[3],
                     max=quantile(Biomass, na.rm = T, c(0.05,0.25, 0.75, 0.95))[4]) 
  
  diffs <- all_sum %>%
    dplyr::select(year, med, proj) %>%
    tidyr::pivot_wider(names_from=proj, values_from=med) %>%
    dplyr::rename(actual = `0`,
                  yr1 = `1`,
                  yr2 = `2`) %>%
    dplyr::mutate(actualdiff = actual - yr2,
                  actualprop = actual/yr2,
                  projdiff = yr1 - yr2,
                  projprop = yr1/yr2) %>%
    dplyr::select(year, actualdiff, actualprop, projdiff, projprop) %>%
    tidyr::pivot_longer(cols=c("actualdiff", "actualprop", "projdiff", "projprop"))
  
  
  # boxplot code here
  if(plot == "Standard boxplot"){
    pred.eval <- ggplot() +
      geom_boxplot(data=all_sum[all_sum$year>(min(all_sum$year)+1),],
                   aes(x=as.factor(year), ymin=min, lower=lower, middle=med, upper=upper, ymax=max, fill=as.factor(proj)), stat="identity", position = position_dodge2(preserve = "single", padding=0.2), width=0.7) +
      theme_bw() +
      ylab("Fully recruited biomass estimate (metric tonnes)") +
      xlab("Year") +
      scale_fill_brewer(type = "qual", palette = "Paired", name="Estimate type", direction=-1) +
      theme(panel.grid=element_blank(), text = element_text(size=18))#+
    
    zoom.pred.eval <- ggplot() +
      geom_boxplot(data=all_sum[all_sum$year %in% ((max(all_sum$year)-3):max(all_sum$year)),],
                   aes(x=factor(year), ymin=min, lower=lower, middle=med, upper=upper, ymax=max, fill=as.factor(proj)), stat="identity", position = position_dodge2(preserve = "single", padding=0.2), width=0.7) +
      theme_bw() +
      ylab("Fully recruited biomass estimate (metric tonnes)") +
      xlab("Year") +
      scale_fill_brewer(type = "qual", palette = "Paired", name="Estimate type", direction=-1) +
      theme(panel.grid=element_blank(), text = element_text(size=18))+
      geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(LRP)), linetype="dashed", colour="red", lwd=1) +
      geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(USR)), linetype="dashed", colour="forestgreen", lwd=1)
  }
  
  
  if(plot == "Functional boxplot"){
    
    pred.eval <- ggplot() +
      geom_ribbon(data=all_sum, aes(year, fill=as.factor(proj), ymin=lower, ymax=upper), alpha=0.3) +
      geom_line(data=all_sum, aes(year, colour=as.factor(proj), y=med), lwd=2) +
      geom_line(data=all_sum, aes(year, colour=as.factor(proj), y=meanB), lwd=2, lty=3) +
      theme_bw() +
      theme(panel.grid=element_blank()) +
      ylab("Fully recruited biomass estimate (metric tonnes)") +
      xlab("Year") +
      scale_fill_brewer(type = "qual", palette = "Set2", name="Estimate type") +
      scale_colour_brewer(type = "qual", palette = "Set2", name="Estimate type") +
      theme(panel.grid=element_blank()) +
      annotate(geom="text", x=Inf, y=Inf, hjust=1.05, vjust=1.2, label="* Bands are 50% IQR,\nsolid line are medians,\ndashed lines are means") +
      scale_x_continuous(breaks=unique(all_sum$year)) +
      theme(panel.grid=element_blank(), text = element_text(size=18))
    
    zoom.pred.eval <- ggplot() +
      geom_ribbon(data=all_sum[all_sum$year %in% ((max(all_sum$year)-3):max(all_sum$year)),],
                  aes(year, ymin=lower, ymax=upper,fill=as.factor(proj)), alpha=0.3) +
      geom_line(data=all_sum[all_sum$year %in% ((max(all_sum$year)-3):max(all_sum$year)),], aes(year, colour=as.factor(proj), y=med), lwd=2) +
      geom_line(data=all_sum[all_sum$year %in% ((max(all_sum$year)-3):max(all_sum$year)),], aes(year, colour=as.factor(proj), y=meanB), lwd=2, lty=3) +
      theme_bw() +
      theme(panel.grid=element_blank()) +
      ylab("Fully recruited biomass estimate (metric tonnes)") +
      xlab("Year") +
      scale_fill_brewer(type = "qual", palette = "Set2", name="Estimate type") +
      scale_colour_brewer(type = "qual", palette = "Set2", name="Estimate type") +
      theme(panel.grid=element_blank()) +
      annotate(geom="text", x=Inf, y=Inf, hjust=1.05, vjust=1.2, label="* Bands are 50% IQR,\nsolid line are medians,\ndashed lines are means") +
      scale_x_continuous(breaks=unique(all_sum$year)) +
      geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(LRP)), linetype="dashed", colour="red", lwd=1) +
      geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(USR)), linetype="dashed", colour="forestgreen", lwd=1)
  }
  
  
  evaluation1 <- ggplot() +
    geom_bar(data=diffs[diffs$name %in% c("projdiff"),],
             aes(x=as.factor(year), value, fill=name),
             stat="identity",
             position = position_dodge2(preserve = "single", padding=0.2), width=0.7) +
    scale_fill_brewer(type = "qual", palette = "Paired", name="Estimate type", labels=c("yr1 - yr2"), direction=-1) +
    xlab("Year") +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18)) +
    ylab("Difference in biomass (mt)")
  
  evaluation2 <- ggplot() +
    geom_bar(data=diffs[diffs$name %in% c("projprop"),],
             aes(x=as.factor(year), value, fill=name),
             stat="identity",
             position = position_dodge2(preserve = "single", padding=0.2), width=0.7) +
    scale_fill_brewer(type = "qual", palette = "Paired", name="Estimate type", labels=c(expression(paste(frac("yr1","yr2")))), direction=-1) +
    xlab("Year") +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18)) +
    ylab("Proportional difference in biomass (mt)")
  
  if(save==T){
    
    if(!is.null(surplus)){
      if(surplus==0) tag1 <- "surplus0"
      if(!surplus==0) tag1 <- "surplus_other"
    }
    if(is.null(surplus)) tag1 <- "surplus_lastyear"
    if(!is.na(mu[2]) & is.na(mu[1])) tag2 <- "y2exploit"
    if(is.na(mu[2])) tag2 <- "realized"
    
    tag <- paste0(tag1, "_", tag2)
    
    if(!dir.exists(paste0("./", area, "/", tag1))) dir.create(paste0("./", area, "/", tag1))
    
    png(filename = paste0("./", area, "/", tag1, "/pred_eval_", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(pred.eval)
    dev.off()
    
    
    png(filename = paste0("./", area, "/", tag1, "/zoom_pred_eval_", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(zoom.pred.eval)
    dev.off()
    
    
    png(filename = paste0("./", area, "/", tag1, "/evaluation1_", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(evaluation1)
    dev.off()
    
    
    png(filename = paste0("./", area, "/", tag1, "/evaluation2_", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(evaluation2)
    dev.off()
  }
  
  return(list(results_process=process, all_sum = all_sum, pred.eval=pred.eval, zoom.pred.eval=zoom.pred.eval, evaluation1=evaluation1, evaluation2=evaluation2))
}
