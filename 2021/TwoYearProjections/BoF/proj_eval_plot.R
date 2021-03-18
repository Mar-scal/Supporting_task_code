proj_eval_plot <- function(object, area, surplus, mu, ref.pts, save){
 
  require(patchwork)
  # this will loop through years to run projections for boxplots
  if(area %in% c("GBa", "BBn")) folder <- "Offshore"
  if(area %in% c("1A", "1B", "3", "4", "6")) folder <- "BoF"
  if(area %in% c("29A", "29B", "29C", "29D")) folder <- "29W"
  source(paste0("./", folder, "/process_2y_proj.R"))
  results_process <- process_2y_proj(object=object, area=area, surplus=surplus, mu=mu, decisiontable=F)
  
  options(scipen = 999)
  
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
  
  diffs <- out[[3]]$realized$all_sum %>%
    dplyr::select(year, med, proj) %>%
    tidyr::pivot_wider(names_from=proj, values_from=med) %>%
    dplyr::rename(actual = `0`,
                  yr1 = `1`,
                  yr2 = `2`) %>%
    dplyr::mutate(actualdiff = yr2 - actual,
                  actualprop = yr2/actual - 1,
                  projdiff = yr2 - yr1,
                  projprop = yr2/yr1 - 1) %>%
    dplyr::select(year, actualdiff, actualprop, projdiff, projprop) %>%
    tidyr::pivot_longer(cols=c("actualdiff", "actualprop", "projdiff", "projprop"))
  
  
  # labelling:
  if(!is.null(surplus)){
    if(surplus==0) tag1 <- "surplus0"
    if(!surplus==0) tag1 <- "surplus_other"
  }
  if(is.null(surplus)) tag1 <- "surplus_lastyear"
  if(!is.na(mu[2]) & is.na(mu[1])) tag2 <- "y2exploit"
  if(is.na(mu[2])) tag2 <- "realized"
  
  # limits:
  if(area=="GBa") {
    break1 <- seq(0,140000, 20000)
    break2 <- seq(0,140000, 20000)
    break3 <- seq(-30000,30000, 5000)
    break4 <- seq(-1.4, 1.4, 0.7)
  }
  
  if(area =="BBn") {
    break1 <- seq(0,30000, 5000)
    break2 <- seq(0,10000, 2000)
    break3 <- seq(-5000,5000, 1000)
    break4 <- seq(-0.8, 0.8, 0.4)
  }
  
  if(!area %in% c("BBn", "GBa", "1B", "3", "4", "6")) {
    break1 <- seq(0,8000, 2000)
    break2 <- seq(0,8000, 2000)
    break3 <- seq(-2500,2500, 500)
    break4 <- seq(-0.8, 0.8, 0.4)
  }
 
  if(area =="1B") {
    break1 <- seq(0,10000, 2000)
    break2 <- seq(0,10000, 2000)
    break3 <- seq(-2500,2500, 500)
    break4 <- seq(-0.8, 0.8, 0.4)
  }
  
  if(area %in% c("3", "4")) {
    break1 <- seq(0,6000, 2000)
    break2 <- seq(0,6000, 2000)
    break3 <- seq(-1500,1500, 500)
    break4 <- seq(-0.8, 0.8, 0.4)
  }
  
  if(area =="6") {
    break1 <- seq(0,4000, 1000)
    break2 <- seq(0,4000, 1000)
    break3 <- seq(-1500,1500, 500)
    break4 <- seq(-0.8, 0.8, 0.4)
  }
  
  
  # boxplot code here
  pred.eval <- ggplot() +
    geom_boxplot(data=all_sum[all_sum$year>(min(all_sum$year)+1),],
                 aes(x=as.factor(year), ymin=min, lower=lower, middle=med, upper=upper, ymax=max, fill=as.factor(proj)), stat="identity", position = position_dodge2(preserve = "single", padding=0.2), width=0.7) +
    theme_bw() +
    ylab("Commercial biomass estimate (metric tonnes)") +
    xlab("Year") +
    scale_fill_brewer(type = "qual", palette = "Paired", name=NULL, direction=-1, labels=c("actual", "year 1", "year 2")) +
    theme(panel.grid=element_blank(), text = element_text(size=18))+
    ggtitle(paste0(area), subtitle=tag1) +
    scale_y_continuous(breaks=break1, limits=c(min(break1), max(break1)))
  
  zoom.pred.eval <- ggplot() +
    geom_boxplot(data=all_sum[all_sum$year %in% ((max(all_sum$year)-3):max(all_sum$year)),],
                 aes(x=factor(year), ymin=min, lower=lower, middle=med, upper=upper, ymax=max, fill=as.factor(proj)), stat="identity", position = position_dodge2(preserve = "single", padding=0.2), width=0.7) +
    theme_bw() +
    ylab("Commercial biomass estimate (metric tonnes)") +
    xlab("Year") +
    scale_fill_brewer(type = "qual", palette = "Paired", name=NULL, direction=-1, labels=c("actual", "year 1", "year 2")) +
    theme(panel.grid=element_blank(), text = element_text(size=18))+
    geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(LRP)), linetype="dashed", colour="red", lwd=1) +
    geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(USR)), linetype="dashed", colour="forestgreen", lwd=1)+
    ggtitle(paste0(area), subtitle=tag1)+
    scale_y_continuous(breaks=break2, limits=c(min(break2), max(break2)))
  
  
  pred.eval.F <- ggplot() +
    geom_ribbon(data=all_sum, aes(year, fill=as.factor(proj), ymin=lower, ymax=upper), alpha=0.3) +
    geom_line(data=all_sum, aes(year, colour=as.factor(proj), y=med), lwd=2) +
    geom_line(data=all_sum, aes(year, colour=as.factor(proj), y=meanB), lwd=2, lty=3) +
    theme_bw() +
    theme(panel.grid=element_blank()) +
    ylab("Commercial biomass estimate (metric tonnes)") +
    xlab("Year") +
    scale_fill_brewer(type = "qual", palette = "Set2", name=NULL, labels=c("actual", "year 1", "year 2")) +
    scale_colour_brewer(type = "qual", palette = "Set2", name=NULL, labels=c("actual", "year 1", "year 2")) +
    theme(panel.grid=element_blank()) +
    annotate(geom="text", x=Inf, y=Inf, hjust=1.05, vjust=1.2, label="* Bands are 50% IQR,\nsolid line are medians,\ndashed lines are means") +
    scale_x_continuous(breaks=unique(all_sum$year)) +
    theme(panel.grid=element_blank(), text = element_text(size=18))+
    ggtitle(paste0(area), subtitle=tag1)+
    scale_y_continuous(breaks=break1, limits=c(min(break1), max(break1)))
  
  zoom.pred.eval.F <- ggplot() +
    geom_ribbon(data=all_sum[all_sum$year %in% ((max(all_sum$year)-3):max(all_sum$year)),],
                aes(year, ymin=lower, ymax=upper,fill=as.factor(proj)), alpha=0.3) +
    geom_line(data=all_sum[all_sum$year %in% ((max(all_sum$year)-3):max(all_sum$year)),], aes(year, colour=as.factor(proj), y=med), lwd=2) +
    geom_line(data=all_sum[all_sum$year %in% ((max(all_sum$year)-3):max(all_sum$year)),], aes(year, colour=as.factor(proj), y=meanB), lwd=2, lty=3) +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18))+
    ylab("Commercial biomass estimate (metric tonnes)") +
    xlab("Year") +
    scale_fill_brewer(type = "qual", palette = "Set2", name=NULL, labels=c("actual", "year 1", "year 2")) +
    scale_colour_brewer(type = "qual", palette = "Set2", name=NULL, labels=c("actual", "year 1", "year 2")) +
    theme(panel.grid=element_blank()) +
    annotate(geom="text", x=Inf, y=Inf, hjust=1.05, vjust=1.2, label="* Bands are 50% IQR,\nsolid line are medians,\ndashed lines are means") +
    scale_x_continuous(breaks=unique(all_sum$year)) +
    geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(LRP)), linetype="dashed", colour="red", lwd=1) +
    geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(USR)), linetype="dashed", colour="forestgreen", lwd=1)+
    ggtitle(paste0(area), subtitle=tag1)+
    scale_y_continuous(breaks=break2, limits=c(min(break2), max(break2)))
  
  
  evaluation1 <- ggplot() +
    geom_bar(data=diffs[diffs$name %in% c("projdiff") & !is.na(diffs$value),],
             aes(x=as.factor(year), value, group=name),
             stat="identity",
             width=0.7,
             fill="grey", colour="black") +
    geom_hline(data = diffs[diffs$name %in% c("projdiff") & !is.na(diffs$value),], aes(yintercept=0))+
    xlab("Year") +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18)) +
    ylab("Difference in biomass (mt)") +
    ggtitle(paste0(area, " - ", tag1), subtitle=expression(y[2]-y[1]))+
    scale_y_continuous(breaks=break3, limits=c(min(break3), max(break3)))
  
  evaluation2 <- ggplot() +
    geom_bar(data=diffs[diffs$name %in% c("projprop") & !is.na(diffs$value),],
             aes(x=as.factor(year), value, group=name),
             stat="identity",
             width=0.7,
             fill="grey", colour="black") +
    geom_hline(data = diffs[diffs$name %in% c("projdiff") & !is.na(diffs$value),], aes(yintercept=0))+
    xlab("Year") +
    theme_bw() +
    theme(panel.grid=element_blank(), text = element_text(size=18)) +
    ylab("Proportional difference in biomass (mt)")+
    ggtitle(paste0(area, " - ", tag1), subtitle=expression((y[2]/y[1])-1))+
    scale_y_continuous(breaks=break4, limits=c(min(break4), max(break4)))
  
  
  if(save==T){
    
    tag <- paste0(tag1, "_", tag2)
    
    if(!dir.exists(paste0("./", folder, "/", area, "/", tag1))) dir.create(paste0("./", folder, "/", area, "/", tag1))
    
    png(filename = paste0("./", folder, "/", area, "/", tag1, "/pred_eval_", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(pred.eval)
    dev.off()
    
    png(filename = paste0("./", folder, "/", area, "/", tag1, "/pred_eval_F", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(pred.eval.F)
    dev.off()
    
    png(filename = paste0("./", folder, "/", area, "/", tag1, "/zoom_pred_eval_F", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(zoom.pred.eval.F)
    dev.off()
    
    
    png(filename = paste0("./", folder, "/", area, "/", tag1, "/evaluation1_", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(evaluation1)
    dev.off()
    
    
    png(filename = paste0("./", folder, "/", area, "/", tag1, "/evaluation2_", tag, ".png"), width=11, height=8.5, units="in", res=400)
    print(evaluation2)
    dev.off()
  }
  
  return(list(results_process=process, all_sum = all_sum, pred.eval=pred.eval, zoom.pred.eval=zoom.pred.eval, pred.eval=pred.eval.F, zoom.pred.eval=zoom.pred.eval.F, evaluation1=evaluation1, evaluation2=evaluation2))
}
