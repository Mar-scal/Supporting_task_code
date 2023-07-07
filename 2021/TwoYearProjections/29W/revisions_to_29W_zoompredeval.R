# Code to adapt RData ggplot objects from R 3.x to R 4.x


require(patchwork)
require(cowplot)
require(ggplot2)

folder <- "29W"
area <- "29D"


load(paste0("./", folder, "/", area, "/twoyearprojections.RData"))
# OR
#load(paste0("Y:/Projects/CSAS/2y_Projection_RAP/data/", folder, "/", area, "/twoyearprojections.RData"))

# out[[1]]$realized$zoom.pred.eval$theme <- #ggplot2::theme_bw() + 
#   theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
# out[[2]]$realized$zoom.pred.eval$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
# out[[3]]$realized$zoom.pred.eval$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")

# for 29W, I had to remake them outside of the normal code because of a memory allocation issue.
if(area %in% c("29A", "29B", "29C", "29D")) {
  break1 <- seq(0,750, 250)
  break2 <- seq(0,30, 5)
  break3 <- seq(-200,200, 50)
  break4 <- seq(-3, 3, 1)
}

if(area =="29B"){
  break2 <- seq(0,20,5)
  break4 <- seq(-3.5, 3.5, 1.75)
}
if(area =="29A"){
  break2 <- seq(0,10,2)
}

ref.pts <- data.frame(area = c("1A", "1B", "3", "4", "6", "29A", "29B", "29C", "29D", "GBa", "BBn"),
                 LRP = c(480, 880, 600, 530, NA, NA, 1.12, 1.41, 1.3, 7137, NA),
                 USR = c(1000, 1800, 1000, 750, NA, NA, 2.24, 2.82, 2.6, 13284, NA))

Dmsy <- data.frame(area=c("29A", "29B", "29C", "29D"),
                   Dmsy=c(NA, 3.75, 4.68, 4.32))

zoom.pred.eval <- NULL
for(i in 1:3){
  all_sum <- out[[i]]$realized$all_sum
  densities <- out[[i]]$realized$densities
  diffs <- out[[i]]$realized$diffs
  process <- out[[i]]$realized$results_process
  
  if(i==1) tag1 <- "surplus0"
  if(i==2) tag1 <- "surplus_other"
  if(i==3) tag1 <- "surplus_lastyear"
  
  zoom.pred.eval[[i]] <- ggplot() +
    geom_boxplot(data=densities[densities$year %in% ((max(densities$year)-3):max(densities$year)),],
                 aes(x=factor(year), ymin=min, lower=lower, middle=med, upper=upper, ymax=max, fill=as.factor(proj)), stat="identity", position = position_dodge2(preserve = "single", padding=0.2), width=0.7) +
    theme_bw() +
    ylab("Commercial biomass density (metric tonnes/square kilometre)") +
    xlab("Year") +
    scale_fill_brewer(type = "qual", palette = "Paired", name=NULL, direction=-1, labels=c("actual", "year 1", "year 2")) +
    theme(panel.grid=element_blank(), text = element_text(size=18))+
    geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(LRP)), linetype="dashed", colour="red", lwd=1) +
    geom_hline(data=ref.pts[ref.pts$area == area,], aes(yintercept=as.numeric(USR)), linetype="dashed", colour="forestgreen", lwd=1)+
    ggtitle(paste0(area), subtitle=tag1)+
    scale_y_continuous(breaks=break2, limits=c(min(break2), max(break2))) +
    geom_hline(data=Dmsy[Dmsy$area==area,], aes(yintercept=Dmsy), lwd=1, lty="dotted")
}

# This simply shows the biomass projections for the most recent year. 
png(paste0("./figures/", folder, "/", area, "/zoom_pred_eval.png"), height=6,  width=22, res=300, units="in")
print(plot_grid(zoom.pred.eval[[1]] + theme(legend.position = c(0.5, 0.95), legend.direction="horizontal") + ylab("Commercial biomass density\n(metric tonnes/square kilometre)") + ggtitle(NULL, subtitle=NULL) +
                  geom_hline(data=Dmsy[Dmsy$area==area,], aes(yintercept=Dmsy), lwd=1, lty="dotted"),  
                zoom.pred.eval[[2]]+ ylab(NULL) + theme(legend.position = c(0.5, 0.95), legend.direction="horizontal") + ggtitle(NULL, subtitle=NULL) +
                  geom_hline(data=Dmsy[Dmsy$area==area,], aes(yintercept=Dmsy), lwd=1, lty="dotted"),  
                zoom.pred.eval[[3]]+ ylab(NULL) + theme(legend.position = c(0.5, 0.95), legend.direction="horizontal") + ggtitle(NULL, subtitle=NULL) +
                  geom_hline(data=Dmsy[Dmsy$area==area,], aes(yintercept=Dmsy), lwd=1, lty="dotted"), 
                ncol=3, align="v"))
dev.off()


out[[1]]$realized$zoom.pred.eval <- zoom.pred.eval[[1]]
out[[2]]$realized$zoom.pred.eval <- zoom.pred.eval[[2]]
out[[3]]$realized$zoom.pred.eval <- zoom.pred.eval[[3]]
# hop over to two_year_projections_french_figures to make the french version


out[[1]]$realized$evaluation1$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
out[[2]]$realized$evaluation1$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
out[[3]]$realized$evaluation1$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")

out[[1]]$realized$evaluation2$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
out[[2]]$realized$evaluation2$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
out[[3]]$realized$evaluation2$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")

out[[1]]$impact_HCR1$evaluation3$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
out[[2]]$impact_HCR1$evaluation3$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
out[[3]]$impact_HCR1$evaluation3$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")

out[[1]]$impact_HCR1$evaluation4$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
out[[2]]$impact_HCR1$evaluation4$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")
out[[3]]$impact_HCR1$evaluation4$theme <- ggplot2::theme_bw() + theme(panel.grid=element_blank(), legend.position = c(0.5, 0.95), legend.direction="horizontal")

# A - 125, 1; 15, 1
# B - 100, 3.5; 30, 1
# C - 150, 2.5; 20, 1.5
# D - 125, 1; 30, 1

french <- T

require(rosettafish)
rosetta_terms <- as.data.frame(readr::read_csv("terms.csv"))
if(french==F) {
  catch_diff <- "Difference in catch potential (mt)"
  prop_catch_diff <- "Proportional difference in catch potential"
}
if(french==T) {
  catch_diff <- "Différence des prises\npotentielles (tonnes)"
  prop_catch_diff <- "Différence proportionelle\ndes prises potentielles"
}

if(french==T) folder <- paste0("french/", folder)

png(paste0("./figures/", folder, "/", area, "/biomass_diff_eval.png"), height=12/1.5, width=22/1.5, res=300, units="in")
print(plot_grid(out[[1]]$realized$evaluation1 + 
                  ggtitle(NULL, subtitle=NULL) +
                  ylab(en2fr("Difference in biomass (mt)",french,custom=rosetta_terms,case = 'sentence'))+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) + ylim(-125, 125), 
                out[[2]]$realized$evaluation1+ 
                  ylab(NULL) + 
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) + ylim(-125, 125), 
                out[[3]]$realized$evaluation1+ 
                  ylab(NULL) + 
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) + ylim(-125, 125),
                out[[1]]$realized$evaluation2 + 
                  ggtitle(NULL, subtitle=NULL) + 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence'))+ 
                  ylab(en2fr("Proportional difference in biomass",french,custom=rosetta_terms,case = 'sentence'))+ ylim(-1,1), 
                out[[2]]$realized$evaluation2+ 
                  ylab(NULL) +
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence'))+ ylim(-1,1), 
                out[[3]]$realized$evaluation2+ 
                  ylab(NULL) + 
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence'))+ ylim(-1,1), 
                ncol=3, align="v"))
dev.off()

png(paste0("./figures/", folder, "/", area, "/decision_impact_HCR1.png"), height=12/1.5, width=22/1.5, res=300, units="in")
print(plot_grid(out[[1]]$impact_HCR1$evaluation3 + 
                  ggtitle(NULL, subtitle=NULL) +
                  ylab(catch_diff) +
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) + ylim(-30, 30), 
                out[[2]]$impact_HCR1$evaluation3 + 
                  ylab(NULL) + 
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) + ylim(-30, 30), 
                out[[3]]$impact_HCR1$evaluation3 + 
                  ylab(NULL) + 
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) + ylim(-30, 30),
                out[[1]]$impact_HCR1$evaluation4 + 
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) +
                  ylab(prop_catch_diff) + ylim(-1, 1), 
                out[[2]]$impact_HCR1$evaluation4 + 
                  ylab(NULL) + 
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) + ylim(-1, 1), 
                out[[3]]$impact_HCR1$evaluation4 + 
                  ylab(NULL) + 
                  ggtitle(NULL, subtitle=NULL)+ 
                  xlab(en2fr("Year",french,custom=rosetta_terms,case = 'sentence')) + ylim(-1, 1),
                ncol=3, align="v"))
dev.off()

