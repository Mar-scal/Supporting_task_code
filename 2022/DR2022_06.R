# code for DR2022_06 (values from GBa Update Figure 4)

require(tidyverse)
require(dplyr)
require(ggplot2)

load("C:/Users/keyserf/Documents/Data/Model/2022/GBa/Results/Final_model_results.Rdata")

TAC <- TACi[["GBa"]]+proj.catch[["GBa"]]

pB<-DD.out[["GBa"]]$sims.list$B.p[,which(DD.out[["GBa"]]$data$C.p==TAC)]
# Subset the data to be limited to 80% of the data.  DK Note I'm still not convinced this is really the way we should do these.
pB.box<-pB[pB>quantile(pB,0.2/2)&pB<quantile(pB,1-0.2/2)]
boxplot.vals <- boxplot(pB.box)$stats

biomass.plt <- data.frame(Year = DD.out[["GBa"]]$data$year, 
                          B = round(DD.out[["GBa"]]$median$B,0),
                          lowerCI95 = round(apply(DD.out[["GBa"]]$sims.list$B, 2, quantile, 1-0.05/2),0),
                          upperCI95 = round(apply(DD.out[["GBa"]]$sims.list$B, 2, quantile, 0.05/2),0))
biomass.plt <- dplyr::full_join(biomass.plt, 
                     data.frame(Year = 2022,
                                pB = round(median(boxplot.vals),0),
                                lowerCI80 = round(boxplot.vals[1],0),
                                lowerCI50 = round(boxplot.vals[2],0),
                                upperCI50 = round(boxplot.vals[4],0),
                                upperCI80 = round(boxplot.vals[5],0)))

png("Y:/Offshore/Data requests/2022/DR2022_06/biomass_check.png", height=4, width=6, res=400, units="in")
ggplot() + geom_line(data=biomass.plt, aes(Year, B)) +
  geom_line(data=biomass.plt, aes(Year, upperCI95), lty="dashed") + 
  geom_line(data=biomass.plt, aes(Year, lowerCI95), lty="dashed") + 
  geom_errorbar(data=biomass.plt, aes(x=Year, ymin=lowerCI80, ymax=upperCI80)) +
  geom_point(data=biomass.plt, aes(Year, B)) +
  geom_point(data=biomass.plt, aes(Year, pB))
dev.off()


names(biomass.plt) <- c("Year", "Biomass (tonnes)",
                        "Upper Credible Interval (95%)",
                        "Lower Credible Interval (95%)",
                        "Forecasted Biomass (tonnes)",
                        "Lower Credible Interval (80%)", 
                        "Lower Credible Interval (50%)", 
                        "Upper Credible Interval (50%)",
                        "Upper Credible Interval (80%)")

biomass.plt[is.na(biomass.plt)] <- "NA"

write.csv(row.names=F, x = biomass.plt, file = "Y:/Offshore/Data requests/2022/DR2022_06/Scallop_GBa_Figure4_FR_2022.csv")
