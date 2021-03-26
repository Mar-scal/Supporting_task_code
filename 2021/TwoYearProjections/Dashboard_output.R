
for(i in c("1A", "1B", "3", "4", "6")){
  load(paste0("./Projection_dashboard_", i, "_func.Rdata"))
  png(filename = paste0("Process_f_boxplot_", i, ".png"), width=12, height=6, res=400, units="in")
  print(decisiondata$pred.eval)
  dev.off()
  
  png(filename = paste0("Simple_f_boxplot_", i, ".png"), width=12, height=6, res=400, units="in")
  print(decisiondata$simple.pred.eval)
  dev.off()
  
  png(filename = paste0("Method_f_comparison_", i, ".png"), width=12, height=6, res=400, units="in")
  print(decisiondata$method.comparison)
  dev.off()
}


for(i in c("1A", "1B", "3", "4", "6")){
  load(paste0("./Projection_dashboard_", i, "_func.Rdata"))
  png(filename = paste0("Absolute_f_boxplot_", i, ".png"), width=12, height=6, res=400, units="in")
  print(decisiondata$diff2)
  dev.off()
  
  png(filename = paste0("Proportional_f_boxplot_", i, ".png"), width=12, height=6, res=400, units="in")
  print(decisiondata$diff1)
  dev.off()
}

require(tidyverse)

load(file = paste0(direct,"/2020/Assessment/Data/Model/SPA", 6, "/Spa", 6, "_2yearPredictionEval_NoProcessError.RData"))

# p<-decisiondata$pred.eval

# dat <- ggplot_build(p)
# 
# dat$plot$data
# 
# layer_data(p)

png(filename = "Standard_boxplot_opposite_levels.png", width=12, height=6, res=400, units="in")
pred.eval
dev.off()

projdata <- NULL

years <- 2011:2018

for(y in years){
  df.current <- data.frame(n=1:length(decisiondata[[paste0(y)]]$decision[[1]]$B.cur),
                           B = decisiondata[[paste0(y)]]$decision[[1]]$B.cur,
                           year = y,
                           proj = "actual",
                           catch= decisiondata[[paste0(y)]]$catch)
  
  df.next <-  data.frame(n=1:length(decisiondata[[paste0(y)]]$decision[[1]]$B.cur),
                         B = decisiondata[[paste0(y)]]$decision[[1]]$B.next[,which(
                           decisiondata[[paste0(y)]]$decision[[1]]$Catch == round(decisiondata[[paste0(y)]]$catch, -1))], 
                         year = y+1, 
                         proj = "1yr",
                         catch= decisiondata[[paste0(y)]]$catch)
  # special case for the 2y projection to grab the B.next for the right catch level
  #if(input$catch2type=="No"){
  df.next2 <- data.frame(n=1:length(decisiondata[[paste0(y)]]$decision[[1]]$B.cur), # just for ease...
                         B = decisiondata[[paste0(y)]]$decision[[2]]$B.next[,which(
                           decisiondata[[paste0(y)]]$decision[[2]]$Catch == round(decisiondata[[paste0(y)]]$catch, -1))],
                         year=y+2, 
                         proj="2yr", 
                         catch= decisiondata[[paste0(y)]]$catch2)#,
  #HCRcatch = max(
  #decisiondata[[paste0(y)]]$decisiontable2$Next.year$Catch[
  #decisiondata[[paste0(y)]]$decisiontable2$Next.year$p.LRP>input$pLRP]))
  #}
  #C decisiondata[[paste0(y)]]$decisiontable2$Next.year$p.LRP>input$pLRP]))
  #}
  print(y)
  projdata <- rbind(projdata, df.current, df.next, df.next2)
  
  #Increment the progress bar, and update the detail text.
  # comment decisiondata for testing
  #incProgress(1/length(years), detail = paste(y, " completed"))
}

#})# comment out for testing
projdata$proj <- factor(projdata$proj, levels=c("2yr", "1yr", "actual")) 

# compare to JS results
B.posterior.out <- do.call("rbind", posteriors.projections)


test <- projdata %>%
  select(n, B, year, proj) %>%
  group_by(proj, year) %>%
  summarise(min=quantile(B, na.rm = T, c(0.05,0.25, 0.75, 0.95))[1],
            lower=quantile(B, na.rm = T,  c(0.05,0.25, 0.75, 0.95))[2],
            med=median(B, na.rm = T),
            meanB = mean(B, na.rm = T),
            upper=quantile(B, na.rm = T, c(0.05,0.25, 0.75, 0.95))[3],
            max=quantile(B, na.rm = T, c(0.05,0.25, 0.75, 0.95))[4]) %>%
  mutate(type="Freya")

test2 <- B.posterior.out %>%
  select(Biomass, year.id, year) %>%
  group_by(year.id, year) %>%
  summarise(min=quantile(Biomass, na.rm = T, c(0.05,0.25, 0.75, 0.95))[1],
            lower=quantile(Biomass, na.rm = T,  c(0.05,0.25, 0.75, 0.95))[2],
            med=median(Biomass, na.rm = T),
            meanB = mean(Biomass, na.rm = T),
            upper=quantile(Biomass, na.rm = T, c(0.05,0.25, 0.75, 0.95))[3],
            max=quantile(Biomass, na.rm = T, c(0.05,0.25, 0.75, 0.95))[4]) %>%
  ungroup() %>%
  mutate(proj = ifelse(year.id==0, "actual", ifelse(year.id==1, "1yr", ifelse(year.id==2, "2yr", NA))),
         type="Jessica") %>%
  select(-year.id)
head(test)
head(test2)


test_all <- full_join(test, test2)

summary(test_all)

test_all$proj <- factor(test_all$proj, levels=c("actual", "1yr", "2yr"))

ggplot() + 
  geom_point(data=test_all, aes(x=factor(year), y=med, colour=type), position = position_dodge(width=0.5)) +
  geom_errorbar(data=test_all, aes(x=factor(year), ymin=lower, ymax=upper, colour=type), position = position_dodge(width=0.5))+
  theme_bw() +
  ylab("Fully recruited biomass estimate (metric tonnes)") + 
  xlab("Year") + 
  scale_fill_brewer(type = "qual", palette = "Paired", name="Estimate type", direction = -1) +
  theme(panel.grid=element_blank(), text = element_text(size=18)) +
  facet_wrap(~proj)
