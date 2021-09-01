
# load data and source fn
source("C:/Documents/Assessment_fns/Survey_and_OSAC/survey.dat.R")
load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.RData")

# prep strata info
surv.info <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/survey_information_2021-07-09.csv")
strata.areas <- subset(surv.info,select =c("Strata_ID","towable_area","startyear"))
strata.areas <- strata.areas[strata.areas$Strata_ID%in% 201:205,]

res <- survey.dat(surv.Rand[["BBn"]], RS=85, CS=95, 
           bk="BBn", areas=strata.areas, mw.par="CF",user.bins = NULL)	

# new strata info
strata.areas$towable_area <- c(256126.06,107809.8,107112.07,71551.33,52280.88)

res2 <- survey.dat(surv.Rand[["BBn"]], RS=85, CS=95, 
                   bk="BBn", areas=strata.areas, mw.par="CF",user.bins = NULL)	

p1 <- ggplot() + geom_point(data=res$bankpertow, aes(year, I)) + 
  geom_line(data=res$bankpertow, aes(year, I)) + 
  geom_errorbar(data=res$bankpertow, aes(year, ymin=I-I.se, ymax=I+I.se), width=0) +
  geom_point(data=res2$bankpertow, aes(year, I), colour="red") + 
  geom_line(data=res2$bankpertow, aes(year, I), colour="red") + 
  geom_errorbar(data=res2$bankpertow, aes(year, ymin=I-I.se, ymax=I+I.se), width=0, colour="red") +
  xlim(2013, 2020)

p2 <- ggplot() + geom_point(data=res$bankpertow, aes(year, IR)) + 
  geom_line(data=res$bankpertow, aes(year, IR)) + 
  geom_errorbar(data=res$bankpertow, aes(year, ymin=IR-IR.se, ymax=IR+IR.se), width=0) +
  geom_point(data=res2$bankpertow, aes(year, IR), colour="red") + 
  geom_line(data=res2$bankpertow, aes(year, IR), colour="red") + 
  geom_errorbar(data=res2$bankpertow, aes(year, ymin=IR-IR.se, ymax=IR+IR.se), width=0, colour="red") +
  xlim(2013, 2020)
  
p3 <- ggplot() + geom_point(data=res$bankpertow, aes(year, IPR)) + 
  geom_line(data=res$bankpertow, aes(year, IPR)) + 
  geom_errorbar(data=res$bankpertow, aes(year, ymin=IPR-IPR.se, ymax=IPR+IPR.se), width=0) +
  geom_point(data=res2$bankpertow, aes(year, IPR), colour="red") + 
  geom_line(data=res2$bankpertow, aes(year, IPR), colour="red") + 
  geom_errorbar(data=res2$bankpertow, aes(year, ymin=IPR-IPR.se, ymax=IPR+IPR.se), width=0, colour="red") +
  xlim(2013, 2020)

p4 <- ggplot() + geom_point(data=res$bankpertow, aes(year, N)) + 
  geom_line(data=res$bankpertow, aes(year, N)) + 
  geom_errorbar(data=res$bankpertow, aes(year, ymin=N-N.se, ymax=N+N.se), width=0) +
  geom_point(data=res2$bankpertow, aes(year, N), colour="red") + 
  geom_line(data=res2$bankpertow, aes(year, N), colour="red") + 
  geom_errorbar(data=res2$bankpertow, aes(year, ymin=N-N.se, ymax=N+N.se), width=0, colour="red") +
  xlim(2013, 2020)

p5 <- ggplot() + geom_point(data=res$bankpertow, aes(year, NR)) + 
  geom_line(data=res$bankpertow, aes(year, NR)) + 
  geom_errorbar(data=res$bankpertow, aes(year, ymin=NR-NR.se, ymax=NR+NR.se), width=0) +
  geom_point(data=res2$bankpertow, aes(year, NR), colour="red") + 
  geom_line(data=res2$bankpertow, aes(year, NR), colour="red") + 
  geom_errorbar(data=res2$bankpertow, aes(year, ymin=NR-NR.se, ymax=NR+NR.se), width=0, colour="red") +
  xlim(2013, 2020)

p6 <- ggplot() + geom_point(data=res$bankpertow, aes(year, NPR)) + 
  geom_line(data=res$bankpertow, aes(year, NPR)) + 
  geom_errorbar(data=res$bankpertow, aes(year, ymin=NPR-NPR.se, ymax=NPR+NPR.se), width=0) +
  geom_point(data=res2$bankpertow, aes(year, NPR), colour="red") + 
  geom_line(data=res2$bankpertow, aes(year, NPR), colour="red") + 
  geom_errorbar(data=res2$bankpertow, aes(year, ymin=NPR-NPR.se, ymax=NPR+NPR.se), width=0, colour="red") +
  xlim(2013, 2020)

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow=2)



p1 <- ggplot() + geom_point(data=res$model.dat, aes(year, I)) + 
  geom_line(data=res$model.dat, aes(year, I)) + 
  geom_point(data=res2$model.dat, aes(year, I), colour="red") + 
  geom_line(data=res2$model.dat, aes(year, I), colour="red") + 
  xlim(2013, 2020)

p2 <- ggplot() + geom_point(data=res$model.dat, aes(year, IR)) + 
  geom_line(data=res$model.dat, aes(year, IR)) + 
  geom_point(data=res2$model.dat, aes(year, IR), colour="red") + 
  geom_line(data=res2$model.dat, aes(year, IR), colour="red") + 
  xlim(2013, 2020)

p3 <- ggplot() + geom_point(data=res$model.dat, aes(year, IPR)) + 
  geom_line(data=res$model.dat, aes(year, IPR)) + 
  geom_point(data=res2$model.dat, aes(year, IPR), colour="red") + 
  geom_line(data=res2$model.dat, aes(year, IPR), colour="red") + 
  xlim(2013, 2020)

p4 <- ggplot() + geom_point(data=res$model.dat, aes(year, N)) + 
  geom_line(data=res$model.dat, aes(year, N)) + 
  geom_point(data=res2$model.dat, aes(year, N), colour="red") + 
  geom_line(data=res2$model.dat, aes(year, N), colour="red") + 
  xlim(2013, 2020)

p5 <- ggplot() + geom_point(data=res$model.dat, aes(year, NR)) + 
  geom_line(data=res$model.dat, aes(year, NR)) + 
  geom_point(data=res2$model.dat, aes(year, NR), colour="red") + 
  geom_line(data=res2$model.dat, aes(year, NR), colour="red") + 
  xlim(2013, 2020)

p6 <- ggplot() + geom_point(data=res$model.dat, aes(year, NPR)) + 
  geom_line(data=res$model.dat, aes(year, NPR)) + 
  geom_point(data=res2$model.dat, aes(year, NPR), colour="red") + 
  geom_line(data=res2$model.dat, aes(year, NPR), colour="red") + 
  xlim(2013, 2020)

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow=2)

