load("summary2.Rdata")
highlights_Oct <- highlights

load("summary2_Sep.Rdata")
highlights_Sep <- highlights


highlights_Oct[highlights_Oct$bank=="GBa",][1:10,]
highlights_Sep[highlights_Sep$bank=="GBa",][1:10,]

load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2024/Survey_summary_output/testing_results_summer2024_Oct2.Rdata")
Oct <- all.surv.dat[all.surv.dat$year==2024 & all.surv.dat$bank=="GBa" & all.surv.dat$tow %in% 191:193 & all.surv.dat$state=="live",]
Oct_model <- survey.obj$GBa$model.dat[survey.obj$GBa$model.dat$year==2024,]


load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2024/Survey_summary_output/testing_results_summer2024.Rdata")
Sep <- all.surv.dat[all.surv.dat$year==2024 & all.surv.dat$bank=="GBa" & all.surv.dat$tow %in% 191:193 & all.surv.dat$state=="live",]
Sep_model <- survey.obj$GBa$model.dat[survey.obj$GBa$model.dat$year==2024,]

Oct_model
Sep_model
