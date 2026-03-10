# projection sensitivity
direct_out <- "C:/Users/keyserf/Documents/"
b <- "GBa"
# mod.res <- get(load(paste0(direct_out, "Data/Model/",2026,"/",b,"/Results/Model_testing_results.RData",sep=""))[1])
# area <- "GBa"
# mod.res <- list(data=mod.res[[area]], summary = DD.out[[area]]$summary, sims.matrix=DD.out[[area]]$sims.list)
# mod.res$Years <- mod.res$data$year
# mod.res$sims.matrix <- mod.res$sims.matrix[!names(mod.res$sims.matrix) %in% names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) == 21))]
# standard <- mod.res$sims.matrix[names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) > 1))]
# newnames <- paste0(rep(names(standard), each=mod.res$data$NY), "[", 1:mod.res$data$NY, "]")
# standard <- as.data.frame(do.call(cbind, standard))
# names(standard) <- newnames
# single <- mod.res$sims.matrix[names(which(map(mod.res$sims.matrix, function(x) dim(x)[2]) == 1))]
# mod.res$sims.matrix <- cbind(standard, data.frame(K = single$K, S=single$S, deviance=single$deviance, ikappa.rho2 = single$ikappa.rho2,
#                                                   ikappa.tau2 = single$ikappa.tau2, logK = single$logK, q = single$q, qU = single$qU, sigma=single$sigma))
# 
# mod.res$data$C[length(mod.res$data$C)+1] <- proj.catch$GBa
# 
# source(paste0("C:/Users/keyserf/Documents/Github/Supporting_task_code/2025/process_2y_proj_m.R"))
# 
# mvals <- seq(0, 0.8, by=0.1)
# 
# null <- process_2y_proj(object=mod.res, area=area, m=NULL, mu=c(NA, NA), decisiontable=F)
# decision1 <- map(mvals, function(x) process_2y_proj(object=mod.res, area=area, m=c(x, x), mu=c(NA, NA), decisiontable=F))
# 
# # tidy up the output
# decision.df <- NULL
# for(i in 1:length(decision1)){
#   B.next0 <- do.call(rbind, decision1[[i]]$B.next0)
#   B.next1 <- do.call(rbind, decision1[[i]]$B.next1)
#   process <- rbind(B.next0, B.next1)
#   decision.df <- rbind(decision.df, process)
# }
# 
# null0 <- do.call(rbind, null$B.next0)
# null1 <- do.call(rbind, null$B.next1)
# decision.null <- rbind(null0, null1)
# decision.null$m <- NA
# decision.null$mR <- NA
# decision.df <- rbind(decision.df, decision.null)
# 
# 
# summary(null1[null1$year==2026,]$Biomass)
# 
# summary(DD.out$GBa$sims.list$B.p)
# 
# decisiontable <- function(object, proj, year, LRP, USR){
#   object <- object[object$proj==proj & object$year == year,]
#   return(
#     object %>%
#       dplyr::group_by(year, proj, m) %>%
#       dplyr::summarize(biomass=median(Biomass),
#                        catch=median(catch),
#                        adj.catch = median(catch-proj.catch[[1]]), # proj.catch is straight from the model object!
#                        mu=median(mu),
#                        B.change=median(B.change),
#                        pB0 = sum(pB0)/length(pB0),
#                        p.LRP = round(sum(Biomass > LRP)/length(Biomass), 3),
#                        p.USR = round(sum(Biomass > USR)/length(Biomass), 3))
#   )
# }
# 
# decision.1 <- map_df(2:length(unique(decision.df$year)), function(x) 
#   decisiontable(decision.df, proj=1, year=unique(decision.df$year)[x], LRP=4649, USR=12399))
# 
# ggplot() + geom_point(data=decision.1, aes(year, biomass)) + 
#   geom_hline(data=decision.1, aes(yintercept=4649), linetype="dashed") +
#   geom_hline(data=decision.1, aes(yintercept=12399), linetype="dashed") +
#   facet_wrap(~m)
# 
# ggplot() + geom_boxplot(data=decision.df, aes(year, Biomass, group=year), outliers = F) + 
#   geom_hline(data=decision.df, aes(yintercept=4649), linetype="dashed") +
#   geom_hline(data=decision.df, aes(yintercept=12399), linetype="dashed") +
#   facet_wrap(~m)
# 
# 
# pB.box <- decision.df %>% 
#   dplyr::group_by(year, m) %>%
#   dplyr::summarize(q1 = quantile(Biomass, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))[[1]],
#             q2 = quantile(Biomass, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))[[2]], 
#             q3 = quantile(Biomass, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))[[3]],
#             q4 = quantile(Biomass, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))[[4]],
#             q5 = quantile(Biomass, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))[[5]])
# 
# ggplot() + geom_boxplot(data=pB.box, stat="identity", aes(year, group=year, ymin=q1, lower=q2, middle=q3, upper=q4, ymax=q5), outliers = F) + 
#   geom_hline(data=decision.df, aes(yintercept=4649), linetype="dashed") +
#   geom_hline(data=decision.df, aes(yintercept=12399), linetype="dashed") +
#   facet_wrap(~m) +
#   scale_x_continuous(breaks=2016:2026)
# 

source("C:/Users/keyserf/Documents/Github/Supporting_task_code/2025/projections_M.r")
source("C:/Users/keyserf/Documents/Github/Assessment_fns/Model/decision.r")
load(paste0(direct_out, "Data/Model/",2026,"/",b,"/Results/Model_testing_results.RData",sep=""))

#2023
median(DD.out$GBa$sims.list$m[,38])
median(DD.out$GBa$sims.list$mR[,38])
D.tab.test_38 <- NULL
for(i in 1:10) {
  test <- projections(DD.out$GBa, C.p = seq(min(proj$GBa), median(proj$GBa),100), my=38)
  D.tab_38 <- decision(test,"GBa", mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]], yr=2025)
  D.tab_38$run <- i
  D.tab.test_38 <- rbind(D.tab.test_38, D.tab_38)
}
ggplot() + geom_point(data=D.tab.test_38, aes(Catch, mu))
ggplot() + geom_point(data=D.tab.test_38, aes(Catch, Pr.above.LRP))

median(DD.out$GBa$sims.list$m[,39])
median(DD.out$GBa$sims.list$mR[,39])

#2025
median(DD.out$GBa$sims.list$m[,40])
median(DD.out$GBa$sims.list$mR[,40])
D.tab.test_40 <- NULL
for(i in 1:10) {
  test <- projections(DD.out$GBa, C.p = seq(min(proj$GBa), median(proj$GBa),100), my=40)
  D.tab_40 <- decision(test,"GBa", mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]], yr=2025)
  D.tab_40$run <- i
  D.tab.test_40 <- rbind(D.tab.test_40, D.tab_40)
}

mup <- ggplot() + geom_point(data=D.tab.test_40[D.tab.test_40$Catch<3000,], aes(Catch, mu), shape=1) +
  geom_point(data=D.tab.test_38[D.tab.test_38$Catch<3000,], aes(Catch, mu), shape=2) +
  ggtitle("2025 M (circles), 2023 M (triangles). Ran 10 times each.")
mp <- ggplot() + geom_point(data=D.tab.test_40[D.tab.test_40$Catch<3000,], aes(Catch, Pr.above.LRP), shape=1) +
  geom_point(data=D.tab.test_38[D.tab.test_38$Catch<3000,], aes(Catch, Pr.above.LRP), shape=2) +
  ggtitle("2025 M (circles), 2023 M (triangles). Ran 10 times each.")

require(patchwork)
mup/mp
png(filename = "C:/Users/keyserf/Documents/2026/Updates/GBa/Figures_and_tables/m_test_output.png",
    height=10,width=8,units="in", res=400)
print(mup/mp) 
dev.off()

out_40 <- D.tab.test_40[D.tab.test_40$Catch <3000,] %>%
  dplyr::group_by(Catch) %>%
  dplyr::summarize(median(mu), 
            median(Pr.above.LRP))
out_40$m <- median(DD.out$GBa$sims.list$m[,40])
out_40$mR <- median(DD.out$GBa$sims.list$mR[,40])
write.csv(x = out_40, file = "C:/Users/keyserf/Documents/2026/Updates/GBa/Figures_and_tables/alternate_decision_table_my40.csv")

out_38 <- D.tab.test_38[D.tab.test_38$Catch <3000,] %>%
  dplyr::group_by(Catch) %>%
  dplyr::summarize(median(mu), 
                   median(Pr.above.LRP))
out_38$m <- median(DD.out$GBa$sims.list$m[,38])
out_38$mR <- median(DD.out$GBa$sims.list$mR[,38])
write.csv(x = out_38, file = "C:/Users/keyserf/Documents/2026/Updates/GBa/Figures_and_tables/alternate_decision_table_my38.csv")

# growth and mort
D.tab.test_38_gm <- NULL
for(i in 1:10) {
  test <- projections(DD.out$GBa, C.p = seq(min(proj$GBa), median(proj$GBa),100), my=38, gy=38)
  D.tab_38 <- decision(test,"GBa", mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]], yr=2025)
  D.tab_38$run <- i
  D.tab.test_38_gm <- rbind(D.tab.test_38_gm, D.tab_38)
}

ggplot() + geom_point(data=D.tab.test_38_gm, aes(Catch, mu))
ggplot() + geom_point(data=D.tab.test_38_gm, aes(Catch, Pr.above.LRP))


D.tab.test_40_gm <- NULL
for(i in 1:10) {
  test <- projections(DD.out$GBa, C.p = seq(min(proj$GBa), median(proj$GBa),100), my=40, gy=40)
  D.tab_40 <- decision(test,"GBa", mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]], yr=2025)
  D.tab_40$run <- i
  D.tab.test_40_gm <- rbind(D.tab.test_40_gm, D.tab_40)
}

mupgm <- ggplot() + geom_point(data=D.tab.test_40_gm[D.tab.test_40_gm$Catch<3000,], aes(Catch, mu), shape=1) +
  geom_point(data=D.tab.test_38_gm[D.tab.test_38_gm$Catch<3000,], aes(Catch, mu), shape=2) +
  ggtitle("2025 M & G (circles), 2023 M & G (triangles). Ran 10 times each.")
mpgm <- ggplot() + geom_point(data=D.tab.test_40_gm[D.tab.test_40_gm$Catch<3000,], aes(Catch, Pr.above.LRP), shape=1) +
  geom_point(data=D.tab.test_38_gm[D.tab.test_38_gm$Catch<3000,], aes(Catch, Pr.above.LRP), shape=2) +
  ggtitle("2025 M & G (circles), 2023 M & G (triangles). Ran 10 times each.")
require(patchwork)
mupgm/mpgm

D.tab.test_40_gm[D.tab.test_40_gm$Catch==0,]
D.tab.test_38_gm[D.tab.test_38_gm$Catch==0,]

D.tab.test_ltm <- NULL
for(i in 1) {
  test <- projections(DD.out$GBa, C.p = seq(min(proj$GBa), median(proj$GBa),100), my=20:40)
  D.tab_38 <- decision(test,"GBa", mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]], yr=2025)
  D.tab_38$run <- i
  D.tab.test_ltm <- rbind(D.tab.test_ltm, D.tab_38)
}

ggplot() + geom_point(data=D.tab.test_ltm, aes(Catch, mu))
ggplot() + geom_point(data=D.tab.test_ltm, aes(Catch, Pr.above.LRP))


D.tab.test_ltm <- NULL
for(i in 1:5) {
  test <- projections(DD.out$GBa, C.p = seq(min(proj$GBa), median(proj$GBa),100), my=38)
  D.tab_38 <- decision(test,"GBa", mu=0.15,refs=c(URP$GBa,LRP$GBa),post.survey.C=proj.catch$GBa, yr=2025)
  D.tab_38$run <- i
  D.tab.test_ltm <- rbind(D.tab.test_ltm, D.tab_38)
}

ggplot() + geom_point(data=D.tab.test_ltm, aes(Catch, mu))
ggplot() + geom_point(data=D.tab.test_ltm, aes(Catch, Pr.above.LRP))

df <- data.frame(clappers=DD.out$GBa$data$clappers, nat.mort.med = DD.out$GBa$median$m, nat.mort.mean = DD.out$GBa$mean$m)
ggplot() + geom_point(data=df, aes(clappers, nat.mort.med)) +
  geom_point(data=df, aes(clappers, nat.mort.mean), colour='red') +
  ylab("natural mortality\n(median=black, mean=red)") +
  xlab("clapper %") +
  geom_smooth(data=df, aes(clappers, nat.mort.med), method="lm")



#38
summary(test$sims.list$B.p[,1])
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1535    6733    8631    9243   10997   58391 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1463    6759    8675    9306   11099   46237 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1316    6741    8622    9240   11011   46558 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1660    6715    8603    9244   11006   45207 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
1761    6780    8660    9287   11069   47567 

D.tab_38 <- decision(test,"GBa", mu=0.15,refs=c(URP[[bnk]],LRP[[bnk]]),post.survey.C=proj.catch[[bnk]], yr=2025)


#39
summary(test$sims.list$B.p[,1])
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
915.8  4518.1  5897.6  6408.7  7713.1 43901.4 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
742.5  4534.1  5924.9  6417.8  7752.2 33642.5 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
686.8  4530.9  5933.7  6418.0  7728.1 31879.9 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
869.9  4525.1  5906.3  6416.0  7756.4 30882.3 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
827.4  4512.8  5900.8  6402.8  7709.6 48128.3 


#40
summary(test$sims.list$B.p[,1])
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
625.4  4129.9  5463.3  5963.1  7205.7 39425.8 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
951.6  4142.8  5450.1  5957.1  7201.8 40125.5 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
805.2  4130.1  5476.6  5978.6  7226.5 31460.7 

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
762.1  4128.9  5449.1  5949.5  7227.2 54449.8

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
705.3  4118.1  5442.8  5939.6  7171.8 32767.5

