direct <- "Y:/Offshore scallop/Assessment/"
logs_and_fish(loc = "offshore",year=1981:2018,export=T,get.marfis = F,ex.marfis = F,
                          direct.in = NULL, un=un.ID,pw=pwd.ID,db.con="ptran",db.lib = "ROracle", direct=NULL)
fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)

write.csv(fish.dat,paste0(direct,"Data/Fishery_data/Summary/2019/Complete_logs_1981_to_2018.csv"))
