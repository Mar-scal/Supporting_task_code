# survey summary run pulling all survey data from SCALOFF
load("Y:/Offshore/Assessment/Data/Survey_data/2021/Survey_summary_output/testing_results_historical_db.Rdata")

all.surv.dat.db <- all.surv.dat
bank.dat.db <- bank.dat
surv.Live.db <- surv.Live
surv.Rand.db <- surv.Rand
survey.obj.db <- survey.obj
merged.survey.obj.db <- merged.survey.obj
surv.dat.db <- surv.dat
mw.dat.all.db <- mw.dat.all
cf.data.db <- cf.data
CF.current.db <- CF.current
MW.dat.new.db <- MW.dat.new

# survey summary run from 2021 (before historical data were loaded)
load("Y:/Offshore/Assessment/Data/Survey_data/2021/Survey_summary_output/Survey_all_results - 2021FINAL.Rdata")
survey.obj.2021 <- survey.obj
all.surv.dat.2021 <- all.surv.dat
bank.dat.2021 <- bank.dat
surv.Live.2021 <- surv.Live
surv.Rand.2021 <- surv.Rand
merged.survey.obj.2021 <- merged.survey.obj
surv.dat.2021 <- surv.dat
mw.dat.all.2021 <- mw.dat.all
cf.data.2021 <- cf.data
CF.current.2021 <- CF.current
MW.dat.new.2021 <- MW.dat.new

# survey summary run from 2019
load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")
survey.obj.2019 <- survey.obj
all.surv.dat.2019 <- all.surv.dat
bank.dat.2019 <- bank.dat
surv.Live.2019 <- surv.Live
surv.Rand.2019 <- surv.Rand
merged.survey.obj.2019 <- merged.survey.obj
surv.dat.2019 <- surv.dat
mw.dat.all.2019 <- mw.dat.all
cf.data.2019 <- cf.data
CF.current.2019 <- CF.current
MW.dat.new.2019 <- MW.dat.new

survey.obj.2019$BBn$model.dat$I
survey.obj.2021$BBn$model.dat$I
survey.obj.db$BBn$model.dat$I

pdf(file = "Y:/Offshore/Assessment/2022/Presentations/Survey_summary/Database_pull_compare.pdf", onefile=T)
for(i in names(survey.obj.2021)[which(names(survey.obj.2021) %in% names(survey.obj.2019))]){
  if(!i=="Ger") {
    print(ggplot() + #geom_line(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="database")) +
            #geom_point(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="database")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('2019', '2021', 'database'),
                               values=c('2019'="black", '2021'='blue', 'database'='red'))+
            ggtitle(i) +
            theme_bw())
  }
  if(i=="Ger") {
    print(ggplot() + 
            geom_line(data=merged.survey.obj.2019, aes(year, I, colour="2019")) +
            geom_line(data=merged.survey.obj.2021, aes(year, I, colour="2021")) +
            geom_line(data=merged.survey.obj.db, aes(year, I, colour="database")) +
            geom_point(data=merged.survey.obj.2019, aes(year, I, colour="2019")) +
            geom_point(data=merged.survey.obj.2021, aes(year, I, colour="2021")) +
            geom_point(data=merged.survey.obj.db, aes(year, I, colour="database")) +
            geom_line(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="database")) +
            geom_point(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="database")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('2019', '2021', 'database'),
                               values=c('2019'="black", '2021'='blue', 'database'='red'))+
            ggtitle(i) +
            theme_bw())
  }
}
print(ggplot() + 
        geom_line(data=survey.obj.2021$BBs$model.dat, aes(year, I, colour="2021")) +
        geom_line(data=survey.obj.db$BBs$model.dat, aes(year, I, colour="database")) +
        geom_point(data=survey.obj.2021$BBs$model.dat, aes(year, I, colour="2021")) +
        geom_point(data=survey.obj.db$BBs$model.dat, aes(year, I, colour="database")) +
        scale_color_manual(name='Survey summary run',
                           breaks=c('2021', 'database'),
                           values=c('2021'='blue', 'database'='red'))+
        ggtitle("BBs") +
        theme_bw())
dev.off()

i="Mid"
ggplot() + geom_line(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
  geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
  geom_line(data=survey.obj[[i]]$model.dat, aes(year, I, colour="database")) +
  geom_point(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
  geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
  geom_point(data=survey.obj[[i]]$model.dat, aes(year, I, colour="database")) +
  scale_color_manual(name='Survey summary run',
                     breaks=c('2019', '2021', 'database'),
                     values=c('2019'="black", '2021'='blue', 'database'='red'))+
  ggtitle(i) +
  theme_bw()


require(tidyverse)
tows <- all.surv.dat.db %>% 
  dplyr::filter(!(bank=="GBa" & random==3)) %>%
  dplyr::filter(!(bank=="GBb" & random==3)) %>%
  #dplyr::filter(!(cruise %in% c("CK03", "P454") & bank%in%c("BBn", "BBs"))) %>%
  dplyr::select(year, bank, tow) %>%
  dplyr::distinct() %>%
  dplyr::arrange(year, bank, tow) %>%
  group_by(year, bank) %>%
  summarize(new=n())

tows2 <- dplyr::arrange(unique(dplyr::select(all.surv.dat.2021, year, bank, tow)), year, bank, tow) %>%
  group_by(year, bank) %>%
  summarize(old=n())


tows <- dplyr::left_join(tows, tows2)
dim(tows)
View(tows[is.na(tows$old),])
tows[!(tows$old==tows$new) & !is.na(tows$old),]



samples.db <- NULL
samples <- NULL
for(i in names(mw.dat.all.db)[which(names(mw.dat.all.db) %in% names(mw.dat.all))]){
  samples.db.1 <- mw.dat.all.db[[i]]
  samples.1 <- mw.dat.all[[i]]
  
  samples.db.1$bank <- i
  samples.1$bank <- i
  
  samples.db.1$num <- 1:nrow(samples.db.1)
  samples.1$num <- 1:nrow(samples.1)
  
  samples.db <- rbind(samples.db, samples.db.1)
  samples <- rbind(samples, samples.1)
}

samples.db <- samples.db %>% 
  dplyr::select(year, bank, tow, num) %>%
  dplyr::arrange(year, bank, tow, num) %>%
  group_by(year, bank) %>%
  summarize(new=n())

samples <- samples %>% 
  dplyr::select(year, bank, tow, num) %>%
  dplyr::arrange(year, bank, tow, num) %>%
  group_by(year, bank) %>%
  summarize(old=n())

samples.2 <- dplyr::left_join(samples, samples.db)
samples.2[!(samples.2$old==samples.2$new),]

samples.2[samples.2$year==2006 & samples.2$bank=="GBa",]
mw.dat.all$GBa[mw.dat.all$GBa$year==2006,]
sort(unique(mw.dat.all$GBa[mw.dat.all$GBa$year==2006,]$tow))
sort(unique(mw.dat.all.db$GBa[mw.dat.all.db$GBa$year==2006,]$tow))

MW.dat.new.db$GBa[MW.dat.new.db$GBa$tow %in% c(320, 327) & MW.dat.new.db$GBa$year==2006,]
all.surv.dat.db[all.surv.dat.db$bank=="GBa" & all.surv.dat.db$tow %in% c(320, 327) & all.surv.dat.db$year=="2006",]
all.surv.dat.db[all.surv.dat.db$bank=="GBa" & all.surv.dat.db$year=="2006",]

all.surv.dat[all.surv.dat$bank=="GBa" & all.surv.dat$tow %in% c(320, 327) & all.surv.dat$year=="2006",]
table(all.surv.dat$random)
table(all.surv.dat.db$random)

table(mw.dat.all.db$Sab[mw.dat.all.db$Sab$year==2000,]$tow)
table(mw.dat.all$Sab[mw.dat.all$Sab$year==2000,]$tow)

dim(unique(mw.dat.all$Sab[mw.dat.all$Sab$year==2000,]))
head(mw.dat.all$Sab[mw.dat.all$Sab$year==2000,])
head(mw.dat.all.db$Sab[mw.dat.all.db$Sab$year==2000,])



cf.db <- NULL
cf <- NULL
for(i in names(cf.data.db)[which(names(cf.data.db) %in% names(cf.data.2021))]){
  cf.db.1 <- cf.data.db[[i]]$CF.data
  cf.1 <- cf.data[[i]]$CF.data
  
  cf.db.1$bank <- i
  cf.1$bank <- i
  
  cf.db.1$num <- 1:nrow(cf.db.1)
  cf.1$num <- 1:nrow(cf.1)
  
  # for CFyrs only
  # if(i=="Ger") {
  #   cf.db.1 <- select(cf.db.1, -CF2)
  #   cf.1 <- select(cf.1, -CF2)
  # }
  
  cf.db <- rbind(cf.db, cf.db.1)
  cf <- rbind(cf, cf.1)
}

cf.db <- cf.db %>% 
  dplyr::select(year,bank,tow,num) %>%
  dplyr::arrange(year,bank,tow) %>%
  group_by(year, bank) %>%
  summarize(new=length(unique(tow))) 

cf <- cf %>% 
  dplyr::select(year,bank,tow,num) %>%
  dplyr::arrange(year,bank,tow) %>%
  group_by(year, bank) %>%
  summarize(old=length(unique(tow))) 

cf.2 <- dplyr::left_join(cf, cf.db)
cf.2[!cf.2$new == cf.2$old,]
cf.2[is.na(cf.2$new) & !is.na(cf.2$old),]
cf.2[!is.na(cf.2$new) & is.na(cf.2$old),]

dim(cf.data.db$BBn$CF.data)
dim(cf.data.2021$BBn$CF.data)

BBn2005db <- cf.data.db$BBn$CF.data[cf.data.db$BBn$CF.data$year==2005,]
BBn2005 <- cf.data.2021$BBn$CF.data[cf.data.2021$BBn$CF.data$year==2005,]
dim(BBn2005db)
dim(BBn2005)
cf.data.db$BBn$CF.data[which(!cf.data.db$BBn$CF.data$ID %in% cf.data.2021$BBn$CF.data$ID), c("year", "tow")]


dim(surv.dat.db$BBn[surv.dat.db$BBn$year==2015,]) == dim(surv.dat.2021$BBn[surv.dat.2021$BBn$year==2015,])
dim(MW.dat.new.db[MW.dat.new.db$bank=="BBn" & MW.dat.new.db$year==2015,]) == dim(MW.dat.new.2021[MW.dat.new.2021$bank=="BBn" & MW.dat.new.2021$year==2015,])
dim(mw.dat.all.db$BBn[mw.dat.all.db$BBn$year==2015,]) == dim(mw.dat.all.2021$BBn[mw.dat.all.2021$BBn$year==2015,])
mw.dat.all.db$BBn[mw.dat.all.db$BBn$year==2015,]
mw.dat.all.2021$BBn[mw.dat.all.2021$BBn$year==2015,]



dim(surv.dat.db$BBs[surv.dat.db$BBs$year==2000,]) == dim(surv.dat.2021$BBs[surv.dat.2021$BBs$year==2000,])
dim(MW.dat.new.db[MW.dat.new.db$bank=="BBs" & MW.dat.new.db$year==2000,]) == dim(MW.dat.new.2021[MW.dat.new.2021$bank=="BBs" & MW.dat.new.2021$year==2001,])
dim(mw.dat.all.db$BBs[mw.dat.all.db$BBs$year==2000,]) == dim(mw.dat.all.2021$BBs[mw.dat.all.2021$BBs$year==2000,])
mw.dat.all.db$BBs[mw.dat.all.db$BBs$year==2000,]
mw.dat.all.2021$BBs[mw.dat.all.2021$BBs$year==2000,]

sort(unique(mw.dat.all.db$BBs$year))
sort(unique(mw.dat.all$BBs$year))

BBs2001db <- all.surv.dat.db[all.surv.dat.db$bank=="BBs" & all.surv.dat.db$year==2000,] %>% arrange(year, tow)
BBs2001 <- all.surv.dat.2021[all.surv.dat.2021$bank=="BBs" & all.surv.dat.2021$year==2000,] %>% arrange(year, tow)

head(BBs2001)[,5:10]
head(BBs2001db)[,5:10]

require(compareDF)
create_output_table(compare_df(tows, tows2), output_type = 'xlsx', file_name = "test_file.xlsx")
dim(tows)
dim(tows2)


dim(all.surv.dat.db)
dim(all.surv.dat)

dim(bank.dat.db$BBn)
dim(bank.dat$BBn)

dim(surv.Live.db$BBn)
dim(surv.Live$BBn)

# dim(survey.obj.db$BBn$bankpertow)
# dim(survey.obj$BBn$bankpertow)

dim(surv.dat.db$BBn)
dim(surv.dat$BBn)

dim(mw.dat.all.db$BBn)
dim(mw.dat.all$BBn)

dim(cf.data.db$BBn$CF.data)
dim(cf.data$BBn$CF.data)

dim(CF.current.db$BBn)
dim(CF.current$BBn)


survey.obj.2021 <- survey.obj

load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")
survey.obj.2019 <- survey.obj
