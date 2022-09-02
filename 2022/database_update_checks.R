# survey summary run pulling all survey data from SCALOFF
load("C:/Users/keyserf/Documents/temp_data/testing_results_historical_db.Rdata")

all.surv.dat.db <- all.surv.dat
bank.dat.db <- bank.dat
surv.Live.db <- surv.Live
surv.Rand.db <- surv.Rand
survey.obj.db <- survey.obj
clap.survey.obj.db <- clap.survey.obj
merged.survey.obj.db <- merged.survey.obj
surv.dat.db <- surv.dat
mw.dat.all.db <- mw.dat.all
cf.data.db <- cf.data
CF.current.db <- CF.current
MW.dat.new.db <- MW.dat.new
mw.db <- mw
MW.dat.db <- MW.dat

# survey summary run pulling all survey data from SCALOFF, subsetting commercial by cruise
#load("C:/Users/keyserf/Documents/temp_data/testing_results_historical_db_comm.Rdata")
load("Y:/Offshore/Assessment/Data/Survey_data/2022/Survey_summary_output/testing_results_historical_db_comm_2.Rdata")

all.surv.dat.db2 <- all.surv.dat
bank.dat.db2 <- bank.dat
surv.Live.db2 <- surv.Live
surv.Rand.db2 <- surv.Rand
survey.obj.db2 <- survey.obj
clap.survey.obj.db2 <- clap.survey.obj
merged.survey.obj.db2 <- merged.survey.obj
surv.dat.db2 <- surv.dat
mw.dat.all.db2 <- mw.dat.all
cf.data.db2 <- cf.data
CF.current.db2 <- CF.current
MW.dat.new.db2 <- MW.dat.new
mw.db2 <- mw
MW.dat.db2 <- MW.dat


load("Y:/Offshore/Assessment/Data/Survey_data/2022/Survey_summary_output/testing_results_historical_db_gbb2.Rdata")

all.surv.dat.db2 <- all.surv.dat
bank.dat.db2 <- bank.dat
surv.Live.db2 <- surv.Live
surv.Rand.db2 <- surv.Rand
survey.obj.db2 <- survey.obj
clap.survey.obj.db2 <- clap.survey.obj
merged.survey.obj.db2 <- merged.survey.obj
surv.dat.db2 <- surv.dat
mw.dat.all.db2 <- mw.dat.all
cf.data.db2 <- cf.data
CF.current.db2 <- CF.current
MW.dat.new.db2 <- MW.dat.new
mw.db2 <- mw
MW.dat.db2 <- MW.dat

# survey summary run from 2021 (before historical data were loaded)
load("C:/Users/keyserf/Documents/temp_data/Survey_all_results - 2021FINAL.Rdata")
survey.obj.2021 <- survey.obj
clap.survey.obj.2021 <- clap.survey.obj
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
mw.2021 <- mw
MW.dat.2021 <- MW.dat

# survey summary run from 2019
load("Y:/Offshore/Assessment/Data/Survey_data/2019/Survey_summary_output/Survey_all_results.Rdata")
survey.obj.2019 <- survey.obj
clap.survey.obj.2019 <- clap.survey.obj
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
for(i in names(survey.obj.db2)[which(names(survey.obj.db2) %in% names(survey.obj.2021))]){
  if(!i=="Ger") {
    print(ggplot() + #geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, n, colour="2021")) +
            geom_line(data=survey.obj.db2[[i]]$model.dat, aes(year, n, colour="db2")) +
            #geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, n, colour="2021")) +
            geom_point(data=survey.obj.db2[[i]]$model.dat, aes(year, n, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(i) +
            theme_bw())
    print(ggplot() + #geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, N, colour="2021")) +
            geom_line(data=survey.obj.db2[[i]]$model.dat, aes(year, N, colour="db2")) +
            #geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, N, colour="2021")) +
            geom_point(data=survey.obj.db2[[i]]$model.dat, aes(year, N, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(i) +
            theme_bw())
    print(ggplot() + #geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_line(data=survey.obj.db2[[i]]$model.dat, aes(year, I, colour="db2")) +
            #geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_point(data=survey.obj.db2[[i]]$model.dat, aes(year, I, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(i) +
            theme_bw())
    print(ggplot() + #geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, CF, colour="2021")) +
            geom_line(data=survey.obj.db2[[i]]$model.dat, aes(year, CF, colour="db2")) +
            #geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, CF, colour="2021")) +
            geom_point(data=survey.obj.db2[[i]]$model.dat, aes(year, CF, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(i) +
            theme_bw())
    print(ggplot() + #geom_line(data=clap.survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=clap.survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_line(data=clap.survey.obj.db2[[i]]$model.dat, aes(year, I, colour="db2")) +
            #geom_point(data=clap.survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=clap.survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_point(data=clap.survey.obj.db2[[i]]$model.dat, aes(year, I, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(paste0(i, "-clappers")) +
            theme_bw())
  }
  if(i=="Ger") {
    print(ggplot() + 
            #geom_line(data=merged.survey.obj.db, aes(year, I, colour="db")) +
            geom_line(data=merged.survey.obj.2021, aes(year, n, colour="2021")) +
            geom_line(data=merged.survey.obj.db2, aes(year, n, colour="db2")) +
            #geom_point(data=merged.survey.obj.db, aes(year, I, colour="db")) +
            geom_point(data=merged.survey.obj.2021, aes(year, n, colour="2021")) +
            geom_point(data=merged.survey.obj.db2, aes(year, n, colour="db2")) +
            #geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, n, colour="2021")) +
            geom_line(data=survey.obj.db2[[i]]$model.dat, aes(year, n, colour="db2")) +
            #geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, n, colour="2021")) +
            geom_point(data=survey.obj.db2[[i]]$model.dat, aes(year, n, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(i) +
            theme_bw())
     print(ggplot() + 
            #geom_line(data=merged.survey.obj.db, aes(year, I, colour="db")) +
            geom_line(data=merged.survey.obj.2021, aes(year, N, colour="2021")) +
            geom_line(data=merged.survey.obj.db2, aes(year, N, colour="db2")) +
            #geom_point(data=merged.survey.obj.db, aes(year, I, colour="db")) +
            geom_point(data=merged.survey.obj.2021, aes(year, N, colour="2021")) +
            geom_point(data=merged.survey.obj.db2, aes(year, N, colour="db2")) +
            #geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, N, colour="2021")) +
            geom_line(data=survey.obj.db2[[i]]$model.dat, aes(year, N, colour="db2")) +
            #geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, N, colour="2021")) +
            geom_point(data=survey.obj.db2[[i]]$model.dat, aes(year, N, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(i) +
            theme_bw())
     print(ggplot() + 
            #geom_line(data=merged.survey.obj.db, aes(year, I, colour="db")) +
            geom_line(data=merged.survey.obj.2021, aes(year, I, colour="2021")) +
            geom_line(data=merged.survey.obj.db2, aes(year, I, colour="db2")) +
            #geom_point(data=merged.survey.obj.db, aes(year, I, colour="db")) +
            geom_point(data=merged.survey.obj.2021, aes(year, I, colour="2021")) +
            geom_point(data=merged.survey.obj.db2, aes(year, I, colour="db2")) +
            #geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_line(data=survey.obj.db2[[i]]$model.dat, aes(year, I, colour="db2")) +
            #geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_point(data=survey.obj.db2[[i]]$model.dat, aes(year, I, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(i) +
            theme_bw())
     print(ggplot() + 
             #geom_line(data=merged.survey.obj.db, aes(year, I, colour="db")) +
             geom_line(data=merged.survey.obj.2021, aes(year, CF, colour="2021")) +
             geom_line(data=merged.survey.obj.db2, aes(year, CF, colour="db2")) +
             #geom_point(data=merged.survey.obj.db, aes(year, I, colour="db")) +
             geom_point(data=merged.survey.obj.2021, aes(year, CF, colour="2021")) +
             geom_point(data=merged.survey.obj.db2, aes(year, CF, colour="db2")) +
             #geom_line(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
             geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, CF, colour="2021")) +
             geom_line(data=survey.obj.db2[[i]]$model.dat, aes(year, CF, colour="db2")) +
             #geom_point(data=survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
             geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, CF, colour="2021")) +
             geom_point(data=survey.obj.db2[[i]]$model.dat, aes(year, CF, colour="db2")) +
             scale_color_manual(name='Survey summary run',
                                breaks=c('db', '2021', 'db2'),
                                values=c('db'="black", '2021'='blue', 'db2'='red'))+
             ggtitle(i) +
             theme_bw())
    print(ggplot() + #geom_line(data=clap.survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_line(data=clap.survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_line(data=clap.survey.obj.db2[[i]]$model.dat, aes(year, I, colour="db2")) +
            #geom_point(data=clap.survey.obj.db[[i]]$model.dat, aes(year, I, colour="db")) +
            geom_point(data=clap.survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_point(data=clap.survey.obj.db2[[i]]$model.dat, aes(year, I, colour="db2")) +
            scale_color_manual(name='Survey summary run',
                               breaks=c('db', '2021', 'db2'),
                               values=c('db'="black", '2021'='blue', 'db2'='red'))+
            ggtitle(paste0(i, "-clappers")) +
            theme_bw())
  }
}
# print(ggplot() + 
#         geom_line(data=survey.obj.2021$BBs$model.dat, aes(year, I, colour="2021")) +
#         #geom_line(data=survey.obj.db2$BBs$model.dat, aes(year, I, colour="db2")) +
#         geom_point(data=survey.obj.2021$BBs$model.dat, aes(year, I, colour="2021")) +
#         #geom_point(data=survey.obj.db2$BBs$model.dat, aes(year, I, colour="db2")) +
#         scale_color_manual(name='Survey summary run',
#                            breaks=c('2021', 'db2'),
#                            values=c('2021'='blue', 'db2'='red'))+
#         ggtitle("BBs") +
#         theme_bw())
# print(ggplot() + 
#         geom_line(data=clap.survey.obj.2021$BBs$model.dat, aes(year, I, colour="2021")) +
#         #geom_line(data=survey.obj.db2$BBs$model.dat, aes(year, I, colour="db2")) +
#         geom_point(data=clap.survey.obj.2021$BBs$model.dat, aes(year, I, colour="2021")) +
#         #geom_point(data=survey.obj.db2$BBs$model.dat, aes(year, I, colour="db2")) +
#         scale_color_manual(name='Survey summary run',
#                            breaks=c('2021', 'db2'),
#                            values=c('2021'='blue', 'db2'='red'))+
#         ggtitle("BBs") +
#         theme_bw())
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
  ggtitle(paste0(i, "-clappers")) +
  theme_bw()


require(tidyverse)
tows <- all.surv.dat.db2 %>% 
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
for(i in names(mw.dat.all.db2)[which(names(mw.dat.all.db2) %in% names(mw.dat.all))]){
  samples.db.1 <- mw.dat.all.db2[[i]]
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
View(samples.2[!(samples.2$old==samples.2$new),])

samples.2[samples.2$year==2006 & samples.2$bank=="GBa",]
mw.dat.all$GBa[mw.dat.all$GBa$year==2006,]
sort(unique(mw.dat.all$GBa[mw.dat.all$GBa$year==2006,]$tow))
sort(unique(mw.dat.all.db2$GBa[mw.dat.all.db2$GBa$year==2006,]$tow))

mw.dat.all.db2$GBa[mw.dat.all.db2$GBa$tow %in% c(320, 327) & mw.dat.all.db2$GBa$year==2006,]
all.surv.dat.db2[all.surv.dat.db2$bank=="GBa" & all.surv.dat.db2$tow %in% c(320, 327) & all.surv.dat.db2$year=="2006",]
all.surv.dat.db2[all.surv.dat.db2$bank=="GBa" & all.surv.dat.db2$year=="2006",]

all.surv.dat[all.surv.dat$bank=="GBa" & all.surv.dat$tow %in% c(320, 327) & all.surv.dat$year=="2006",]
table(all.surv.dat$random)
table(all.surv.dat.db2$random)

table(mw.dat.all.db$Sab[mw.dat.all.db$Sab$year==2000,]$tow)
table(mw.dat.all$Sab[mw.dat.all$Sab$year==2000,]$tow)

dim(unique(mw.dat.all$Sab[mw.dat.all$Sab$year==2000,]))
head(mw.dat.all$Sab[mw.dat.all$Sab$year==2000,])
head(mw.dat.all.db$Sab[mw.dat.all.db$Sab$year==2000,])


for (i in 1:length(unique(mw.dat.all.db$Ger$year))) {
  message(unique(mw.dat.all.db$Ger$year)[i])
  print(dim(mw.dat.all.db$Ger[mw.dat.all.db$Ger$year==unique(mw.dat.all.db$Ger$year)[i],]) ==
          dim(mw.dat.all.2021$Ger[mw.dat.all.2021$Ger$year==unique(mw.dat.all.db$Ger$year)[i],]))
}

#1983, 1984 have samples in new db
all.surv.dat.2021 %>% group_by(bank) %>%
  summarize(start=min(year))

for (i in names(mw.dat.all.2021)){
  print(i)
  print(min(unique(mw.dat.all.2021[[i]]$year)))
}

for (i in names(mw.dat.all.db)){
  print(i)
  print(min(unique(mw.dat.all.db[[i]]$year)))
}

MW.dat.new.2021 %>% group_by(bank) %>%
  summarize(start=min(year))

MW.dat.new.db %>% group_by(bank) %>%
  summarize(start=min(year))

test <- MW.dat.new.db %>%
  filter((year>1983 & !bank %in% c("GBa", "GBb", "GB")) | (year>1980 & bank %in% c("GBa", "GBb", "GB")))

test %>% group_by(bank) %>%
  summarize(start=min(year))

# commercial + historical
MW.dat.2021 %>% group_by(bank) %>% summarize(start=min(year)) 
MW.dat.db <- MW.dat.2021 %>% filter(tow==0)
#commercial
MW.dat.db %>% group_by(bank) %>% summarize(start=min(year))
#survey
MW.dat.new.db %>% group_by(bank) %>% summarize(start=min(year))


MW.dat.2021$tow <- as.numeric(MW.dat.2021$tow)
MW.dat.2021$year <- as.character(MW.dat.2021$year)
test <- full_join(MW.dat.2021, MW.dat.new.2021) %>%
  select(cruise, bank, year, scalnum) %>%
  group_by(cruise, bank, year) %>%
  summarize(count=length(unique(scalnum)))
write.csv(test, "C:/Users/keyserf/Desktop/oldmw.csv")

MW.dat.db$tow <- as.numeric(MW.dat.db$tow)
MW.dat.db$year <- as.character(MW.dat.db$year)
test <- full_join(MW.dat.db, MW.dat.new.db) %>%
  select(cruise, bank, year, scalnum) %>%
  group_by(cruise, bank, year) %>%
  summarize(count=length(unique(scalnum)))
write.csv(test, "C:/Users/keyserf/Desktop/newmw.csv")

MW.dat.new.db[MW.dat.new.db$cruise=="P472",]

unique(mw.dat.all.db$GBa[mw.dat.all.db$GBa$year==1982,]$ID)

> #commercial
  MW.dat.db %>% group_by(bank) %>% summarize(start=min(year))
# A tibble: 9 x 2
bank  start
<chr> <int>
  1 Ban    2000
2 BBn    1989
3 BBs    1998
4 GB     1983
5 GBa    1983
6 GBb    1983
7 Ger    1993
8 Mid    1983
9 Sab    1983
> #survey
  MW.dat.new.db %>% group_by(bank) %>% summarize(start=min(year))
# A tibble: 12 x 2
bank    start
<chr>   <chr>
  1 Ban     1985 
2 BanIce  2019 
3 BBn     1983 
4 BBs     1983 
5 GBa     1982 
6 GBb     1982 
7 GBUSA   1984 
8 Ger     1983 
9 LURCHER 1983 
10 Mid     1983 
11 Sab     1984 
12 SPB     2004 




cf.db <- NULL
cf <- NULL
for(i in names(cf.data.db)[which(names(cf.data.db) %in% names(cf.data.2021))]){
  cf.db.1 <- cf.data.db[[i]]$CF.data
  cf.1 <- cf.data.2021[[i]]$CF.data
  
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
year  bank    old   new
<chr> <chr> <int> <int>
  1 1984  GBa      28    26 # possibly due to GBUSA
2 1984  GBb      10     9 # possibly due to GBUSA
3 1985  GBa      29    20 #?
4 1985  GBb       4     3 #?
5 1987  GBa      32    30 #?
6 1987  GBb       8     4 #?
7 1988  GBa      13    11 #?
8 NA    NA       NA    NA
9 1989  GB       13     1 # AG flagged missing data?
10 1989  GBa      26    25 #?
11 1995  GB       17    10 #?
12 1995  GBa      19    12 #?
13 1996  GB       14    10 #?
14 1996  GBa      24    20 #?
15 2000  Ger       8     1 # corrected
16 2006  GBa      18    20 #?

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


# German 1999 discrepancy
all.surv.dat.db %>% 
  filter(bank=="Ger" & year==1999) %>%
  nrow()

all.surv.dat.2021 %>% 
  filter(bank=="Ger" & year==1999) %>%
  nrow()

mw.dat.all.db$Ger %>% 
  filter(year==1999) %>%
  nrow()

mw.dat.all.2021$Ger %>% 
  filter(year==1999) %>%
  nrow()

MW.dat.new.db %>% 
  filter(bank=="Ger" & year==1999) %>%
  nrow()

MW.dat.new.2021 %>% 
  filter(bank=="Ger" & year==1999) %>%
  nrow()

cf.data.db$Ger$CFyrs %>% filter(year==1999)
cf.data.2021$Ger$CFyrs %>% filter(year==1999)

cf.data.db$Ger$CF.data %>% filter(year==1999)
cf.data.2021$Ger$CF.data %>% filter(year==1999)

cf.data.db$Ger$HtWt.fit$data %>% filter(year==1999)
cf.data.2021$Ger$HtWt.fit$data %>% filter(year==1999)

cf.data.db$Ger$HtWt.fit$data %>%
  group_by(year) %>%
  summarize(n())
unique(cf.data.db$Ger$HtWt.fit$data$year)
unique(cf.data.db$GBa$HtWt.fit$data$year)
cf.data.2021$Ger$HtWt.fit$data %>%
  group_by(year) %>%
  summarize(n())
unique(cf.data.2021$Ger$HtWt.fit$data$year)

load("Y:/Offshore/Assessment/Data/Survey_data/2021/Survey_summary_output/testing_results_historical_db_ger.Rdata")
unique(cf.data$Ger$HtWt.fit$data$year)

# cruise P322 added
# check for other new cruises?

newcruise <- unique(MW.dat.new.db$cruise)[which(!unique(MW.dat.new.db$cruise) %in% unique(MW.dat.new.2021$cruise))]

table(MW.dat.new.db[MW.dat.new.db$cruise %in% newcruise,]$bank,
      MW.dat.new.db[MW.dat.new.db$cruise %in% newcruise,]$year)

table(MW.dat.new.db[MW.dat.new.db$cruise %in% newcruise,]$bank,
      MW.dat.new.db[MW.dat.new.db$cruise %in% newcruise,]$year)

unique(mw.db$Ger$cruise)[which(!unique(mw.db$Ger$cruise) %in% unique(mw.2021$Ger$cruise))]

unique(mw.db$Ger$cruise)==unique(MW.dat.new.db$cruise[MW.dat.new.db$bank=="Ger"])
unique(mw.2021$Ger$cruise)==unique(MW.dat.new.2021$cruise[MW.dat.new.2021$bank=="Ger"])

unique(MW.dat.db[MW.dat.db$bank=="Ger",]$cruise)

unique(c(unique(mw.2021$Ger$cruise), unique(MW.dat.2021[MW.dat.2021$bank=="Ger",]$cruise)))
# CK15 included twice?
dim(mw.2021$Ger[mw.2021$Ger$cruise=="CK15",])
dim(MW.dat.2021[MW.dat.2021$bank=="Ger" & MW.dat.2021$cruise=="CK15",])
mw.dat.all.db$Ger$ID[which(!mw.dat.all.db$Ger$ID %in% mw.dat.all.2021$Ger$ID)]
mw.dat.all.2021$Ger$ID[which(!mw.dat.all.2021$Ger$ID %in% mw.dat.all.db$Ger$ID)]

#MW.dat.2021$ID <- paste0(MW.dat.2021$cruise, ".", MW.dat.2021$tow)

missing_21 <- NULL
missing_db <- NULL
cruise.mw.db <- NULL
cruise.mw.21 <-  NULL
combined <- NULL
for(i in c("BBn","Ger","Sab","Mid", "GB", "GBa", "GBb")){
  print(i)
  if(i %in% c("BBn","Ger","Sab")){
    testdb <- merge(
      subset(MW.dat.db2[MW.dat.db2$bank==i,], 
             month %in% 5:6 & year %in% 1985:2021,
             c("cruise","year",
               "lon","lat","depth",
               "sh","wmw", "month", "tow")),
      subset(mw.db2[[i]], (month %in% 5:6 & !year %in% c(2015, 2020)) | year==2015 | year==2000, 
             select=c("cruise","year",
                      "lon","lat","depth",
                      "sh","wmw", "month", "tow")),
      all=T)
    
    test21 <- merge(
      subset(MW.dat.2021[MW.dat.2021$bank==i,], 
             month %in% 5:6 & year %in% 1985:2021,
             c("cruise","year",
               "lon","lat","depth",
               "sh","wmw", "month", "tow")),
      subset(mw.2021[[i]], (month %in% 5:6 & !year %in% c(2015, 2020)) | year==2015 | year==2000, 
             select=c("cruise","year",
                      "lon","lat","depth",
                      "sh","wmw", "month", "tow")),
      all=T)
  }
  
  if(i %in% c("GB")){
    testdb <- merge(
      subset(MW.dat.db2[MW.dat.db2$bank %in% c("GB", "GBa", "GBb"),], 
             month %in% 5:6 & year %in% 1985:2021,
             c("cruise","year",
               "lon","lat","depth",
               "sh","wmw", "month", "tow")),
      subset(mw.db2[[i]], (month %in% 5:6 & !year %in% c(2015, 2020)) | year==2015 | year==2000, 
             select=c("cruise","year",
                      "lon","lat","depth",
                      "sh","wmw", "month", "tow")),
      all=T)
    
    test21 <- merge(
      subset(MW.dat.2021[MW.dat.2021$bank %in% c("GB", "GBa", "GBb"),], 
             month %in% 5:6 & year %in% 1985:2021,
             c("cruise","year",
               "lon","lat","depth",
               "sh","wmw", "month", "tow")),
      subset(mw.2021[[i]], (month %in% 5:6 & !year %in% c(2015, 2020)) | year==2015 | year==2000, 
             select=c("cruise","year",
                      "lon","lat","depth",
                      "sh","wmw", "month", "tow")),
      all=T)
  }
  
  if(i %in% c("GBa", "GBb")){
    testdb <- merge(
      subset(MW.dat.db2[MW.dat.db2$bank==i,], 
             month >7 & year %in% years,
             c("cruise","year",
               "lon","lat","depth",
               "sh","wmw", "month", "tow")),
      subset(mw.db2[[i]], year %in% years & month > 7,
             select=c("cruise","year",
                      "lon","lat","depth",
                      "sh","wmw", "month", "tow")),
      all=T)
   
    test21 <- merge(
      subset(MW.dat.2021[MW.dat.2021$bank==i,], 
             month > 7 & year %in% years,
             c("cruise","year",
               "lon","lat","depth",
               "sh","wmw", "month", "tow")),
      subset(mw.2021[[i]], year %in% years & month > 7,
             select=c("cruise","year",
                      "lon","lat","depth",
                      "sh","wmw", "month", "tow")),
      all=T)
  }
  
  if(i == "Mid"){
    testdb <- merge(
      subset(MW.dat.db2[MW.dat.db2$bank==i,], 
             month %in% 5:6 & year >1983,
             c("cruise","year",
               "lon","lat","depth",
               "sh","wmw", "month", "tow")),
      subset(mw.db2[[i]], month %in% 5:6, 
             select=c("cruise","year",
                      "lon","lat","depth",
                      "sh","wmw", "month", "tow")),
      all=T)
    
    test21 <- merge(
      subset(MW.dat.2021[MW.dat.2021$bank==i,], 
             month %in% 5:6 & year >1983,
             c("cruise","year",
               "lon","lat","depth",
               "sh","wmw", "month", "tow")),
      subset(mw.2021[[i]], month %in% 5:6, 
             select=c("cruise","year",
                      "lon","lat","depth",
                      "sh","wmw", "month", "tow")),
      all=T)
  }
  # merge(
  #   subset(mw.tmp, 
  #          month %in% 5:6 & year %in% years,
  #          c("ID","year","lon","lat","depth","sh","wmw","tow")),
  #   subset(mw[[bnk]], (month %in% 5:6 & !year %in% 2015) | year==2015 | year==2000, 
  #          select=c("ID","year","lon","lat","depth","sh","wmw","tow")),
  #   all=T)
  
  # test21$ID <- paste0(test21$cruise, ".", test21$tow)
  # 
  # testdb$tow <- as.character(testdb$tow)
  
  testdb <- testdb %>%
    group_by(cruise, year, lon, lat, month, depth, tow) %>%
    summarize(numsamples=n(),
              meansh = mean(sh), 
              meanwmw = mean(wmw))

  test21 <- test21 %>%
    group_by(cruise, year, lon, lat, month, depth, tow) %>%
    summarize(numsamples=n(),
              meansh = mean(sh), 
              meanwmw = mean(wmw))
  
  testdb$bank <- i
  test21$bank <- i
  
  testdb$db <- "db"
  test21$old <- "21"
  
  testdb$lon <- round(testdb$lon,0)
  testdb$lat <- round(testdb$lat,0)
  test21$lon <- round(test21$lon,0)
  test21$lat <- round(test21$lat,0)
  test21$depth <- round(test21$depth,-1)
  testdb$depth <- round(testdb$depth,-1)
  testdb$meansh <- round(testdb$meansh,0)
  testdb$meanwmw <- round(testdb$meanwmw,0)
  test21$meansh <- round(test21$meansh,0)
  test21$meanwmw <- round(test21$meanwmw,0)
  
  combine <- full_join(testdb, test21, by=c("cruise", "year", "lon", "lat", "month", "depth", "numsamples", "meansh", "meanwmw", "bank"))
  combined <- rbind(combined, combine)
  
  not_in_21 <- anti_join(testdb, test21)
  not_in_db <- anti_join(test21, testdb)
  
  missing_21 <- rbind(missing_21, not_in_21)
  missing_db <- rbind(missing_db, not_in_db)
  
  cruise.mw.db <- rbind(cruise.mw.db, testdb)
  cruise.mw.21 <- rbind(cruise.mw.21, test21)
  
}
dim(missing_21)
dim(missing_db)

dim(combined)
combined$check <- "fine"
combined$check[is.na(combined$db) | is.na(combined$old)] <- "check"

dim(combined[combined$check == "check",])
combined[combined$bank=="GB" & combined$check == "check" & combined$cruise=="CK04",]

write.csv(combined, file="C:/Users/keyserf/Documents/temp_data/combined.csv")


source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")
offshore <- github_spatial_import("offshore_survey_strata", "offshore_survey_strata.zip")

ggplot() + geom_sf(data=offshore[offshore$label=="GBa",])+
  geom_point(data=NULL, aes(x=-66.618, y=	41.98166667))

table(MW.dat.2021[MW.dat.2021$cruise=="P387",]$bank)
table(mw.db2$GB[mw.db2$GB$cruise=="P387",]$year)
table(mw.db2$GBa[mw.db2$GBa$cruise=="P387",]$month)
table(mw.db2$GBb[mw.db2$GBb$cruise=="P387",]$month)
sort(unique(as.numeric(mw.db2$GBa[mw.db2$GBa$cruise=="P387",]$tow)))
sort(unique(as.numeric(MW.dat.2021[MW.dat.2021$cruise=="P387" & MW.dat.2021$bank=="GBa",]$tow)))
sort(unique(as.numeric(MW.dat.2021[MW.dat.2021$cruise=="P387" & MW.dat.2021$bank=="GB",]$tow)))
sort(unique(as.numeric(mw.db2$GBb[mw.db2$GBb$cruise=="P387",]$tow)))
sort(unique(as.numeric(MW.dat.2021[MW.dat.2021$cruise=="P387" & MW.dat.2021$bank=="GBb",]$tow)))

table(MW.dat.2021[MW.dat.2021$cruise=="CK32",]$bank)
table(MW.dat.new.2021[MW.dat.new.2021$cruise=="CK32",]$bank)
table(mw.db2$GBa[mw.db2$GBa$cruise=="CK32",]$year)
sort(unique(as.numeric(mw.db2$GBa[mw.db2$GBa$cruise=="CK32",]$tow)))
sort(unique(as.numeric(MW.dat.new.2021[MW.dat.new.2021$cruise=="CK32" & MW.dat.new.2021$bank=="GBa",]$tow)))


ggplot() + geom_sf(data=offshore[offshore$label%in% c("GBa", "GBb"),]) + 
  #geom_point(data=MW.dat.2021[MW.dat.2021$cruise=="P322",], aes(lon, lat)) +
  geom_text(data=MW.dat.2021[MW.dat.2021$cruise=="P387" & MW.dat.2021$bank %in% c("GBa", "GBb", "GB"),], aes(lon, lat, label=tow)) +
  geom_text(data=mw.db2$GBa[mw.db2$GBa$cruise=="P387",], aes(lon, lat, label=tow), colour="red") +
  geom_text(data=mw.db2$GBb[mw.db2$GBb$cruise=="P387",], aes(lon, lat, label=tow), colour="red") 


oldest.41 <- bank.dat.2019$GBb[bank.dat.2019$GBb$year==1998 & bank.dat.2019$GBb$state=="live",]# & bank.dat.2019$GBb$tow==41,]
old.41 <- bank.dat.2021$GBb[bank.dat.2021$GBb$year==1998 & bank.dat.2021$GBb$state=="live",] #& bank.dat.2021$GBb$tow==41,]
new.41 <- bank.dat.db2$GBb[bank.dat.db2$GBb$year==1998 & bank.dat.db2$GBb$state=="live",] #& bank.dat.db2$GBb$tow==41,]

# GBb 1992 strata change for tow 41
oldest.41 <- bank.dat.2019$GBb[bank.dat.2019$GBb$year==1992 & bank.dat.2019$GBb$state=="live" & bank.dat.2019$GBb$tow==41,]
old.41 <- bank.dat.2021$GBb[bank.dat.2021$GBb$year==1992 & bank.dat.2021$GBb$state=="live" & bank.dat.2021$GBb$tow==41,]
new.41 <- bank.dat.db2$GBb[bank.dat.db2$GBb$year==1992 & bank.dat.db2$GBb$state=="live" & bank.dat.db2$GBb$tow==41,]

ggplot() + geom_sf(data=offshore[offshore$label %in% c("GBb"),], aes(fill=Strt_ID)) + geom_point(data=old.41, aes(lon, lat), colour="red") + 
  geom_point(data=new.41, aes(lon, lat), colour="blue") + xlim(-66.16, -66.15) + ylim(42, 42.005)
# both appear to be in 101
new.41$Strata_ID
old.41$Strata_ID

which(!bank.dat.2021$GBb$Strata_ID == bank.dat.db2$GBb$Strata_ID)
bank.dat.2021$GBb[265:266,]
bank.dat.db2$GBb[265:266,]

require(PBSmapping)
survey.detail.polys <- read.csv(paste0(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv"), 
                                header=T,stringsAsFactors = F)
detail.poly.surv <- subset(survey.detail.polys,label=="GBb")
attr(detail.poly.surv,"projection")<-"LL"
source(paste(direct_fns,"Survey_and_OSAC/assign_strata.r",sep=""),local=T) 
new.41 <- dplyr::select(new.41, -Strata_ID)
old.41 <- dplyr::select(old.41, -Strata_ID)
oldest.41 <- dplyr::select(oldest.41, -Strata_ID)

new.gbb <- assign.strata(new.41,detail.poly.surv)
old.gbb <- assign.strata(old.41,detail.poly.surv)
oldest.gbb <- assign.strata(oldest.41,detail.poly.surv)

# detail.poly.surv hasn't changed, why was this put in the wrong stratum? Check 2019 run of survey summary

gbb_sf <- detail.poly.surv %>% sf::st_as_sf(coords=c(x="X", y="Y"), crs=4326) %>%
  group_by(label, Strata_ID, startyear, PID, SID) %>%
  summarize(do_union=F) %>% 
  st_cast("POLYGON") %>%
  ungroup() %>%
  group_by(label, Strata_ID, startyear, PID) %>%
  summarize(do_union=F) %>%
  st_cast("MULTIPOLYGON")

ggplot() + geom_sf(data=gbb_sf, aes(fill=as.factor(Strata_ID))) + 
  geom_point(data=old.41, aes(slon, slat), colour="red") + 
  geom_point(data=new.41, aes(slon, slat), colour="blue") +
  geom_point(data=oldest.41, aes(slon, slat), colour="green") + xlim(-66.16, -66.15) + ylim(41.995, 42.01) +
  geom_point(data=old.41, aes(elon, elat), colour="red") + 
  geom_point(data=new.41, aes(elon, elat), colour="blue") +
  geom_point(data=oldest.41, aes(elon, elat), colour="green")

# assign.strata uses START position to place stn in stratum. The start position for this tow has changed slightly since being entered in the db, 
# such that the tow is now being placed in stratum 101. Looking at the midpoint coords and endpoints, it should have been in 101 all along. 
# Need to check if 1992 had the appropriate tow allocation by strata

table(bank.dat.db2$GBb[bank.dat.db2$GBb$year==1992 & bank.dat.db2$GBb$state=="live",]$Strata_ID)
table(bank.dat.2021$GBb[bank.dat.2021$GBb$year==1992 & bank.dat.2021$GBb$state=="live",]$Strata_ID)
table(bank.dat.2019$GBb[bank.dat.2019$GBb$year==1992 & bank.dat.2019$GBb$state=="live",]$Strata_ID)

table(bank.dat.db2$GBb[bank.dat.db2$GBb$year==1991 & bank.dat.db2$GBb$state=="live",]$Strata_ID)
table(bank.dat.2021$GBb[bank.dat.2021$GBb$year==1991 & bank.dat.2021$GBb$state=="live",]$Strata_ID)
table(bank.dat.2019$GBb[bank.dat.2019$GBb$year==1991 & bank.dat.2019$GBb$state=="live",]$Strata_ID)

table(bank.dat.db2$GBb[bank.dat.db2$GBb$year==1993 & bank.dat.db2$GBb$state=="live",]$Strata_ID)
table(bank.dat.2021$GBb[bank.dat.2021$GBb$year==1993 & bank.dat.2021$GBb$state=="live",]$Strata_ID)
table(bank.dat.2019$GBb[bank.dat.2019$GBb$year==1993 & bank.dat.2019$GBb$state=="live",]$Strata_ID)
# don't you need the same number of tows in each stratum each year? 

# Sab 1984
table(MW.dat.db2[MW.dat.db2$bank=="Sab" & MW.dat.db2$year==1984,]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$bank=="Sab" & MW.dat.new.db2$year==1984,]$tow)
table(MW.dat.2021[MW.dat.2021$bank=="Sab" & MW.dat.2021$year==1984,]$tow)
table(MW.dat.new.2021[MW.dat.new.2021$bank=="Sab" & MW.dat.new.2021$year==1984,]$tow)

table(MW.dat.new.db2[MW.dat.new.db2$bank=="Sab" & MW.dat.new.db2$year==1984,]$month)
table(MW.dat.2021[MW.dat.2021$bank=="Sab" & MW.dat.2021$year==1984 & !MW.dat.2021$tow==0,]$month)

ggplot() + geom_sf(data=offshore[offshore$label=="Sab",]) + theme_minimal() +
  geom_text(data=MW.dat.new.db2[MW.dat.new.db2$bank=="Sab" & MW.dat.new.db2$year==1984,], aes(lon, lat, label=tow), colour="red") + 
  geom_text(data=MW.dat.2021[MW.dat.2021$bank=="Sab" & MW.dat.2021$year==1984 & !MW.dat.2021$tow==0,], aes(lon, lat, label=tow), colour="blue") +
  ggtitle("Sable 1984 (blue is pre-update, red is current database")

# GBa 1984
table(MW.dat.db2[MW.dat.db2$bank=="GBa" & MW.dat.db2$year==1984,]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$bank=="GBa" & MW.dat.new.db2$year==1984,]$tow)
table(MW.dat.2021[MW.dat.2021$bank=="GBa" & MW.dat.2021$year==1984,]$tow)
table(MW.dat.new.2021[MW.dat.new.2021$bank=="GBa" & MW.dat.new.2021$year==1984,]$tow)

table(MW.dat.new.db2[MW.dat.new.db2$bank=="GBa" & MW.dat.new.db2$year==1984,]$month)
table(MW.dat.2021[MW.dat.2021$bank=="GBa" & MW.dat.2021$year==1984 & !MW.dat.2021$tow==0,]$month)

table(MW.dat.new.db2[MW.dat.new.db2$bank=="GBa" & MW.dat.new.db2$year==1984,]$month)
table(MW.dat.2021[MW.dat.2021$bank=="GBa" & MW.dat.2021$year==1984 & !MW.dat.2021$tow==0,]$month)

MW.dat.new.db2[MW.dat.new.db2$bank=="GBa" & MW.dat.new.db2$year==1984 & MW.dat.new.db2$tow %in% c(13,8,3),]
unique(MW.dat.2021[MW.dat.2021$bank=="GBa" & MW.dat.2021$year==1984 & MW.dat.2021$tow %in% c(13,8,3),c("tow", "month", "day")])
MW.dat.new.db2[MW.dat.new.db2$bank=="GBa" & MW.dat.new.db2$year==1984 & MW.dat.new.db2$tow %in% 173,]
MW.dat.2021[MW.dat.2021$bank=="GBa" & MW.dat.2021$year==1984 & MW.dat.2021$tow %in% 173,]

ggplot() + geom_sf(data=offshore[offshore$label=="GBa",]) + theme_minimal() +
  geom_text(data=MW.dat.new.db2[MW.dat.new.db2$bank=="GBa" & MW.dat.new.db2$year==1984,], aes(lon, lat, label=tow), colour="red") + 
  geom_text(data=MW.dat.2021[MW.dat.2021$bank=="GBa" & MW.dat.2021$year==1984 & !MW.dat.2021$tow==0,], aes(lon, lat, label=tow), colour="blue") +
  ggtitle("GBa 1984 (blue is pre-update, red is current database") + facet_wrap(~month)

# GBa 1985
table(MW.dat.db2[MW.dat.db2$bank=="GBb" & MW.dat.db2$year==1985,]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$bank=="GBb" & MW.dat.new.db2$year==1985,]$tow)
table(MW.dat.2021[MW.dat.2021$bank=="GBb" & MW.dat.2021$year==1985,]$tow)
table(MW.dat.new.2021[MW.dat.new.2021$bank=="GBb" & MW.dat.new.2021$year==1985,]$tow)

table(MW.dat.new.db2[MW.dat.new.db2$bank=="GBb" & MW.dat.new.db2$year==1985,]$month)
table(MW.dat.2021[MW.dat.2021$bank=="GBb" & MW.dat.2021$year==1985 & !MW.dat.2021$tow==0,]$month)

ggplot() + geom_sf(data=offshore[offshore$label=="GBb",]) + theme_minimal() +
  geom_text(data=MW.dat.new.db2[MW.dat.new.db2$bank=="GBb" & MW.dat.new.db2$year==1985,], aes(lon, lat, label=tow), colour="red") + 
  geom_text(data=MW.dat.2021[MW.dat.2021$bank=="GBb" & MW.dat.2021$year==1985 & !MW.dat.2021$tow==0,], aes(lon, lat, label=tow), colour="blue") +
  ggtitle("GBb 1985 (blue is pre-update, red is current database)") #+ facet_wrap(~month)

ggplot() + geom_sf(data=offshore[offshore$label %in% c("GBa", "GBb"),]) + theme_minimal() +
  geom_text(data=MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb") & MW.dat.new.db2$year==1985,], aes(lon, lat, label=tow), colour="red") + 
  geom_text(data=MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb") & MW.dat.2021$year==1985 & !MW.dat.2021$tow==0,], aes(lon, lat, label=tow), colour="blue") +
  ggtitle("GBa/GBb 1985 (blue is pre-update, red is current database)") + facet_wrap(~cruise)

# GBa 1987
table(MW.dat.db2[MW.dat.db2$bank=="GBa" & MW.dat.db2$year==1987,]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$bank=="GBa" & MW.dat.new.db2$year==1987,]$tow)
table(MW.dat.2021[MW.dat.2021$bank=="GBa" & MW.dat.2021$year==1987,]$tow)
table(MW.dat.new.2021[MW.dat.new.2021$bank=="GBa" & MW.dat.new.2021$year==1987,]$tow)
table(MW.dat.2021[MW.dat.2021$cruise=="P387",]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$cruise=="P387",]$tow)

table(MW.dat.new.db2[MW.dat.new.db2$bank=="GBa" & MW.dat.new.db2$year==1987,]$month)
table(MW.dat.2021[MW.dat.2021$bank=="GBa" & MW.dat.2021$year==1987 & !MW.dat.2021$tow==0,]$month)

ggplot() + geom_sf(data=offshore[offshore$label %in% c("GBa", "GBb"),]) + theme_minimal() +
  geom_text(data=MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb") & MW.dat.new.db2$year==1987,], aes(lon, lat, label=tow), colour="red") + 
  geom_text(data=MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb") & MW.dat.2021$year==1987 & !MW.dat.2021$tow==0,], aes(lon, lat, label=tow), colour="blue") +
  ggtitle("GBa/GBb 1987 (blue is pre-update, red is current database)") + facet_wrap(~cruise)

# GBa/GBb 1988
table(MW.dat.db2[MW.dat.db2$bank %in% c("GBa", "GBb") & MW.dat.db2$year==1988,]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb") & MW.dat.new.db2$year==1988,]$tow)
table(MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb") & MW.dat.2021$year==1988,]$tow)
table(MW.dat.new.2021[MW.dat.new.2021$bank %in% c("GBa", "GBb") & MW.dat.new.2021$year==1988,]$tow)
table(MW.dat.2021[MW.dat.2021$cruise=="P371",]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$cruise=="P371",]$tow)

table(MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb")  & MW.dat.new.db2$year==1988,]$month)
table(MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb")  & MW.dat.2021$year==1988 & !MW.dat.2021$tow==0,]$month)

ggplot() + geom_sf(data=offshore[offshore$label %in% c("GBa", "GBb"),]) + theme_minimal() +
  geom_text(data=MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb") & MW.dat.new.db2$year==1988,], aes(lon, lat, label=tow), colour="red") + 
  geom_text(data=MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb") & MW.dat.2021$year==1988 & !MW.dat.2021$tow==0,], aes(lon, lat, label=tow), colour="blue") +
  ggtitle("GBa/GBb 1988 (blue is pre-update, red is current database)") + facet_wrap(~month)

# GBa/GBb 1989
table(MW.dat.db2[MW.dat.db2$bank %in% c("GBa", "GBb", "GB") & MW.dat.db2$year==1989,]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.db2$year==1989,]$tow)
table(MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.2021$year==1989,]$tow)
table(MW.dat.new.2021[MW.dat.new.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.2021$year==1989,]$tow)
table(MW.dat.2021[MW.dat.2021$cruise=="P384",]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$cruise=="P384",]$tow)

table(MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb", "GB")  & MW.dat.new.db2$year==1989,]$month)
table(MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb", "GB")  & MW.dat.2021$year==1989 & !MW.dat.2021$tow==0,]$month)

ggplot() + geom_sf(data=offshore[offshore$label %in% c("GBa", "GBb", "GB"),]) + theme_minimal() +
  geom_text(data=MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.db2$year==1989,], aes(lon, lat, label=tow), colour="red") + 
  geom_text(data=MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.2021$year==1989 & !MW.dat.2021$tow==0,], aes(lon, lat, label=tow), colour="blue") +
  ggtitle("GBa/GBb 1989 (blue is pre-update, red is current database)") + facet_wrap(~month)


# GBa/GBb 2006
table(MW.dat.db2[MW.dat.db2$bank %in% c("GBa", "GBb", "GB") & MW.dat.db2$year==2006,]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.db2$year==2006,]$tow)
table(MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.2021$year==2006,]$tow)
table(MW.dat.new.2021[MW.dat.new.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.2021$year==2006,]$tow)
table(MW.dat.new.2021[MW.dat.new.2021$cruise=="CK32",]$tow)
table(MW.dat.new.db2[MW.dat.new.db2$cruise=="CK32",]$tow)

table(MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb", "GB")  & MW.dat.new.db2$year==2006,]$month)
table(MW.dat.new.2021[MW.dat.new.2021$bank %in% c("GBa", "GBb", "GB")  & MW.dat.new.2021$year==2006 & !MW.dat.new.2021$tow==0,]$month)

ggplot() + geom_sf(data=offshore[offshore$label %in% c("GBa", "GBb", "GB"),]) + theme_minimal() +
  geom_text(data=unique(MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.db2$year==2006,c("lon", "lat", "tow", "month")]), aes(lon, lat, label=tow), colour="red", position=position_jitter(width=0.01, height=0.01)) + 
  geom_text(data=MW.dat.new.2021[MW.dat.new.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.2021$year==2006 & !MW.dat.new.2021$tow==0,], aes(lon, lat, label=tow), colour="blue") +
  ggtitle("GBa/GBb 2006 (blue is pre-update, red is current database)") + facet_wrap(~month)


# GB 1988
# surv.dat[[bnk]]$random[surv.dat[[bnk]]$tow > 312 & surv.dat[[bnk]]$year == 1988] <- 4

arrange(unique(surv.dat.2021$GB[surv.dat.2021$GB$year==1988, c("month", "tow", "random")]), tow)
arrange(unique(surv.dat.db2$GB[surv.dat.db2$GB$year==1988, c("month", "tow", "random")]), tow)

survey.obj.2021$GB$model.dat$n[survey.obj.2021$GB$model.dat$year==1988]
survey.obj.db2$GB$model.dat$n[survey.obj.db2$GB$model.dat$year==1988]
survey.obj.2021$GB$model.dat$I[survey.obj.2021$GB$model.dat$year==1988]
survey.obj.db2$GB$model.dat$I[survey.obj.db2$GB$model.dat$year==1988]
survey.obj.2021$GB$model.dat$CF[survey.obj.2021$GB$model.dat$year==1988]
survey.obj.db2$GB$model.dat$CF[survey.obj.db2$GB$model.dat$year==1988]
survey.obj.2021$GB$model.dat$N[survey.obj.2021$GB$model.dat$year==1988]
survey.obj.db2$GB$model.dat$N[survey.obj.db2$GB$model.dat$year==1988]
dim(surv.dat.2021$GB[surv.dat.2021$GB$year==1988,])
dim(surv.dat.db2$GB[surv.dat.db2$GB$year==1988,])
MW.dat.new.2021[MW.dat.new.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.2021$year==1988,]
unique(MW.dat.2021[MW.dat.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.2021$year==1988,]$cruise)
unique(MW.dat.new.db2[MW.dat.new.db2$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.db2$year==1988,]$cruise)
unique(MW.dat.db2[MW.dat.db2$bank %in% c("GBa", "GBb", "GB") & MW.dat.db2$year==1988,]$cruise)
dim(mw.dat.all.2021$GB[mw.dat.all.2021$GB$year==1988,])
dim(mw.dat.all.db2$GB[mw.dat.all.db2$GB$year==1988,])
cf.data.2021$GB$CF.data[cf.data.2021$GB$CF.data$year==1988,]
cf.data.db2$GB$CF.data[cf.data.db2$GB$CF.data$year==1988,]
dim(subset(surv.dat.2021$GB,state=='live' & random==3))
dim(subset(surv.dat.db2$GB,state=='live' & random==3))


surv.dat.2021$GB[surv.dat.2021$GB$year==1988 & surv.dat.2021$GB$tow %in% c(1,26),]
surv.dat.db2$GB[surv.dat.db2$GB$year==1988 & surv.dat.db2$GB$tow %in% c(301,326),]
# 1988 gets condition estimate because only tow type 3s are used for analysis and there had been a tow type 3 tow with no depth info



# sable 1992
length(unique(surv.dat.2021$Sab[surv.dat.2021$Sab$year==1992,]$tow))
length(unique(surv.dat.db2$Sab[surv.dat.db2$Sab$year==1992,]$tow))
length(unique(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992,]$tow))
length(unique(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992,]$tow))
table(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992,]$random)
table(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992,]$random)
table(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992,]$month)
table(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992,]$month)
summary(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992,]$CF)
summary(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992,]$CF)
summary(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992,]$date)
summary(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992,]$date)
summary(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992,]$date)
summary(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992,]$date)

survey.obj.2021$Sab$model.dat$n[survey.obj.2021$Sab$model.dat$year==1992]
survey.obj.db2$Sab$model.dat$n[survey.obj.db2$Sab$model.dat$year==1992]
survey.obj.2021$Sab$model.dat$N[survey.obj.2021$Sab$model.dat$year==1992]
survey.obj.db2$Sab$model.dat$N[survey.obj.db2$Sab$model.dat$year==1992]
summary(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992,]$Strata_ID_new)
summary(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992,]$Strata_ID_old)
summary(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992,]$Strata_ID_new)
summary(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992,]$Strata_ID_old)
surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992 & is.na(surv.Rand.db2$Sab$Strata_ID_new) & !is.na(surv.Rand.db2$Sab$Strata_ID_old),]
#tow 47!
surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992 & surv.Rand.2021$Sab$tow==47, c("slat", "slon", "elat", "elon")]
surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992 & surv.Rand.db2$Sab$tow==47, c("slat", "slon", "elat", "elon")]

sabsf2021 <- st_as_sf(surv.Rand.2021$Sab, coords=c(X="lon", Y="lat"), crs=4326)
sabsfdb2 <- st_as_sf(surv.Rand.db2$Sab, coords=c(X="lon", Y="lat"), crs=4326)



ggplot() + geom_sf(data=offshore[offshore$label=="Sab",]) +
  # geom_segment(data=surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992 & surv.Rand.2021$Sab$tow==47, ],
  #              aes(x=slon, xend=elon, y=slat, yend=slat), colour="red") +
  # geom_segment(data=surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992 & surv.Rand.db2$Sab$tow==47, ],
  #              aes(x=slon, xend=elon, y=slat, yend=slat), colour="blue") +
  geom_point(data=surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992 & surv.Rand.2021$Sab$tow==47,],
             aes(x=slon, y=slat), colour="red") +
  geom_point(data=surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992 & surv.Rand.db2$Sab$tow==47,],
             aes(x=slon, y=slat), colour="blue") +
  coord_sf(xlim=c(-61.806, -61.808), ylim=c(43.135, 43.136))

ggplot() + geom_sf(data=offshore[offshore$label=="Sab",]) +
  geom_segment(data=surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==2018 & surv.Rand.2021$Sab$tow==47, ],
               aes(x=slon, xend=elon, y=slat, yend=slat), colour="red") +
  geom_segment(data=surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==2018 & surv.Rand.db2$Sab$tow==47, ],
               aes(x=slon, xend=elon, y=slat, yend=slat), colour="blue") +
  geom_point(data=surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==2018 & surv.Rand.2021$Sab$tow==47,],
             aes(x=lon, y=lat), colour="red") +
  geom_point(data=surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==2018 & surv.Rand.db2$Sab$tow==47,],
             aes(x=lon, y=lat), colour="blue") +
  coord_sf(xlim=c(-61.3, -61.5), ylim=c(43.2, 43.3))


ggplot() + geom_sf(data=offshore[offshore$label=="BBn",]) +
  geom_segment(data=surv.Rand.2021$BBn[surv.Rand.2021$BBn$year==2018 & surv.Rand.2021$BBn$tow==201, ],
               aes(x=slon, xend=elon, y=slat, yend=slat), colour="red") +
  geom_segment(data=surv.Rand.db2$BBn[surv.Rand.db2$BBn$year==2018 & surv.Rand.db2$BBn$tow==201, ],
               aes(x=slon, xend=elon, y=slat, yend=slat), colour="blue") +
  geom_point(data=surv.Rand.2021$BBn[surv.Rand.2021$BBn$year==2018 & surv.Rand.2021$BBn$tow==201,],
             aes(x=slon, y=slat), colour="red") +
  geom_point(data=surv.Rand.db2$BBn[surv.Rand.db2$BBn$year==2018 & surv.Rand.db2$BBn$tow==201,],
             aes(x=lon, y=lat), colour="blue") +
  coord_sf(xlim=c(-65.85, -65.95), ylim=c(42.55, 42.60))

surv.Rand.db2$BBn[surv.Rand.db2$BBn$year==1992 & surv.Rand.db2$BBn$tow==201, c("lon", "lat", "slon", "slat", "elon", "elat")]
surv.Rand.2021$BBn[surv.Rand.2021$BBn$year==1992 & surv.Rand.2021$BBn$tow==201, c("lon", "lat", "slon", "slat", "elon", "elat")]
  
surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992 & surv.Rand.db2$Sab$tow==47, c("lon", "lat", "slon", "slat", "elon", "elat")]
surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992 & surv.Rand.2021$Sab$tow==47, c("lon", "lat", "slon", "slat", "elon", "elat")]

pt1 <- st_as_sf(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1992 & surv.Rand.db2$Sab$tow==47,], coords=c(X="slon", Y="slat"), crs=4326) %>% st_transform(32620)
pt2 <- st_as_sf(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1992 & surv.Rand.2021$Sab$tow==47,], coords=c(X="slon", Y="slat"), crs=4326) %>% st_transform(32620)
st_distance(pt1, pt2)
# Tow 47 from 1992 was excluded because the start point was moved 4m, and now it is getting placed outside the survey domain by the assign_strata function (which uses start points)
# also of note, it looks like our lon/lat calculations based on slon/slat/elon/elat are weird, but this has been the same all along?

ggplot() + geom_segment(data=NULL, aes(x=-61.807, xend=-61.80367, y=43.13583, yend=43.147)) +
  geom_point(data=NULL, aes(x=mean(c(-61.807, -61.80367)), y=mean(c(43.13583, 43.147)))) + 
  coord_sf()

mean(c(-61.807, -61.80367))
mean(c(43.13583, 43.147))

seg <- st_as_sf(data.frame(X=c(-61.807, -61.80367), Y=c(43.13583, 43.147)), coords=c("X", "Y"), crs=4326) %>% summarize() %>% st_cast("LINESTRING")
pt <- st_as_sf(data.frame(X=mean(c(-61.807, -61.80367)), Y=mean(c(43.13583, 43.147))), coords=c("X", "Y"), crs=4326)

ggplot() + geom_sf(data=seg) + geom_sf(data=pt) +
  geom_segment(data=NULL, aes(x=-61.807, xend=-61.80367, y=43.13583, yend=43.147), lty=2, lwd=2)

# never mind, seems fine? moving on...

# Sable 1999

survey.obj.2021$Sab$model.dat[survey.obj.2021$Sab$model.dat$year==1999,]
survey.obj.db2$Sab$model.dat[survey.obj.db2$Sab$model.dat$year==1999,]
length(unique(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1999,]$tow))
length(unique(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1999,]$tow))
table(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1999,]$Strata_ID_new)
table(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1999,]$Strata_ID_new)
table(surv.Rand.2021$Sab[surv.Rand.2021$Sab$year==1999,]$Strata_ID_old)
table(surv.Rand.db2$Sab[surv.Rand.db2$Sab$year==1999,]$Strata_ID_old)
# Must be the same issue. Tows getting INCLUDED here that weren't before. 

# Sable 1989
length(unique(surv.dat.2021$Sab[surv.dat.2021$Sab$year==1989,]$tow))
length(unique(surv.dat.db2$Sab[surv.dat.db2$Sab$year==1989,]$tow))
unique(surv.dat.db2$Sab[surv.dat.db2$Sab$year==1989,]$tow)[which(!unique(surv.dat.db2$Sab[surv.dat.db2$Sab$year==1989,]$tow) %in% unique(surv.dat.2021$Sab[surv.dat.2021$Sab$year==1989,]$tow))]
surv.dat.db2$Sab[surv.dat.db2$Sab$tow %in% c(100:106),]

sabdb2 <- st_as_sf(surv.dat.db2$Sab[surv.dat.db2$Sab$year==1989,], coords=c("lon", "lat"), crs=4326)
sab2021 <- st_as_sf(surv.dat.2021$Sab[surv.dat.2021$Sab$year==1989,], coords=c("lon", "lat"), crs=4326)

ggplot() + geom_sf(data=offshore[offshore$label=="Sab",]) +
  geom_sf_text(data=sabdb2, colour="red", aes(label=tow)) +
  geom_sf_text(data=sab2021, colour="blue",aes(label=tow)) +
  ggtitle("Sable 1992, blue is before, red is after.\nBlue plotted over red.")
# so these tows were added to the db, but don't affect analysis since they are way outside domain

# BBn 2005
survey.obj.2021$BBn$model.dat[survey.obj.2021$BBn$model.dat$year==2005,]$n
survey.obj.db2$BBn$model.dat[survey.obj.db2$BBn$model.dat$year==2005,]$n
length(unique(surv.dat.2021$BBn[surv.dat.2021$BBn$year==2005,]$tow))
length(unique(surv.dat.db2$BBn[surv.dat.db2$BBn$year==2005,]$tow))
table(surv.dat.2021$BBn[surv.dat.2021$BBn$year==2005,]$Strata_ID)
table(surv.dat.db2$BBn[surv.dat.db2$BBn$year==2005,]$Strata_ID)
table(surv.dat.2021$BBn[surv.dat.2021$BBn$year==2005,]$cruise)
table(surv.dat.db2$BBn[surv.dat.db2$BBn$year==2005,]$cruise)
table(surv.Rand.db2$BBn[surv.Rand.db2$BBn$year==2005,]$tow)
table(surv.Rand.2021$BBn[surv.Rand.2021$BBn$year==2005,]$tow)

unique(surv.dat.db2$BBn[surv.dat.db2$BBn$year==2005,]$tow)[which(!unique(surv.dat.db2$BBn[surv.dat.db2$BBn$year==2005,]$tow) %in% unique(surv.dat.2021$BBn[surv.dat.2021$BBn$year==2005,]$tow))]

sabdb2 <- st_as_sf(surv.dat.db2$BBn[surv.dat.db2$BBn$year==1989,], coords=c("lon", "lat"), crs=4326)
sab2021 <- st_as_sf(surv.dat.2021$BBn[surv.dat.2021$BBn$year==1989,], coords=c("lon", "lat"), crs=4326)

#remove!
all.surv.dat.db2[all.surv.dat.db2$cruise=="CK29",]$bank


for(i in names(surv.dat.2021)){
  print(i)
  print(dim(surv.dat.2021[[i]][is.na(surv.dat.2021[[i]]$date),])[1]/dim(surv.dat.2021[[i]])[1])
}
for(i in names(surv.dat.db2)){
  print(i)
  print(dim(surv.dat.db2[[i]][is.na(surv.dat.db2[[i]]$date),])[1]/dim(surv.dat.db2[[i]])[1])
}



for (i in 1983:2000){
  table1 <- table(cruise.mw.db[cruise.mw.db$year==i,]$bank,cruise.mw.db[cruise.mw.db$year==i,]$month)
  table2 <- table(cruise.mw.21[cruise.mw.21$year==i,]$bank,cruise.mw.21[cruise.mw.21$year==i,]$month)
  
  print(i)
  print(table1)
  print(table2)
  
}




unique(MW.dat.new.db[MW.dat.new.db$cruise=="CK04" & MW.dat.new.db$bank=="BBn" & MW.dat.new.db$tow==251,]$lon)
unique(MW.dat.2021[MW.dat.2021$cruise=="CK04" & MW.dat.2021$bank=="BBn" & MW.dat.2021$tow==251,]$lon)


tail(testdb[testdb$cruise=="CK04",])
dim(testdb[testdb$cruise=="CK04",])
dim(unique(testdb[testdb$cruise=="CK04",]))

tail(test21[test21$cruise=="CK04",])
dim(test21[test21$cruise=="CK04",])
dim(unique(test21[test21$cruise=="CK04",]))
test21[test21$cruise=="CK04",]$tow


unique(missing_21[missing_21$bank=="BBs",]$tow)
unique(missing_db[missing_db$bank=="BBs",]$tow)
table(missing_db[missing_db$bank=="BBn",]$cruise,missing_db[missing_db$bank=="BBn",]$tow)
table(missing_21[missing_21$bank=="BBn",]$cruise,missing_21[missing_21$bank=="BBn",]$tow)

unique(MW.dat.new.2021[MW.dat.new.2021$year==1999,]$bank)
unique(MW.dat.new.2021[MW.dat.new.2021$year==1999,]$cruise)
all.surv.dat.2021[all.surv.dat.2021$cruise=="CK12",]

table(MW.dat.new.db[MW.dat.new.db$year==1999,]$bank,MW.dat.new.db[MW.dat.new.db$year==1999,]$month)
table(MW.dat.new.2021[MW.dat.new.2021$year==1999,]$bank,MW.dat.new.2021[MW.dat.new.2021$year==1999,]$month)
table(MW.dat.2021[MW.dat.2021$year==1999,]$bank,MW.dat.2021[MW.dat.2021$year==1999,]$month)

head(missing_21)
head(missing_db)
cruises_added <- missing_21 %>%
  dplyr::select(bank, cruise, year) %>%
  distinct() %>%
  arrange(year, bank, cruise)

write.csv(file="C:/Users/keyserf/Documents/temp_data/cruises_added2.csv", cruises_added)

cruises_removed <- missing_db %>%
  dplyr::select(bank, cruise, year) %>%
  distinct()

write.csv(file="C:/Users/keyserf/Documents/temp_data/cruises_removed2.csv", cruises_removed)


dim(unique(testdb))
dim(unique(test21))

unique(testdb$cruise)[which(!unique(testdb$cruise) %in% unique(test21$cruise))]

testdb[testdb$cruise=="P290",]$year
testdb[testdb$cruise=="P306",]$year
dim(testdb[testdb$cruise %in% c("P290", "P306"),])


weights82 <- read.csv("Y:/Offshore/Assessment/Data/Hydration/Weights_82-92.csv")
weights92 <- read.csv("Y:/Offshore/Assessment/Data/Hydration/Weights_92-08.csv")

mw.rows <- NULL
missing_from_csv <- NULL
for(i in 1:nrow(cruises_added)){
  cruisei <- cruises_added$cruise[i]
  banki <- cruises_added$bank[i]
  yeari <- cruises_added$year[i]
  
  if(!banki=="GB") {
    rows <- data.frame(cruise=cruisei,
                       bank=banki, 
                       year=yeari,
                       MW.dat.new.2021 = dim(MW.dat.new.2021[MW.dat.new.2021$bank %in% banki & MW.dat.new.2021$cruise==cruisei & !is.na(MW.dat.new.2021$bank),])[1],
                       MW.dat.2021 = dim(MW.dat.2021[MW.dat.2021$cruise==cruisei & MW.dat.2021$bank %in% banki,])[1], 
                       mw.2021 = dim(mw.2021[[banki]][mw.2021[[banki]]$cruise==cruisei,])[1],
                       cruise.mw.21 = dim(cruise.mw.21[cruise.mw.21$cruise==cruisei & cruise.mw.21$bank %in% banki,])[1], 
                       cf.data.2021 = dim(cf.data.2021[[banki]]$CF.data[cf.data.2021[[banki]]$CF.data$year==yeari,])[1]
                       
                       
    )
    
    tows <- missing_21[missing_21$cruise==cruisei & missing_21$bank==banki & missing_21$year == yeari,]$tow
    if(banki=="BBs") tows <- as.numeric(tows)+500
    missing <- data.frame(cruise=cruisei,
                          bank=banki, 
                          year=yeari,
                          weights82 = dim(weights82[weights82$Cruise==cruisei & weights82$Stn %in% tows,])[1],
                          weights92 = dim(weights92[weights92$Cruise==cruisei & weights92$Stn %in% tows,])[1],
                          MW.dat.2021 = dim(MW.dat.2021[MW.dat.2021$cruise==cruisei & MW.dat.2021$tow %in% tows,])[1],
                          MW.dat.new.db = dim(MW.dat.new.db2[MW.dat.new.db2$cruise==cruisei & MW.dat.new.db2$tow %in% tows,])[1],
                          mw.2021 = dim(mw.2021[[banki]][mw.2021[[banki]]$cruise==cruisei & mw.2021[[banki]]$tow %in% tows,])[1],
                          mw.db = dim(mw.db2[[banki]][mw.db2[[banki]]$cruise==cruisei & mw.db2[[banki]]$tow %in% tows,])[1],
                          cruise.mw.21 = dim(cruise.mw.21[cruise.mw.21$cruise==cruisei & cruise.mw.21$tow %in% tows,])[1],
                          cruise.mw.db = dim(cruise.mw.db[cruise.mw.db$cruise==cruisei & cruise.mw.db$tow %in% tows,])[1],
                          cf.data.2021 = dim(cf.data.2021[[banki]]$CF.data[cf.data.2021[[banki]]$CF.data$year==yeari & cf.data.2021[[banki]]$CF.data$tow %in% tows,])[1],
                          cf.data.db = dim(cf.data.db2[[banki]]$CF.data[cf.data.db2[[banki]]$CF.data$year==yeari & cf.data.db2[[banki]]$CF.data$tow %in% tows,])[1]
    )
  }
  
  
  if(banki=="GB") {
    rows <- data.frame(cruise=cruisei,
                       bank=banki, 
                       year=yeari,
                       MW.dat.new.2021 = dim(MW.dat.new.2021[MW.dat.new.2021$bank %in% c("GBa", "GBb", "GB") & MW.dat.new.2021$cruise==cruisei & !is.na(MW.dat.new.2021$bank),])[1],
                       MW.dat.2021 = dim(MW.dat.2021[MW.dat.2021$cruise==cruisei & MW.dat.2021$bank %in% c("GBa", "GBb", "GB"),])[1], 
                       mw.2021 = dim(mw.2021[[banki]][mw.2021[[banki]]$cruise==cruisei,])[1],
                       cruise.mw.21 = dim(cruise.mw.21[cruise.mw.21$cruise==cruisei & cruise.mw.21$bank %in% c("GBa", "GBb", "GB"),])[1], 
                       cf.data.2021 = dim(cf.data.2021[[banki]]$CF.data[cf.data.2021[[banki]]$CF.data$year==yeari,])[1]
    )
    
    tows <- missing_21[missing_21$cruise==cruisei & missing_21$bank==banki & missing_21$year == yeari,]$tow
    missing <- data.frame(cruise=cruisei,
                          bank=banki, 
                          year=yeari,
                          weights82 = dim(weights82[weights82$Cruise==cruisei & weights82$Stn %in% tows,])[1],
                          weights92 = dim(weights92[weights92$Cruise==cruisei & weights92$Stn %in% tows,])[1],
                          MW.dat.2021 = dim(MW.dat.2021[MW.dat.2021$cruise==cruisei & MW.dat.2021$tow %in% tows,])[1],
                          MW.dat.new.db = dim(MW.dat.new.db2[MW.dat.new.db2$cruise==cruisei & MW.dat.new.db2$tow %in% tows,])[1],
                          mw.2021 = dim(mw.2021[[banki]][mw.2021[[banki]]$cruise==cruisei & mw.2021[[banki]]$tow %in% tows,])[1],
                          mw.db = dim(mw.db2[[banki]][mw.db2[[banki]]$cruise==cruisei & mw.db2[[banki]]$tow %in% tows,])[1],
                          cruise.mw.21 = dim(cruise.mw.21[cruise.mw.21$cruise==cruisei & cruise.mw.21$tow %in% tows,])[1],
                          cruise.mw.db = dim(cruise.mw.db[cruise.mw.db$cruise==cruisei & cruise.mw.db$tow %in% tows,])[1],
                          cf.data.2021 = dim(cf.data.2021[[banki]]$CF.data[cf.data.2021[[banki]]$CF.data$year==yeari & cf.data.2021[[banki]]$CF.data$tow %in% tows,])[1],
                          cf.data.db = dim(cf.data.db2[[banki]]$CF.data[cf.data.db2[[banki]]$CF.data$year==yeari & cf.data.db2[[banki]]$CF.data$tow %in% tows,])[1]
    )
  }
  
  mw.rows <- rbind(mw.rows, rows)
  missing_from_csv <- rbind(missing_from_csv, missing)
}

missing_from_csv$MW.dat.2021==(missing_from_csv$weights82+missing_from_csv$weights92)

missing_from_csv[missing_from_csv$MW.dat.2021>0,]
