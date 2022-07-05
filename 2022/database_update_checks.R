# survey summary run pulling all survey data from SCALOFF
load("C:/Users/keyserf/Documents/temp_data/testing_results_historical_db.Rdata")

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
mw.db <- mw
MW.dat.db <- MW.dat

# survey summary run from 2021 (before historical data were loaded)
load("C:/Users/keyserf/Documents/temp_data/Survey_all_results - 2021FINAL.Rdata")
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
mw.2021 <- mw
MW.dat.2021 <- MW.dat

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
    print(ggplot() + geom_line(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
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
  if(i=="Ger") {
    print(ggplot() + 
            #geom_line(data=merged.survey.obj.2019, aes(year, I, colour="2019")) +
            geom_line(data=merged.survey.obj.2021, aes(year, I, colour="2021")) +
            geom_line(data=merged.survey.obj, aes(year, I, colour="database")) +
            #geom_point(data=merged.survey.obj.2019, aes(year, I, colour="2019")) +
            geom_point(data=merged.survey.obj.2021, aes(year, I, colour="2021")) +
            geom_point(data=merged.survey.obj, aes(year, I, colour="database")) +
            #geom_line(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
            geom_line(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_line(data=survey.obj[[i]]$model.dat, aes(year, I, colour="database")) +
            #geom_point(data=survey.obj.2019[[i]]$model.dat, aes(year, I, colour="2019")) +
            geom_point(data=survey.obj.2021[[i]]$model.dat, aes(year, I, colour="2021")) +
            geom_point(data=survey.obj[[i]]$model.dat, aes(year, I, colour="database")) +
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
View(samples.2[!(samples.2$old==samples.2$new),])

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

missing_21 <- NULL
missing_db <- NULL
cruise.mw.db <- NULL
cruise.mw.21 <-  NULL
for(i in c("BBn","Ger","Sab","BBs","GB")){
  print(i)
  testdb <- merge(
    subset(MW.dat.db[MW.dat.db$bank==i,], 
           month %in% 5:6 & year %in% 1985:2021,
           c("cruise", "ID","year",
             "lon","lat","depth",
             "sh","wmw","tow", "month")),
    subset(mw.db[[i]], (month %in% 5:6 & !year == 2015) | year==2015 | year==2000, 
           select=c("cruise", "ID","year",
                    "lon","lat","depth",
                    "sh","wmw","tow", "month")),
    all=T)
  
  test21 <- merge(
    subset(MW.dat.2021[MW.dat.2021$bank==i,], 
           month %in% 5:6 & year %in% 1985:2021,
           c("cruise", "year",
             "lon","lat","depth",
             "sh","wmw","tow", "month")),
    subset(mw.2021[[i]], (month %in% 5:6 & !year == 2015) | year==2015 | year==2000, 
           select=c("cruise", "ID","year",
                    "lon","lat","depth",
                    "sh","wmw","tow", "month")),
    all=T)
  test21$ID <- paste0(test21$cruise, ".", test21$tow)
  
  testdb$tow <- as.character(testdb$tow)
  
  testdb <- unique(dplyr::select(testdb, cruise, ID, year, tow, month))
  test21 <- unique(dplyr::select(test21, cruise, ID, year, tow, month))
  
  testdb$bank <- i
  test21$bank <- i
  
  not_in_21 <- anti_join(testdb, test21)
  not_in_db <- anti_join(test21, testdb)
  
  missing_21 <- rbind(missing_21, not_in_21)
  missing_db <- rbind(missing_db, not_in_db)
  
  cruise.mw.db <- rbind(cruise.mw.db, testdb)
  cruise.mw.21 <- rbind(cruise.mw.21, test21)
  
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

write.csv(file="C:/Users/keyserf/Documents/temp_data/cruises_added.csv", cruises_added)

cruises_removed <- missing_db %>%
  dplyr::select(bank, cruise, year) %>%
  distinct()

write.csv(file="C:/Users/keyserf/Documents/temp_data/cruises_removed.csv", cruises_removed)


dim(unique(testdb))
dim(unique(test21))

unique(testdb$cruise)[which(!unique(testdb$cruise) %in% unique(test21$cruise))]

testdb[testdb$cruise=="P290",]$year
testdb[testdb$cruise=="P306",]$year
dim(testdb[testdb$cruise %in% c("P290", "P306"),])

mw.rows <- NULL
for(i in 1:nrow(cruises_added)){
  cruisei <- cruises_added$cruise[i]
  banki <- cruises_added$bank[i]
  yeari <- cruises_added$year[i]
  
  rows <- data.frame(cruise=cruisei,
                     bank=banki, 
                     year=yeari,
                     MW.dat.new.2021 = dim(MW.dat.new.2021[MW.dat.new.2021$bank==banki & MW.dat.new.2021$cruise==cruisei & !is.na(MW.dat.new.2021$bank),])[1],
                     MW.dat.2021 = dim(MW.dat.2021[MW.dat.2021$cruise==cruisei & cruise.mw.21$bank==banki,])[1], 
                     mw.2021 = dim(mw.2021[[bank]][mw.2021[[bank]]$cruise==cruisei,])[1],
                     cruise.mw.21 = dim(cruise.mw.21[cruise.mw.21$cruise==cruisei & cruise.mw.21$bank==banki,])[1], 
                     cf.data.2021 = dim(cf.data.2021[[banki]]$CF.data[cf.data.2021[[banki]]$CF.data$year==yeari,])[1]
                     )
  mw.rows <- rbind(mw.rows, rows)
}

#import.hyd.data(yrs=1982:2000, export=F,dirt=direct)
