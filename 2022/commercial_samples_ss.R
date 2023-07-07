years_f <- 2018:2022

df1 <- NULL
df2 <- NULL
df3 <- NULL
df4 <- NULL
for(q in 1:length(years_f)){
  print(q)
  #if(years_f[q]==2017) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_spring_results.Rdata"))
    if(years_f[q]==2018) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results_FINAL.Rdata"))
      if(years_f[q]==2019) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results.Rdata"))
        if(years_f[q]==2020) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results.Rdata"))
          if(years_f[q]==2021) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results.Rdata"))
            if(years_f[q]==2022) load(paste0("Y:/Offshore/Assessment/Data/Survey_data/",years_f[q],"/Survey_summary_output/Survey_all_results.Rdata"))
  MW.dat$run <- years_f[q]
  MW.dat.new$run <- years_f[q]
  mw.dat.all$GBa$run <- years_f[q]
  cf.data$GBa$CF.data$run <- years_f[q]
  
  MW.dat$year <- as.numeric(as.character(MW.dat$year))
  MW.dat$tow <- as.numeric(as.character(MW.dat$tow))
  MW.dat.new$year <- as.numeric(as.character(MW.dat.new$year))
  MW.dat.new$tow <- as.numeric(as.character(MW.dat.new$tow))
  mw.dat.all$GBa$year <- as.numeric(as.character(mw.dat.all$GBa$year))
  mw.dat.all$GBa$tow <- as.numeric(as.character(mw.dat.all$GBa$tow))
  cf.data$GBa$CF.data$tow <- as.numeric(as.character(cf.data$GBa$CF.data$tow))
  cf.data$GBa$CF.data$run <- as.numeric(as.character(cf.data$GBa$CF.data$run))
  
  if(q == 1){
    df1 <- MW.dat
    df2 <- MW.dat.new
    df3 <- mw.dat.all$GBa
    df4 <- cf.data$GBa$CF.data
  }
  if(q>1){
    df1 <- full_join(df1, MW.dat)
    df2 <- full_join(df2, MW.dat.new)
    df3 <- full_join(df3, mw.dat.all$GBa)
    df4 <- full_join(df4, cf.data$GBa$CF.data)
  }
}


MW.dat <- df1[df1$tow == 0,] %>%
  group_by(run, year, bank) %>%
  summarize(n=n())

df2[df2$tow == 0,] # EMPTY

ggplot() + 
  geom_line(data=MW.dat[MW.dat$bank=="GBa",], aes(year, n, colour=as.factor(run))) + facet_wrap(~run)

mw.dat.all <- df3[df3$tow==0,] %>%
  group_by(run, year, bank) %>%
  summarize(n=n())

ggplot() + 
  geom_line(data=mw.dat.all, aes(year, n, colour=as.factor(run))) + facet_wrap(~run)

CF.data <- df4[df4$tow==0,]

ggplot() + 
  geom_line(data=CF.data, aes(year, CF, colour=as.factor(run), group=as.factor(run))) + facet_wrap(~run)

dim(MW.dat[MW.dat$run==2018,])
dim(MW.dat[MW.dat$run==2019,])
dim(MW.dat[MW.dat$run==2020,])
dim(MW.dat[MW.dat$run==2021,])
#85-67
# 18 tows missing?


sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="GBa",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="GBa",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="GBa",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="GBa",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="GBa",]$n, na.rm=T)
# missing 1560 from GBA

sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="GBb",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="GBb",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="GBb",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="GBb",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="GBb",]$n, na.rm=T)
#missing 180 from GBb

sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="BBn",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="BBn",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="BBn",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="BBn",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="BBn",]$n, na.rm=T)
# missing 330 from BBn

sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="BBs",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="BBs",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="BBs",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="BBs",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="BBs",]$n, na.rm=T)
# missing 60 from BBs

sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="Ger",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="Ger",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="Ger",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="Ger",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="Ger",]$n, na.rm=T)
# missing 90 from Ger

sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="Sab",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="Sab",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="Sab",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="Sab",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="Sab",]$n, na.rm=T)
# missing 60 from Sab

sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="Mid",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="Mid",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="Mid",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="Mid",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="Mid",]$n, na.rm=T)
# none missing from Mid

sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="Ban",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="Ban",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="Ban",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="Ban",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="Ban",]$n, na.rm=T)
# none missing from Ban

sum(MW.dat[MW.dat$run==2018 & MW.dat$year >2000 & MW.dat$bank=="GB",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2019 & MW.dat$year >2000 & MW.dat$bank=="GB",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2020 & MW.dat$year >2000 & MW.dat$bank=="GB",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2021 & MW.dat$year >2000 & MW.dat$bank=="GB",]$n, na.rm=T)
sum(MW.dat[MW.dat$run==2022 & MW.dat$year >2000 & MW.dat$bank=="GB",]$n, na.rm=T)
# none missing from Ban

