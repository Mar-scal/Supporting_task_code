
exported <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2021/Spring/GB/Survey1984-2021.csv")

load("Y:/Offshore/Assessment/Data/Survey_data/2021/Survey_summary_output/Survey_all_results.Rdata")

#all.suv.dat matches db?

#surv.dat matches SS output csv ("exported" above)?


all <- all.surv.dat 



table(all[all$bank=="GB" & all$year<2013,]$random)

# 1       2    3    5 
# 1080  116   96   44

all[all$bank=="GB",]$random[all[all$bank=="GB",]$year < 2013] <-4
table(all[all$bank=="GB",]$random)
# 1    2    3    4    5 
# 192    4  142 1364  144 

all[all$bank=="GB",]$random[all[all$bank=="GB",]$tow %in% c(1:24,301:324)] <-3
table(all[all$bank=="GB",]$random)
# 2    3    4    5 
# 4 1440  258  144 

all[all$bank=="GB",]$random[all[all$bank=="GB",]$tow > 12 & all[all$bank=="GB",]$year == 1988] <- 4 
table(all[all$bank=="GB",]$random)
# 2    3    4    5 
# 4 1416  282  144

table(all.surv.dat[all.surv.dat$bank=="GB",]$random)#, all.surv.dat[all.surv.dat$bank=="GB",]$year)
table(surv.dat[["GB"]]$random)#, surv.dat[["GB"]]$year)

bnk<- "GB"

table(all[all$bank=="GB" & !all$tow %in% c(1:24,301:324) & all$state=="live",]$random,
      all[all$bank=="GB" & !all$tow %in% c(1:24,301:324) & all$state=="live",]$year)


all[all$cruise=="LE07",]

1	Regular survey tow
2	Exploratory tow
3	Repeat tow from previous year
4	Comparative survey tow
5	Exploratory repeat tow

surv.dat[["GB"]][surv.dat[["GB"]]$year>2018 & surv.dat[["GB"]]$random==5,]



require(ggplot2)
require(tidyverse)
require(dplyr)
require(plotly)
require(sf)
table(exported$random)

towids <- all.surv.dat %>%
  group_by(year, bank, random) %>%
  summarize(mintow= min(tow),
            maxtow=max(tow))
towids$text <- paste0(towids$mintow, " - ", towids$maxtow)
towids$year <- as.numeric(as.character(towids$year))

write <- towids
write <- dplyr::select(write, year, bank, random, mintow, maxtow)
names(write) <- c("year", "bank", "tow_type_id", "min_tow_num", "max_tow_num")
write.csv(write, "Y:/Offshore/Assessment/Data/Survey_data/2022/tow type and number summary.csv")

pdf(file="Y:/Offshore/Assessment/Data/Survey_data/2022/checking database tow IDs and types_db.pdf", onefile=T, height=6, width=8)
for(i in 1:length(unique(towids$bank))){
  sub <- subset(towids, bank==unique(towids$bank)[i])
  
  print(ggplot() + geom_text(data=sub[!sub$random==1,], aes(x=maxtow, y=year+0.5, label=text, group=as.factor(random)), size=1, hjust=0)+
          geom_text(data=sub[sub$random==1,], aes(x=mintow, y=year+0.5, label=text, group=as.factor(random)), size=1, hjust=0)+
          geom_segment(data=sub, aes(x=mintow, xend=maxtow, y=year, yend=year, colour=as.factor(random)), lwd=1.5) + 
          theme_bw() + 
          scale_color_viridis_d(name="Tow Type ID") +
          scale_x_continuous(expand=c(0.2,0))+
          xlab("Tow number range") +
          ylab("Year") +
          ggtitle(paste0(unique(towids$bank)[i], "\nIn database (all.surv.dat obj)")))
}
dev.off()

banks <- unique(towids$bank)
banks <- banks[!banks %in% c("Ban", "BanIce")]

pdf(file="Y:/Offshore/Assessment/Data/Survey_data/2022/tow types_database.pdf", onefile=T, height=6, width=8)
for(i in banks){
  test <- all.surv.dat %>% group_by(year, bank, random) %>%
    summarize(n=n()) %>%
    filter(bank==i)
  
  print(ggplot() + geom_point(data=test, aes(random, year, size=n)) + scale_size_continuous(name="Number\nof tows") +
    xlab("tow type ID") +
    theme_bw() + 
    ylab("Year") +
    ggtitle(paste0(i, "\ndatabase (all.surv.dat obj)")) + 
    xlim(1,5) +
    ylim(1980, 2022))
}
dev.off()


pdf(file="Y:/Offshore/Assessment/Data/Survey_data/2022/checking database tow IDs and types_survdatobj.pdf", onefile=T, height=6, width=8)
for(i in banks){
  towids <- surv.dat[[i]] %>%
    group_by(year, bank, random) %>%
    summarize(mintow= min(tow),
              maxtow=max(tow))
  towids$text <- paste0(towids$mintow, " - ", towids$maxtow)
  towids$year <- as.numeric(as.character(towids$year))
  sub <- subset(towids, bank==i)
  
  print(ggplot() + geom_text(data=sub[!sub$random==1,], aes(x=maxtow, y=year+0.5, label=text, group=as.factor(random)), size=1, hjust=0)+
          geom_text(data=sub[sub$random==1,], aes(x=mintow, y=year+0.5, label=text, group=as.factor(random)), size=1, hjust=0)+
          geom_segment(data=sub, aes(x=mintow, xend=maxtow, y=year, yend=year, colour=as.factor(random)), lwd=1.5) + 
          theme_bw() + 
          scale_color_viridis_d(name="Tow Type ID") +
          scale_x_continuous(expand=c(0.2,0))+
          xlab("Tow number range") +
          ylab("Year") +
          ggtitle(paste0(i, "\nSS output (surv.dat obj)")))
}
dev.off()


pdf(file="Y:/Offshore/Assessment/Data/Survey_data/2022/tow types_survdatobj.pdf", onefile=T, height=6, width=8)
for(i in banks){
  test <- surv.dat[[i]] %>% group_by(year, bank, random) %>%
    summarize(n=n())
  
  print(ggplot() + geom_point(data=test, aes(random, year, size=n)) + scale_size_continuous(name="Number\nof tows") +
    xlab("tow type ID") +
    theme_bw() + 
    ylab("Year") +
    ggtitle(paste0(i, "\nSS output (surv.dat obj)")) + 
    xlim(1,5) +
    ylim(1980, 2022))
}
dev.off()

table(all.surv.dat[all.surv.dat$random==5,]$bank)
gba5 <- all.surv.dat[all.surv.dat$random==5 & all.surv.dat$bank=="GBa",]
table(gba5$year)

pdf(file="Y:/Offshore/Assessment/Data/Survey_data/2022/tow type 5.pdf", onefile=T, height=6, width=8)
for(j in banks){
  gba5 <- all.surv.dat[all.surv.dat$random==5 & all.surv.dat$bank==j,]
  for(i in sort(unique(gba5$year))){
    year2 <- gba5[gba5$year==i,c("tow", "slon", "slat", "elon", "elat", "random")]
    year2$station <- "repeat"
    year2_start <- st_as_sf(year2[,c("tow", "slon", "slat", "random", "station")], coords=c("slon", "slat"), crs=4326)
    year2_start$pos <- "start"
    year2_end <- st_as_sf(year2[,c("tow", "elon", "elat", "random", "station")], coords=c("elon", 'elat'), crs=4326)
    year2_end$pos <- "end"
    year2 <- rbind(year2_start, year2_end)
    year2 <- year2 %>% group_by(tow) %>% summarize() %>% 
      st_cast("MULTILINESTRING")
    
    if(!j=="Sab"){
      year1 <- all.surv.dat[all.surv.dat$bank==j & all.surv.dat$year==(i-1),c("tow", "slon", "slat", "elon", "elat", "random")]
    }
    if(j=="Sab"){
      year1 <- all.surv.dat[all.surv.dat$bank==j & all.surv.dat$year==(i-2),c("tow", "slon", "slat", "elon", "elat", "random")]
    }
    year1$station <- "original"
    year1_start <- st_as_sf(year1[,c("tow", "slon", "slat", "random", "station")], coords=c("slon", "slat"), crs=4326)
    year1_start$pos <- "start"
    year1_end <- st_as_sf(year1[,c("tow", "elon", "elat", "random", "station")], coords=c("elon", 'elat'), crs=4326)
    year1_end$pos <- "end"
    year1 <- rbind(year1_start, year1_end)
    year1 <- year1 %>% group_by(tow) %>% summarize() %>% 
      st_cast("MULTILINESTRING")
    
    bbox <- st_buffer(st_as_sfc(st_bbox(year2)), 10000)
    
    year1 <- st_intersection(year1, bbox)
    year2 <- st_intersection(year2, bbox)
    
    print(ggplot() + geom_sf(data=year1, lwd=2, colour="grey") + 
      geom_sf(data=year2, colour="red") +theme_bw() +
      geom_sf_text(data=year2, aes(label=tow)) +
      ggtitle(paste0("Tow type 5 - ", j, " - ", i, "\nThick grey lines are previous year's survey tows;\nlabelled red lines are above year's tow type 5's")))
    
  }
}
dev.off()


pdf(file="Y:/Offshore/Assessment/Data/Survey_data/2022/tow type 3.pdf", onefile=T, height=6, width=8)
for(j in banks){
  message(j)
  gba5 <- all.surv.dat[all.surv.dat$random==3 & all.surv.dat$bank==j,]
  if(dim(gba5)[1]>0){
    for(i in as.numeric(sort(unique(gba5$year)))){
      message(i)
      year2 <- gba5[gba5$year==i,c("tow", "slon", "slat", "elon", "elat", "random")]
      year2$station <- "repeat"
      year2_start <- st_as_sf(year2[,c("tow", "slon", "slat", "random", "station")], coords=c("slon", "slat"), crs=4326)
      year2_start$pos <- "start"
      year2_end <- st_as_sf(year2[,c("tow", "elon", "elat", "random", "station")], coords=c("elon", 'elat'), crs=4326)
      year2_end$pos <- "end"
      year2 <- rbind(year2_start, year2_end)
      year2 <- year2 %>% group_by(tow) %>% summarize() %>% 
        st_cast("MULTILINESTRING")
      
      year1 <- all.surv.dat[all.surv.dat$bank==j & all.surv.dat$year==(i-1),c("tow", "slon", "slat", "elon", "elat", "random")]
      if(dim(year1)[1] > 0){
        year1$station <- "original"
        year1_start <- st_as_sf(year1[,c("tow", "slon", "slat", "random", "station")], coords=c("slon", "slat"), crs=4326)
        year1_start$pos <- "start"
        year1_end <- st_as_sf(year1[,c("tow", "elon", "elat", "random", "station")], coords=c("elon", 'elat'), crs=4326)
        year1_end$pos <- "end"
        year1 <- rbind(year1_start, year1_end)
        year1 <- year1 %>% group_by(tow) %>% summarize() %>% 
          st_cast("MULTILINESTRING")
        
        bbox <- st_buffer(st_as_sfc(st_bbox(year2)), 10000)
        
        year1 <- st_intersection(year1, bbox)
        year2 <- st_intersection(year2, bbox)
        
        print(ggplot() + geom_sf(data=year1, lwd=2, colour="grey") + 
                geom_sf(data=year2, colour="red") +theme_bw() +
                geom_sf_text(data=year2, aes(label=tow)) +
                ggtitle(paste0("Tow type 3 - ", j, " - ", i, "\nThick grey lines are previous year's survey tows;\nlabelled red lines are above year's tow type 3's")))
      }
    } 
  }
}
dev.off()


pdf(file="Y:/Offshore/Assessment/Data/Survey_data/2022/tow type 4.pdf", onefile=T, height=6, width=8)
for(j in banks){
  gba5 <- all.surv.dat[all.surv.dat$random==4 & all.surv.dat$bank==j,]
  for(i in as.numeric(sort(unique(gba5$year)))){
    year2 <- gba5[gba5$year==i,c("tow", "slon", "slat", "elon", "elat", "random")]
    year2$station <- "repeat"
    year2_start <- st_as_sf(year2[,c("tow", "slon", "slat", "random", "station")], coords=c("slon", "slat"), crs=4326)
    year2_start$pos <- "start"
    year2_end <- st_as_sf(year2[,c("tow", "elon", "elat", "random", "station")], coords=c("elon", 'elat'), crs=4326)
    year2_end$pos <- "end"
    year2 <- rbind(year2_start, year2_end)
    year2 <- year2 %>% group_by(tow) %>% summarize() %>% 
      st_cast("MULTILINESTRING")
    
    year1 <- all.surv.dat[all.surv.dat$bank==j & all.surv.dat$year==(i-3),c("tow", "slon", "slat", "elon", "elat", "random")]
    if(dim(year1)[1] > 0){
      year1$station <- "original"
      year1_start <- st_as_sf(year1[,c("tow", "slon", "slat", "random", "station")], coords=c("slon", "slat"), crs=4326)
      year1_start$pos <- "start"
      year1_end <- st_as_sf(year1[,c("tow", "elon", "elat", "random", "station")], coords=c("elon", 'elat'), crs=4326)
      year1_end$pos <- "end"
      year1 <- rbind(year1_start, year1_end)
      year1 <- year1 %>% group_by(tow) %>% summarize() %>% 
        st_cast("MULTILINESTRING")
      
      bbox <- st_buffer(st_as_sfc(st_bbox(year2)), 10000)
      
      year1 <- st_intersection(year1, bbox)
      year2 <- st_intersection(year2, bbox)
      
      print(ggplot() + geom_sf(data=year1, lwd=2, colour="grey") + 
              geom_sf(data=year2, colour="red") +theme_bw() +
              geom_sf_text(data=year2, aes(label=tow)) +
              ggtitle(paste0("Tow type 4 - ", j, " - ", i, "\nThick grey lines are previous year's survey tows;\nlabelled red lines are above year's tow type 4's")))
      
    }
  }
}
dev.off()


which(gba5$slat %in% all.surv.dat$slat)

