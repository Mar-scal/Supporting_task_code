library(Mar.datawrangling)
library(tidyverse)
library(ggthemes)
library(cowplot)

theme_set(theme_few(base_size = 16))


funs <- c("https://raw.githubusercontent.com/Mar-Scal/Assessment_fns/master/Maps/pectinid_projector_sf.R")

for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} # end for(un in funs)

temp <- tempfile()
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/offshore/offshore.zip", temp, quiet=T)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)

# Make a basemap

# Make this with pecjector...

basemap <- pecjector(area = "GB",
                     plot = F,
                     repo = 'github', 
                     quiet=F,
                     crs=4326,
                     txt.size = 14,
                     add_layer = list(land='grey',
                                      eez = 'eez', 
                                      sfa = 'offshore', 
                                      bathy = c(50,'both',500)#, 
                                      # scale.bar= scale.bar
                     )) +
  theme(panel.grid=element_blank(), 
        axis.ticks=element_line(),
        legend.position = 'right',
        legend.direction = 'vertical',
        legend.justification = 'left',
        legend.key.size = unit(.5,"line")) #+


# This pulls in all the layers from the above location
offshore.spa <- combo.shp(temp2,make.sf=T, quiet=T)
gb.spa <- offshore.spa[offshore.spa$ID %in% c("SFA27A.shp", "SFA27B.shp"),]
data.dir.wrangled <- "Y:/Offshore/Assessment/2024/Supporting_tasks/predators_on_GB/"
years <- 2007:2025



######### Sea stars

get_data("rv",usepkg = 'roracle',data.dir = data.dir.wrangled,fn.oracle.username=un.ID,fn.oracle.password =pwd.ID,
         force.extract = F)
# Lets pull ASTROPECTEN AND ASTERIAS SPECIES
sea.stars <- c(grep("ASTERIAS",GSSPECIES$SPEC),grep("ASTROPECTEN",GSSPECIES$SPEC))

GSSPECIES = GSSPECIES[sea.stars,]
self_filter(keep_nullsets = F)
all.tmp <- summarize_catches()

sppCounter_CAT <- aggregate(
x = list(N_OCCURENCES_GSCAT = GSCAT$SPEC),
by = list(CODE = GSCAT$SPEC,
year = substr(GSCAT$MISSION, start = 4,7)),
length
)

names(all.tmp) <- tolower(names(all.tmp))

all.new.vessel <- all.tmp |> collapse::fsubset(mission %in% c("CAR2025002","CAR2024010",
                                                            "CAB2024003","CAR2023011","CAR2023002",
                                                            "CAB2022010", "CAR2022102",
                                                            "CAR2021241","CAR2021240"))

all <- all.tmp |> collapse::fsubset(!mission %in% c("CAR2025002","CAR2024010",
                                               "CAB2024003","CAR2023011","CAR2023002",
                                               "CAB2022010", "CAR2022102",
                                               "CAR2021241","CAR2021240"))

all.combo <- all |> collapse::fgroup_by(setno,year,longitude,latitude,season,type,xtype) |> collapse::fsummarise(totno = sum(totno,na.rm=T),
                                                                                         totwgt = sum(totwgt,na.rm=T))

all.nv.combo <-   all.new.vessel |> collapse::fgroup_by(setno,year,longitude,latitude,season,type,xtype) |> collapse::fsummarise(totno = sum(totno,na.rm=T),
                                                                                                                      totwgt = sum(totwgt,na.rm=T))

all.sf <- st_as_sf(all.combo,coords = c("longitude","latitude"))
st_crs(all.sf) <- 4326
all.nv.sf <- st_as_sf(all.nv.combo,coords = c("longitude","latitude"))
st_crs(all.nv.sf) <- 4326

gb.ss.sf <- st_intersection(all.sf,gb.spa)
gb.nv.ss.sf <- st_intersection(all.nv.sf,gb.spa)
  
gb.ss.all <- gb.ss.sf[gb.ss.sf$year >= 2007,]
gb.ss.all$Ind.wgt <- gb.ss.all$totwgt / gb.ss.all$totno
gb.ss.type.1.winter <- gb.ss.all[gb.ss.all$type ==1 & gb.ss.all$season == "SPRING",]
gb.ss.winter <- gb.ss.all[gb.ss.all$season == "SPRING",]
gb.ss.summer <- gb.ss.all[gb.ss.all$season != "SPRING",]
# New vessels
gb.nv.ss.all <- gb.nv.ss.sf[gb.nv.ss.sf$year >= 2007,]
gb.nv.ss.all$Ind.wgt <- gb.nv.ss.all$totwgt / gb.nv.ss.all$totno
gb.nv.ss.type.1.winter <- gb.nv.ss.all[gb.nv.ss.all$type ==1 & gb.nv.ss.all$season == "SPRING",]
gb.nv.ss.winter <- gb.nv.ss.all[gb.nv.ss.all$season == "SPRING",]
gb.nv.ss.summer <- gb.nv.ss.all[gb.nv.ss.all$season != "SPRING",]



ss.wgt.plt <- basemap + geom_sf(data=gb.ss.type.1.winter, aes(size=totwgt),pch=19,colour ="red") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))

ss.num.plt <- basemap + geom_sf(data=gb.ss.type.1.winter, aes(size=totno),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))

ss.ind.wgt.plt <- basemap + geom_sf(data=gb.ss.type.1.winter, aes(size=Ind.wgt),pch=19,colour ="orange") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))


# Try to get anything for GB from the new vessels, warning in the plot has something to do with the xlim/ylim, ignore it
ss.nv.wgt.plt <- basemap + geom_sf(data=gb.nv.ss.all, aes(size=totwgt),pch=19,colour ="red") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))

ss.nv.num.plt <- basemap + geom_sf(data=gb.nv.ss.all, aes(size=totno),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))

ss.nv.ind.wgt.plt <- basemap + geom_sf(data=gb.nv.ss.all, aes(size=Ind.wgt),pch=19,colour ="orange") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))





# Now try crabs...


get_data("rv",usepkg = 'roracle',data.dir = data.dir.wrangled)
# Crab time.
crabs <- c(grep("CANCER",GSSPECIES$SPEC))

GSSPECIES = GSSPECIES[crabs,]
self_filter(keep_nullsets = F)
all.crabs <- summarize_catches()

sppCounter_CAT <- aggregate(
  x = list(N_OCCURENCES_GSCAT = GSCAT$SPEC),
  by = list(CODE = GSCAT$SPEC,
            year = substr(GSCAT$MISSION, start = 4,7)),
  length
)

names(all.crabs) <- tolower(names(all.crabs))

all.crab <- all.crabs |> collapse::fsubset(!mission %in% c("CAR2025002","CAR2024010",
                                                              "CAB2024003","CAR2023011","CAR2023002",
                                                              "CAB2022010", "CAR2022102",
                                                              "CAR2021241","CAR2021240"))
# new vessels only

all.nv.crab <- all.crabs |> collapse::fsubset(!mission %in% c("CAR2025002","CAR2024010",
                                                           "CAB2024003","CAR2023011","CAR2023002",
                                                           "CAB2022010", "CAR2022102",
                                                           "CAR2021241","CAR2021240"))

crab.combo <- all.crab |> collapse::fgroup_by(setno,year,longitude,latitude,season,type,xtype) |> collapse::fsummarise(totno = sum(totno,na.rm=T),
                                                                                                                       totwgt = sum(totwgt,na.rm=T))

crab.nv.combo <- all.nv.crab |> collapse::fgroup_by(setno,year,longitude,latitude,season,type,xtype) |> collapse::fsummarise(totno = sum(totno,na.rm=T),
                                                                                                                       totwgt = sum(totwgt,na.rm=T))


all.crab.sf <- st_as_sf(all.crab,coords = c("longitude","latitude"))
st_crs(all.crab.sf) <- 4326
gb.crab <- st_intersection(all.crab.sf,gb.spa)
# new vessel only
all.nv.crab.sf <- st_as_sf(all.nv.crab,coords = c("longitude","latitude"))
st_crs(all.nv.crab.sf) <- 4326
gb.nv.crab <- st_intersection(all.nv.crab.sf,gb.spa)


gb.crab.all <- gb.crab[gb.crab$year >= 2007,]
gb.crab.all$Ind.wgt <- gb.crab.all$totwgt / gb.crab.all$totno
gb.crab.type.1.winter <- gb.crab.all[gb.crab.all$xtype ==1 & gb.crab.all$season == "SPRING",]
gb.crab.winter <- gb.crab.all[gb.crab.all$season == "SPRING",]
gb.crab.summer <- gb.crab.all[gb.crab.all$season != "SPRING",]
# New Vessel only crab
gb.nv.crab.all <- gb.nv.crab[gb.nv.crab$year >= 2007,]
gb.nv.crab.all$Ind.wgt <- gb.nv.crab.all$totwgt / gb.nv.crab.all$totno
gb.nv.crab.type.1.winter <- gb.nv.crab.all[gb.nv.crab.all$xtype ==1 & gb.nv.crab.all$season == "SPRING",]
gb.nv.crab.winter <- gb.nv.crab.all[gb.nv.crab.all$season == "SPRING",]
gb.nv.crab.summer <- gb.nv.crab.all[gb.nv.crab.all$season != "SPRING",]


crab.wgt.plt <- basemap + geom_sf(data=gb.crab.type.1.winter, aes(size=totwgt),pch=19,colour ="red") + facet_wrap(~year) + 
  ggtitle("Crab species (Cancer genus) on Georges Bank") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))
crab.num.plt <- basemap + geom_sf(data=gb.crab.type.1.winter, aes(size=totno),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("CANCER SPP") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))
crab.ind.wgt.plt <- basemap + geom_sf(data=gb.crab.type.1.winter, aes(size=Ind.wgt),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("CANCER SPP") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))
# New vessel all data...warning in the plot has something to do with the xlim/ylim, ignore it
crab.nv.wgt.plt <- basemap + geom_sf(data=gb.nv.crab.all, aes(size=totwgt),pch=19,colour ="red") + facet_wrap(~year) + 
  ggtitle("Crab species (Cancer genus) on Georges Bank") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))
crab.nv.num.plt <- basemap + geom_sf(data=gb.nv.crab.all, aes(size=totno),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("CANCER SPP") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))
crab.nv.ind.wgt.plt <- basemap + geom_sf(data=gb.nv.crab.all, aes(size=Ind.wgt),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("CANCER SPP") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))



# I can do a basic time series too, but need all the data to figure out how many tows there were within the domain I'm covering here...

get_data("rv",usepkg = 'roracle',data.dir = data.dir.wrangled)
# All sets for all species...
self_filter(keep_nullsets = T)
all.tow <- summarize_catches()

names(all.tow) <- tolower(names(all.tow))

all.tow <- all.tow |> collapse::fsubset(!is.na(longitude) | !is.na(latitude))
all.tows <- all.tow |> collapse::fsubset(!mission %in% c("CAR2025002","CAR2024010",
                                                         "CAB2024003","CAR2023011","CAR2023002",
                                                         "CAB2022010", "CAR2022102",
                                                         "CAR2021241","CAR2021240"))

all.nv.tows <- all.tow |> collapse::fsubset(mission %in% c("CAR2025002","CAR2024010",
                                                         "CAB2024003","CAR2023011","CAR2023002",
                                                         "CAB2022010", "CAR2022102",
                                                         "CAR2021241","CAR2021240"))


all.tows <- all.tows |> collapse::fsubset(year >= 2007) # Need to do that separately from the na's for logic reason...

all.tows.sf <- st_as_sf(all.tows,coords = c("longitude","latitude"))
st_crs(all.tows.sf) <- 4326

all.nv.tows.sf <- st_as_sf(all.nv.tows,coords = c("longitude","latitude"))
st_crs(all.nv.tows.sf) <- 4326


# Now get this for my corner of GB. In 2022 there are three Missions that cover this corner of GB. There are no Type 1 sets on the Winter survey
gb.all.tows <-   st_intersection(all.tows.sf,gb.spa)
gb.type.1.winter <- gb.all.tows[gb.all.tows$xtype ==1 & gb.all.tows$season == "SPRING",]
gb.winter <- gb.all.tows[gb.all.tows$season == "SPRING" ,]
gb.summer <- gb.all.tows[gb.all.tows$season != "SPRING" ,]
# New vessels
gb.nv.all.tows <-   st_intersection(all.nv.tows.sf,gb.spa)
gb.nv.type.1.winter <- gb.nv.all.tows[gb.nv.all.tows$xtype ==1 & gb.nv.all.tows$season == "SPRING",]
gb.nv.winter <- gb.nv.all.tows[gb.nv.all.tows$season == "SPRING" ,]
gb.nv.summer <- gb.nv.all.tows[gb.nv.all.tows$season != "SPRING" ,]


# Now can I get the number of tows in this domain each year...
# And here are the number of unique tows on GB from the RV survey.
num.tows <- as.data.frame(gb.type.1.winter) |> collapse::fgroup_by(year) |> collapse::fsummarise(total.tows = dplyr::n_distinct(setno))
missing.years <- setdiff(years,num.tows$year)
mis <- data.frame(year=missing.years,total.tows = rep(NA,length(missing.years)))
num.tows <- rbind(num.tows,mis)
num.tows <- num.tows[order(num.tows$year),]
# Now how many tows actually observed seastars or crabs
ss.tows <- as.data.frame(gb.ss.type.1.winter) |> collapse::fgroup_by(year) |> collapse::fsummarise(ss.tows = dplyr::n_distinct(setno))
missing.ss.years <- setdiff(years,ss.tows$year)
mcss <- data.frame(year=missing.ss.years,ss.tows = rep(NA,length(missing.ss.years)))
ss.tows <- rbind(ss.tows,mcss)
ss.tows <- ss.tows[order(ss.tows$year),]

crab.tows <- as.data.frame(gb.crab.type.1.winter) |> collapse::fgroup_by(year) |> collapse::fsummarise(crab.tows = dplyr::n_distinct(setno))
missing.crab.years <- setdiff(years,crab.tows$year)
mcy <- data.frame(year=missing.crab.years,crab.tows = rep(NA,length(missing.crab.years)))
crab.tows <- rbind(crab.tows,mcy)
crab.tows <- crab.tows[order(crab.tows$year),]
# Merge these together
tow.data <- left_join(ss.tows,num.tows,by="year")
tow.data <- left_join(tow.data,crab.tows,by="year")

tow.props <- data.frame(year = rep(min(tow.data$year):max(tow.data$year),2),name = c(rep("prop.crab",nrow(tow.data)),rep("prop.ss",nrow(tow.data))),
                        prop = c(tow.data$crab.tows/tow.data$total.tows,tow.data$ss.tows/tow.data$total.tows))
# stack this for ggplot...
tow.data.long <- pivot_longer(tow.data,cols = c("ss.tows","total.tows","crab.tows"))

# Now make some figures...
ggplot(tow.data.long) + geom_line(aes(x=year,y=value,group = name,color=name),size=1.5) + xlab("") + ylab("Number of tows") +
  scale_color_manual(name="",values = c("blue","firebrick2","darkgrey"),labels=c("Crabs","Seastars","Total")) +ylim(c(0,60))

ggplot(tow.props) + geom_line(aes(x=year,y=prop,group = name,color=name),size=1.5) + xlab("") + ylab("Proportion of tows") +
  scale_color_manual(name="Species",values = c("blue","firebrick2"),labels=c("Crabs","Seastars")) + ylim(c(0,1))



basemap + geom_sf(data=gb.type.1.winter) + facet_wrap(~year) + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) 

# Let's get a simple index...
ss.tows$no.ss <- num.tows$total.tows - ss.tows$ss.tows
crab.tows$no.crab <- num.tows$total.tows - crab.tows$crab.tows
# Create an object with lots of 0s that I can add the the ss and crab data.
# This isn't pretty but best my brain could come up with
yrs.w.dat <- c(2007:2021,2023,2024) 
for(i in yrs.w.dat)
{
  crab.tmp <- data.frame(year = i,totno = rep(0,crab.tows$no.crab[crab.tows$year == i]),
                         totwgt = rep(0,crab.tows$no.crab[crab.tows$year == i]),
                         setno = 5000+(1:crab.tows$no.crab[crab.tows$year == i]))
  ss.tmp <- data.frame(year = i,totno = rep(0,ss.tows$no.ss[ss.tows$year == i]),
                       totwgt = rep(0,ss.tows$no.ss[ss.tows$year == i]),
                       setno = 5000+(1:ss.tows$no.ss[ss.tows$year == i]))
  
  if(i == 2007) 
  {
    crab.fake <- crab.tmp
    ss.fake <- ss.tmp
  }
  if(i > 2007) 
  {
    crab.fake <- rbind(crab.fake,crab.tmp)
    ss.fake <- rbind(ss.fake,ss.tmp)
  }
} 

# Now get the crab and ss data combined.
crab.fake.db <- matrix(data = NA,ncol = ncol(gb.crab.type.1.winter),nrow = nrow(crab.fake)+1)
colnames(crab.fake.db) <- names(gb.crab.type.1.winter)
crab.fake.db <- as.data.frame(crab.fake.db)
crab.fake.db$year[1] <- 2022
crab.fake.db$year[-1] <- crab.fake$year  
crab.fake.db$totno[-1] <- crab.fake$totno  
crab.fake.db$totwgt[-1] <- crab.fake$totwgt
crab.fake.db$setno[-1] <- crab.fake$setno
#seastars
ss.fake.db <- matrix(data = NA,ncol = ncol(gb.ss.type.1.winter),nrow = nrow(ss.fake)+1)
colnames(ss.fake.db) <- names(gb.ss.type.1.winter)
ss.fake.db <- as.data.frame(ss.fake.db)
ss.fake.db$year[1] <- 2022 # Hake to get 2022 with no tows in here
ss.fake.db$year[-1] <- ss.fake$year  
ss.fake.db$totno[-1] <- ss.fake$totno  
ss.fake.db$totwgt[-1] <- ss.fake$totwgt
ss.fake.db$setno[-1] <- ss.fake$setno


# Now stitch this into the crab and sea star data...
crab.dat.4.index <- rbind(data.frame(gb.crab.type.1.winter),crab.fake.db)

crab.ts <- crab.dat.4.index |> collapse::fgroup_by(year) |> collapse::fsummarise(mn.num = mean(totno,na.rm=T),
                                                                                 mn.wgt = mean(totwgt,na.rm=T),
                                                                                 sd.num = sd(totno,na.rm=T),
                                                                                 sd.wgt = sd(totwgt,na.rm=T),
                                                                                 med.num = median(totno,na.rm=T),
                                                                                 med.wgt = median(totwgt,na.rm=T))
crab.ts$Species <- "Cancer Species"

# Sea starts now...
ss.dat.4.index <- rbind(data.frame(gb.ss.type.1.winter),ss.fake.db)

ss.ts <- ss.dat.4.index |> collapse::fgroup_by(year) |> collapse::fsummarise(mn.num = mean(totno,na.rm=T),
                                                                             mn.wgt = mean(totwgt,na.rm=T),
                                                                             sd.num = sd(totno,na.rm=T),
                                                                             sd.wgt = sd(totwgt,na.rm=T),
                                                                             med.num = median(totno,na.rm=T),
                                                                             med.wgt = median(totwgt,na.rm=T))
# Check that the counts are right..
ss.count <- ss.dat.4.index |> collapse::fgroup_by(year) |> collapse::fcount()

ss.ts$Species <- "Astropecten and Asterias Species"
# Merge these together...
pred.ts <- rbind(crab.ts,ss.ts)

# Now make the plots

ggplot(pred.ts) + geom_line(aes(x=year,y=mn.num,color=Species,group=Species),size=1.5) + 
                  xlab("") + ylab("Mean Number per tow") + scale_y_log10() +
                  scale_color_manual(values=c("blue","firebrick2"))

ggplot(pred.ts) + geom_line(aes(x=year,y=mn.wgt,color=Species,group=Species),size=1.5) + 
                  xlab("") + ylab("Mean Weight per tow (kg)") + scale_y_log10() +
                  scale_color_manual(values=c("blue","firebrick2"))
    


# These plots make the presentation cut...

crab.ts.plt <- ggplot(pred.ts[pred.ts$Species == "Cancer Species",]) + geom_line(aes(x=year,y=mn.wgt),size=1.5) + 
                        xlab("") + ylab("Mean Weight per tow (kg)") + ggtitle("Crab species (Cancer genus) on Georges Bank") +
                        scale_color_manual(values=c("blue","firebrick2")) + scale_x_continuous(breaks=seq(2007,2024,by=2))
save_plot("Y:/Offshore/Assessment/2024/Supporting_tasks/predators_on_GB/Crab_wgt_ts.png",crab.ts.plt,base_height = 8,base_width = 8)
                        

ss.ts.plt <-   ggplot(pred.ts[pred.ts$Species == "Astropecten and Asterias Species",]) + geom_line(aes(x=year,y=mn.wgt),size=1.5) + 
                        xlab("") + ylab("Mean Weight per tow (kg)") + ggtitle("Astropecten and Asterias Species on Georges Bank")+
                        scale_color_manual(values=c("blue","firebrick2")) + scale_x_continuous(breaks=seq(2007,2024,by=2))
save_plot("Y:/Offshore/Assessment/2024/Supporting_tasks/predators_on_GB/Sea_stars_wgt_ts.png",ss.ts.plt,base_height = 8,base_width = 8)
                        
  
gb.crab.2018.onwards <- gb.crab.type.1.winter |> collapse::fsubset(year >=2018)

crab.spat.wgt.plt <- basemap + geom_sf(data=gb.crab.2018.onwards , aes(size=totwgt),pch=19,colour ="red") + facet_wrap(~year) + labs(size = "Kg per tow")+
                                           ggtitle("Crab species (Cancer genus) on Georges Bank") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))+
                                            theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2024/Supporting_tasks/predators_on_GB/Crabs_wgt_spatial_2018_2024.png",crab.spat.wgt.plt,base_height = 8,base_width = 9)

  
gb.ss.2018.onwards <- gb.ss.type.1.winter |> collapse::fsubset(year >=2018)

ss.spat.wgt.plt <-    basemap + geom_sf(data=gb.ss.2018.onwards , aes(size=totwgt),pch=19,colour ="red") + facet_wrap(~year) + labs(size = "Kg per tow") +
                                           ggtitle("Astropecten and Asterias Species on Georges Bank") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) +
                                           theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2024/Supporting_tasks/predators_on_GB/Sea_stars_wgt_spatial_2018_2024.png",ss.spat.wgt.plt,base_height = 8,base_width = 9)

# Repeat the index calucations for the New vessels only, using all data for a year for this instead of just winter



# Now can I get the number of tows in this domain each year...
# And here are the number of unique tows on GB from the RV survey.
years.nv <- 2021:2025
gb.nv.all.tows <- gb.nv.all.tows |> collapse::fsubset(year %in% years.nv)
num.tows <- as.data.frame(gb.nv.all.tows) |> collapse::fgroup_by(year) |> collapse::fsummarise(total.tows = dplyr::n_distinct(setno))
missing.years <- setdiff(years.nv,num.tows$year)
mis <- data.frame(year=missing.years,total.tows = rep(NA,length(missing.years)))
num.tows <- rbind(num.tows,mis)
num.tows <- num.tows[order(num.tows$year),]
# Now how many tows actually observed seastars or crabs
ss.tows <- as.data.frame(gb.nv.ss.all) |> collapse::fgroup_by(year) |> collapse::fsummarise(ss.tows = dplyr::n_distinct(setno))
missing.ss.years <- setdiff(years.nv,ss.tows$year)
mcss <- data.frame(year=missing.ss.years,ss.tows = rep(NA,length(missing.ss.years)))
ss.tows <- rbind(ss.tows,mcss)
ss.tows <- ss.tows[order(ss.tows$year),]

crab.tows <- as.data.frame(gb.nv.crab.all) |> collapse::fgroup_by(year) |> collapse::fsummarise(crab.tows = dplyr::n_distinct(setno))
missing.crab.years <- setdiff(years.nv,crab.tows$year)
mcy <- data.frame(year=missing.crab.years,crab.tows = rep(NA,length(missing.crab.years)))
crab.tows <- rbind(crab.tows,mcy)
crab.tows <- crab.tows[order(crab.tows$year),]
# Merge these together
tow.data <- left_join(ss.tows,num.tows,by="year")
tow.data <- left_join(tow.data,crab.tows,by="year")

tow.props <- data.frame(year = rep(min(tow.data$year):max(tow.data$year),2),name = c(rep("prop.crab",nrow(tow.data)),rep("prop.ss",nrow(tow.data))),
                        prop = c(tow.data$crab.tows/tow.data$total.tows,tow.data$ss.tows/tow.data$total.tows))
# stack this for ggplot...
tow.data.long <- pivot_longer(tow.data,cols = c("ss.tows","total.tows","crab.tows"))

# Now make some figures...
ggplot(tow.data.long) + geom_line(aes(x=year,y=value,group = name,color=name),size=1.5) + xlab("") + ylab("Number of tows") +
  scale_color_manual(name="",values = c("blue","firebrick2","darkgrey"),labels=c("Crabs","Seastars","Total")) +ylim(c(0,60))

ggplot(tow.props) + geom_line(aes(x=year,y=prop,group = name,color=name),size=1.5) + xlab("") + ylab("Proportion of tows") +
  scale_color_manual(name="Species",values = c("blue","firebrick2"),labels=c("Crabs","Seastars")) + ylim(c(0,1))




# Let's get a simple index...
ss.tows$no.ss <- num.tows$total.tows - ss.tows$ss.tows
crab.tows$no.crab <- num.tows$total.tows - crab.tows$crab.tows
# Create an object with lots of 0s that I can add the the ss and crab data.
# This isn't pretty but best my brain could come up with
yrs.w.dat <- years.nv
for(i in yrs.w.dat)
{
  crab.tmp <- data.frame(year = i,totno = rep(0,crab.tows$no.crab[crab.tows$year == i]),
                         totwgt = rep(0,crab.tows$no.crab[crab.tows$year == i]),
                         setno = 5000+(1:crab.tows$no.crab[crab.tows$year == i]))
  ss.tmp <- data.frame(year = i,totno = rep(0,ss.tows$no.ss[ss.tows$year == i]),
                       totwgt = rep(0,ss.tows$no.ss[ss.tows$year == i]),
                       setno = 5000+(1:ss.tows$no.ss[ss.tows$year == i]))
  
  if(i == 2021) 
  {
    crab.fake <- crab.tmp
    ss.fake <- ss.tmp
  }
  if(i > 2021) 
  {
    crab.fake <- rbind(crab.fake,crab.tmp)
    ss.fake <- rbind(ss.fake,ss.tmp)
  }
} 

# Now get the crab and ss data combined.
crab.fake.db <- matrix(data = NA,ncol = ncol(gb.nv.crab.all),nrow = nrow(crab.fake))
colnames(crab.fake.db) <- names(gb.nv.crab.all)
crab.fake.db <- as.data.frame(crab.fake.db)
#crab.fake.db$year[1] <- 2022
crab.fake.db$year <- crab.fake$year  
crab.fake.db$totno <- crab.fake$totno  
crab.fake.db$totwgt <- crab.fake$totwgt
crab.fake.db$setno <- crab.fake$setno
#seastars
ss.fake.db <- matrix(data = NA,ncol = ncol(gb.nv.ss.all),nrow = nrow(ss.fake))
colnames(ss.fake.db) <- names(gb.nv.ss.all)
ss.fake.db <- as.data.frame(ss.fake.db)
ss.fake.db$year <- ss.fake$year  
ss.fake.db$totno <- ss.fake$totno  
ss.fake.db$totwgt <- ss.fake$totwgt
ss.fake.db$setno <- ss.fake$setno


# Now stitch this into the crab and sea star data...
crab.dat.4.index <- rbind(data.frame(gb.nv.crab.all),crab.fake.db)

crab.ts <- crab.dat.4.index |> collapse::fgroup_by(year) |> collapse::fsummarise(mn.num = mean(totno,na.rm=T),
                                                                                 mn.wgt = mean(totwgt,na.rm=T),
                                                                                 sd.num = sd(totno,na.rm=T),
                                                                                 sd.wgt = sd(totwgt,na.rm=T),
                                                                                 med.num = median(totno,na.rm=T),
                                                                                 med.wgt = median(totwgt,na.rm=T))
crab.ts$Species <- "Cancer Species"

# Sea starts now...
ss.dat.4.index <- rbind(data.frame(gb.nv.ss.all),ss.fake.db)

ss.ts <- ss.dat.4.index |> collapse::fgroup_by(year) |> collapse::fsummarise(mn.num = mean(totno,na.rm=T),
                                                                             mn.wgt = mean(totwgt,na.rm=T),
                                                                             sd.num = sd(totno,na.rm=T),
                                                                             sd.wgt = sd(totwgt,na.rm=T),
                                                                             med.num = median(totno,na.rm=T),
                                                                             med.wgt = median(totwgt,na.rm=T))
# Check that the counts are right..
ss.count <- ss.dat.4.index |> collapse::fgroup_by(year) |> collapse::fcount()

ss.ts$Species <- "Astropecten and Asterias Species"
# Merge these together...
pred.ts <- rbind(crab.ts,ss.ts)

# Now make the plots

nv.pred.num <- ggplot(pred.ts) + geom_line(aes(x=year,y=mn.num,color=Species,group=Species),size=1.5) + 
                                 xlab("") + ylab("Mean Number per tow") + scale_y_log10() +
                                  scale_color_manual(values=c("blue","firebrick2"))
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Predators_num_ts_new_vessls.png",nv.pred.num,base_height = 8,base_width = 9)



nv.pred.wgt <- ggplot(pred.ts) + geom_line(aes(x=year,y=mn.wgt,color=Species,group=Species),size=1.5) + 
                                 xlab("") + ylab("Mean Weight per tow (kg)") + scale_y_log10() +
                                 scale_color_manual(values=c("blue","firebrick2"))
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Predators_wgt_ts_new_vessls.png",nv.pred.wgt,base_height = 8,base_width = 9)





load("C:/Users/keyserf/Documents/temp_data/Data/survey_data/2025/Survey_summary_output/Survey_all_results.Rdata")

joined <- left_join(survey.obj$GBa$model.dat, pred.ts[pred.ts$Species=="Astropecten and Asterias Species",])
ggplot() + geom_text(data=joined, aes(mn.wgt, clappers, label=year)) + xlab("mean weight of seastars per tow") + ylab("clapper index")
