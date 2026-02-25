library(Mar.datawrangling)
library(Mar.utils)
library(tidyverse)
library(ggthemes)
library(cowplot)
require(sf)
require(ggplot2)

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

# Make a basemap with pecjector...

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
data.dir.wrangled <- "Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/"
years <- 2007:2025


###################################################
######### Step 1, set up the data....
# Conversion factors from Yin et al. 20XX comparative survey work 
# Asterias Rubens (6111): 
ar.code <- 6111
ar.num.con.fac <- 1.24
ar.bm.con.fac <- 2.42
# Astropecten Americanus (6136)
aa.code <- 6136
aa.num.con.fac <- 1.4
aa.bm.con.fac <- 2.7
# Asterias forbesi
af.code <- 6109
af.num.con.fac <- (aa.num.con.fac + ar.num.con.fac) /2 # The number in the Table is 236.12, that's cracked, so taking the average of the other two sea stars...
af.bm.con.fac <- 3.08 # At least plausable.
# For the other seastars we'll take the average of the ones we have information for
ss.other.num.con.fac <- (aa.num.con.fac + af.num.con.fac) /2
ss.other.bm.con.fac <- (ar.bm.con.fac + af.bm.con.fac + aa.bm.con.fac) /3
# Now try crabs...
# The conversion factors from Yin et al. 20XX
# Jonah Crab (2511) to start
jc.code <- 2511
jc.num.con.fac <- 2.18
jc.bm.con.fac <- 3.38
# Rock Crab (2513)
rc.code <- 2513
rc.num.con.fac <- 1.27
rc.bm.con.fac <- 2.08
# Toad Crabs (2520)
tc.code <- 2520
tc.num.con.fac <- 1.09
tc.bm.con.fac <- 1.13
# Other crabs (2524)
oc.code <- 2524
oc.num.con.fac <- mean(c(jc.num.con.fac,rc.num.con.fac,tc.num.con.fac))
oc.bm.con.fac <- mean(c(jc.bm.con.fac,rc.bm.con.fac,tc.bm.con.fac))
# All combined...
crab.codes <- c(jc.code,rc.code,tc.code,oc.code)


# New vessels codes in case I want them.
nv.missions <- c("CAR2025010", "CAR2025002","CAR2024010","CAB2024003","CAR2023011","CAR2023002",
                 "CAB2022010", "CAR2022102","CAR2021241","CAR2021240")

#Here is the procedure from Ryan Martin on how to get standardized tow lengths. Basically, apply conversions for each set, 
# get a swept area in km2 (so length times width of tow), and divide the biomass/numbers by the swept area to get that right
# If you want to go and scale that up to area I would recommend following this, but calculating your areas yourself.
# Load in set-level catch data from gscat
# Apply the biomass conversion factors to the TOTWGT columns
# Applying conversion factors at the set level is important as numerous times throughout the time series multiple vessels have sampled the same strata in a given survey year. During the stratification process (described below), an average biomass is taken for a given strata, thus all set-level data within that strata has to be in the same vessel/gear units.
# Standardize the TOTWGT into an area of 1 KM2. This is done by taking distance towed in nautical miles and dividing it by 1.852 (Changes it into km) then dividing that by the wing spread of the net, which gives you area sampled in km2. You then divide TOTWGT by the area sampled to get TOTWGT by km2.
# Calculate the average TOTWGTKM2 from all sets within a strata/area.
# Next you need to calculate the area of a given strata in KM2 (in gsstratum the area is in nautical miles). This is done by just taking the area times 3.429904.
# Times the average TOTWGTKM2 within a strata by its area in KM2 to get biomass for the strata.

# The wingspan is assumed to be fixed for both gears
# This is gear type 9 (note there is some type 1 data on the new vessel using the W11A gear), so going with gear as the differentiation throughout this.
# The W2a is a 1.8 NM tow
w2a.ws.km <- 12.5/1000 # This is the gear width in meters, more commonly this is given as 41 feet.
nm.to.km <- 1.852
# Nest conversions... these are type 15 data, Ryan Martin got me this conversion, it is in meters
# Note the NEST is a 1 NM tow
nest.ws.km <- 12.49/1000

##### Step 2, get data from database...

# Connect to Oracle
cxn <- ROracle::dbConnect(DBI::dbDriver("Oracle"), un.ID, pwd.ID, "PTRAN")
get_data(db="rv", cxn=cxn)
# Lets pull ASTROPECTEN AND ASTERIAS SPECIES
sea.stars <- c(grep("ASTERIAS",GSSPECIES$SPEC),grep("ASTROPECTEN",GSSPECIES$SPEC))

GSSPECIES = GSSPECIES[sea.stars,]
self_filter(keep_nullsets = F)
all.tmp <- summarize_catches()

names(all.tmp) <- tolower(names(all.tmp))
all.tmp <- all.tmp |> collapse::fsubset(!(is.na(longitude) | is.na(latitude)))

############ Now the crabs
get_data("rv",cxn=cxn)
# Crab time.#################
crabs <- which(GSSPECIES$CODE %in% crab.codes)

GSSPECIES = GSSPECIES[crabs,]
self_filter(keep_nullsets = F)
all.crabs <- summarize_catches()

names(all.crabs) <- tolower(names(all.crabs))
all.crabs <- all.crabs |> collapse::fsubset(!(is.na(longitude) | is.na(latitude)))

# Finally I need to get all of the tows from the database so we can find the 0's, this is probably a stupid way, but it's a way....
# I can do a basic time series too, but need all the data to figure out how many tows there were within the domain I'm covering here...##############

get_data("rv",cxn=cxn)
# All sets for all species...
self_filter(keep_nullsets = T)
all.tow <- summarize_catches()

names(all.tow) <- tolower(names(all.tow))
all.tows <- all.tow |> collapse::fsubset(!(is.na(longitude) | is.na(latitude)))
all.tows <- all.tows |> collapse::fsubset(year >= 2007) # Need to do that separately from the na's for logic reason...

all.tows.sf <- st_as_sf(all.tows,coords = c("longitude","latitude"))
st_crs(all.tows.sf) <- 4326

# Now get this for my corner of GB. In 2022 there are three Missions that cover this corner of GB. There are no Type 1 sets on the spring survey
gb.all.tows <-   st_intersection(all.tows.sf,gb.spa)
gb.type.1.spring <- gb.all.tows[gb.all.tows$xtype ==1 & gb.all.tows$season == "SPRING",]
gb.spring <- gb.all.tows[gb.all.tows$season == "SPRING" ,]
gb.summer <- gb.all.tows[gb.all.tows$season != "SPRING" ,]
# Now I want a subset that are the unique tows, this assumes that all the 'good' tows caught at least 1 thing...
gb.all.tows$ID <- paste0(gb.all.tows$mission, ".", gb.all.tows$setno)
IDs <- unique(gb.all.tows$ID) 
dat <- NULL
for(i in IDs) dat[[i]] <- data.frame(gb.all.tows[gb.all.tows$ID  == i,])[1,]
gb.all.unique.tows <- do.call('rbind',dat)
gb.all.unique.tows <- gb.all.unique.tows[,c('mission','setno','year','type','dist','speed','strat','area',
                                            'dmin','dmax','gear',"ID",'geometry','season','xtype')]



##############################################
## Step 3, take data and apply the conversion factors for each species
# Note that for species we don't have conversion factors for I have taken averages above.
# I am doing this based on gear type rather than vessel.  
# I am also going to retrospectively apply the conversions as this is what the plan is for the Maritimes.

# Now update the old vessel/gear with the conversion factors...
# Frist up Rubens
all.tmp$totno[all.tmp$gear %in% 9 & all.tmp$spec == ar.code] <- 
                                          all.tmp$totno[all.tmp$gear %in% 9 & all.tmp$spec == ar.code] / ar.num.con.fac
all.tmp$totwgt[all.tmp$gear %in% 9 & all.tmp$spec == ar.code] <- 
                                          all.tmp$totwgt[all.tmp$gear %in% 9 & all.tmp$spec == ar.code] / ar.bm.con.fac
# Then Americanus
all.tmp$totno[all.tmp$gear %in% 9 & all.tmp$spec == aa.code] <- 
                                          all.tmp$totno[all.tmp$gear %in% 9 & all.tmp$spec == aa.code] /aa.num.con.fac
all.tmp$totwgt[all.tmp$gear %in% 9 & all.tmp$spec == aa.code] <- 
                                          all.tmp$totwgt[all.tmp$gear %in% 9 & all.tmp$spec == aa.code] /aa.bm.con.fac
# Then forbesi
all.tmp$totno[all.tmp$gear %in% 9 & all.tmp$spec == af.code] <- 
                                          all.tmp$totno[all.tmp$gear %in% 9 & all.tmp$spec == af.code] /af.num.con.fac
all.tmp$totwgt[all.tmp$gear %in% 9 & all.tmp$spec == af.code] <- 
                                          all.tmp$totwgt[all.tmp$gear %in% 9 & all.tmp$spec == af.code] /af.bm.con.fac
# Then the rest of theem....
all.tmp$totno[all.tmp$gear %in% 9 & !all.tmp$spec %in% c(aa.code,ar.code,af.code)] <- 
                                  all.tmp$totno[all.tmp$gear %in% 9 & !all.tmp$spec %in% c(aa.code,ar.code,af.code)] / ss.other.num.con.fac
all.tmp$totwgt[all.tmp$gear %in% 9 & !all.tmp$spec %in% c(aa.code,ar.code,af.code)] <- 
                                  all.tmp$totwgt[all.tmp$gear %in% 9 & !all.tmp$spec %in% c(aa.code,ar.code,af.code)] / ss.other.bm.con.fac


# Now get the crabs crabed, first Jonah crabs
all.crabs$totno[all.crabs$gear %in% 9 & all.crabs$spec == jc.code] <- 
                                      all.crabs$totno[all.crabs$gear %in% 9 & all.crabs$spec == jc.code] / jc.num.con.fac
all.crabs$totwgt[all.crabs$gear %in% 9 & all.crabs$spec == jc.code] <- 
                                      all.crabs$totwgt[all.crabs$gear %in% 9 & all.crabs$spec == jc.code] / jc.bm.con.fac
# Rocks
all.crabs$totno[all.crabs$gear %in% 9 & all.crabs$spec == rc.code] <- 
                                         all.crabs$totno[all.crabs$gear %in% 9 & all.crabs$spec == rc.code] /rc.num.con.fac
all.crabs$totwgt[all.crabs$gear %in% 9 & all.crabs$spec == rc.code] <- 
                                         all.crabs$totwgt[all.crabs$gear %in% 9 & all.crabs$spec == rc.code] /rc.bm.con.fac
# Toads
all.crabs$totno[all.crabs$gear %in% 9 & all.crabs$spec == tc.code] <- 
                                        all.crabs$totno[all.crabs$gear %in% 9 & all.crabs$spec == tc.code] / tc.num.con.fac 
all.crabs$totwgt[all.crabs$gear %in% 9 & all.crabs$spec == tc.code] <- 
                                         all.crabs$totwgt[all.crabs$gear %in% 9 & all.crabs$spec == tc.code] /tc.bm.con.fac
# The rest of the crabs
all.crabs$totno[all.crabs$gear %in% 9 & all.crabs$spec == oc.code] <- 
                                         all.crabs$totno[all.crabs$gear %in% 9 & all.crabs$spec == oc.code] /oc.num.con.fac
all.crabs$totwgt[all.crabs$gear %in% 9 & all.crabs$spec == oc.code] <- 
                                         all.crabs$totwgt[all.crabs$gear %in% 9 & all.crabs$spec == oc.code] /oc.bm.con.fac



##############################################
## Step 4,  now doing some data processing and sticthing things together.

# this sums up the total numbers for all the sea star species by each tow.
all.combo <- all.tmp |> collapse::fgroup_by(setno,year,longitude,latitude,season,mission,type,xtype,dist,speed,strat,area,dmin,dmax,gear) |> 
                        collapse::fsummarise(totno = sum(totno,na.rm=T),
                                             totwgt = sum(totwgt,na.rm=T))


all.sf <- st_as_sf(all.combo,coords = c("longitude","latitude"))
st_crs(all.sf) <- 4326
gb.ss.sf <- st_intersection(all.sf,gb.spa)
gb.ss.all <- gb.ss.sf[gb.ss.sf$year >= 2007,]
# We have some here with weights but no numbers, I'm going to believe the weights and not the numbers (didn't have time to count is assumption)
# and make all those 0's NAs
gb.ss.all$totno[gb.ss.all$totno == 0] <- NA
gb.ss.all$Ind.wgt <- gb.ss.all$totwgt / gb.ss.all$totno
gb.ss.type.1.spring <- gb.ss.all[gb.ss.all$type ==1 & gb.ss.all$season == "SPRING",]
gb.ss.spring <- gb.ss.all[gb.ss.all$season == "SPRING",]
gb.ss.summer <- gb.ss.all[gb.ss.all$season != "SPRING",]

# Now do the same thing with the crabs
all.crab.combo <- all.crabs |> collapse::fgroup_by(setno,year,longitude,latitude,season,mission,type,xtype,dist,speed,strat,area,dmin,dmax,gear) |> 
                               collapse::fsummarise(totno = sum(totno,na.rm=T),
                                                    totwgt = sum(totwgt,na.rm=T))


all.crab.sf <- st_as_sf(all.crab.combo,coords = c("longitude","latitude"))
st_crs(all.crab.sf) <- 4326
gb.crab <- st_intersection(all.crab.sf,gb.spa)

gb.crab.all <- gb.crab[gb.crab$year >= 2007,]
gb.crab.all$totno[gb.crab.all$totno == 0] <- NA

gb.crab.all$Ind.wgt <- gb.crab.all$totwgt / gb.crab.all$totno
gb.crab.type.1.spring <- gb.crab.all[gb.crab.all$xtype ==1 & gb.crab.all$season == "SPRING",]
gb.crab.spring <- gb.crab.all[gb.crab.all$season == "SPRING",]
gb.crab.summer <- gb.crab.all[gb.crab.all$season != "SPRING",]


########################################################################
## Step 5
# Let's put every thing above together and get objects with total biomass of crabs and sea stars along with the 0 tows...
# tow.data$no.crab <- tow.data$total.tows - tow.data$crab.tows

# Clean up the Infs that show up because weights weren't taken on some tows and/or for some species.
gb.crab.all.2 <- gb.crab.all[,-which(names(gb.crab.all) == "ID"),]
crab.all.tows <- left_join(gb.all.unique.tows,data.frame(gb.crab.all.2),by = c('mission','setno','year','type','dist','speed','strat','area',
                                                                        'dmin','dmax','gear','geometry','season','xtype'))
# I only want to replace an NA if both totno and totwgt are NA, if they both are then it's a 0.  If only one is, it might
# be that they only measured one of wgt or numbers on a tow, so leave those as NAs.
crab.all.tows$totno[is.na(crab.all.tows$totno) & is.na(crab.all.tows$totwgt)] <- 0
crab.all.tows$totwgt[crab.all.tows$totno == 0 & is.na(crab.all.tows$totwgt)] <- 0

# this is the area swept for the WIIa gear
crab.all.tows$area.swept <-  crab.all.tows$dist*nm.to.km * w2a.ws.km
# Now get area swept for the NEST, very similar...
crab.all.tows$area.swept[crab.all.tows$gear == 15] <-  crab.all.tows$dist[crab.all.tows$gear == 15] *nm.to.km * w2a.ws.km
# Now do the standardization
crab.all.tows$kg.km2 <- crab.all.tows$totwgt/crab.all.tows$area.swept
crab.all.tows$n.km2 <- crab.all.tows$totno/crab.all.tows$area.swept
crab.all.tows$Ind.wgt <- crab.all.tows$totwgt/crab.all.tows$totno
crab.all.tows$Ind.wgt[is.nan(crab.all.tows$Ind.wgt)] <- NA
# Lets make an index for the spring and summer
# I am going to keep the comparative survey tows in here so we have a complete time series in the spring
# as in 2022 only type 5 tows (comparative) were undertaken. This leads to a higher sample size in 2023/24
# when the spring had regular and comparative tows. Doing this does not greatly impact the results
# 2023 and 2024 saw the highest numbers of crabs and the number hardly chages if you remove or keep the compariative survey tows..
crab.spring.4.index <- crab.all.tows |> collapse::fsubset(season == "SPRING" & xtype %in% c(1,5))
crab.summer.4.index <- crab.all.tows |> collapse::fsubset(season == "SUMMER" & xtype %in% c(1,5))
crab.spring.ts <- crab.spring.4.index |> collapse::fgroup_by(year) |> collapse::fsummarise(mn.num.km2 = mean(n.km2,na.rm=T),
                                                                                           mn.wgt.km2 = mean(kg.km2,na.rm=T),
                                                                                           sd.num = sd(n.km2,na.rm=T),
                                                                                           sd.wgt = sd(kg.km2,na.rm=T),
                                                                                           #med.num.km2 = median(n.km2,na.rm=T),
                                                                                           #med.wgt.km2 = median(kg.km2,na.rm=T),
                                                                                           mn.ind.kg = mean(Ind.wgt,na.rm=T),
                                                                                           #med.ind.kg = median(Ind.wgt,na.rm=T),
                                                                                           sd.ind.kg = sd(Ind.wgt,na.rm=T))
# Summer
crab.summer.ts <- crab.summer.4.index |> collapse::fgroup_by(year) |> collapse::fsummarise(mn.num.km2 = mean(n.km2,na.rm=T),
                                                                                           mn.wgt.km2 = mean(kg.km2,na.rm=T),
                                                                                           sd.num = sd(n.km2,na.rm=T),
                                                                                           sd.wgt = sd(kg.km2,na.rm=T),
                                                                                           #med.num.km2 = median(n.km2,na.rm=T),
                                                                                           #med.wgt.km2 = median(kg.km2,na.rm=T),
                                                                                           mn.ind.kg = mean(Ind.wgt,na.rm=T),
                                                                                           #med.ind.kg = median(Ind.wgt,na.rm=T),
                                                                                           sd.ind.kg = sd(Ind.wgt,na.rm=T))

crab.summer.ts$Species <- "Crab species"
crab.spring.ts$Species <- "Crab species"
# now sea stars
# Clean up the Infs that show up because weights weren't taken on some tows and/or for some species.
gb.ss.all.2 <- gb.ss.all[,-which(names(gb.ss.all) == "ID"),]
ss.all.tows <- left_join(gb.all.unique.tows,data.frame(gb.ss.all.2),by = c('mission','setno','year','type','dist','speed','strat','area',
                                                                             'dmin','dmax','gear','geometry','season','xtype'))
# I only want to replace an NA if both totno and totwgt are NA, if they both are then it's a 0.  If only one is, it might
# be that they only measured one of wgt or numbers on a tow, so leave those as NAs.
ss.all.tows$totno[is.na(ss.all.tows$totno) & is.na(ss.all.tows$totwgt)] <- 0
ss.all.tows$totwgt[ss.all.tows$totno == 0 & is.na(ss.all.tows$totwgt)] <- 0
# this is the area swept for the WIIa gear
ss.all.tows$area.swept <-  ss.all.tows$dist*nm.to.km * w2a.ws.km
# Now get area swept for the NEST, very similar...
ss.all.tows$area.swept[ss.all.tows$gear == 15] <-  ss.all.tows$dist[ss.all.tows$gear == 15] *nm.to.km * w2a.ws.km
# Now do the standardization
ss.all.tows$kg.km2 <- ss.all.tows$totwgt/ss.all.tows$area.swept
ss.all.tows$n.km2 <- ss.all.tows$totno/ss.all.tows$area.swept
ss.all.tows$Ind.wgt <- ss.all.tows$totwgt/ss.all.tows$totno
ss.all.tows$Ind.wgt[is.nan(ss.all.tows$Ind.wgt)] <- NA
# Lets make an index for the spring and summer
# I am going to keep the comparative survey tows in here so we have a complete time series in the spring
# as in 2022 only type 5 tows (comparative) were undertaken. This leads to a higher sample size in 2023/24
# when the spring had regular and comparative tows. Doing this does not greatly impact the results
# 2023 and 2024 saw incredible numbers of sea-stars.
ss.spring.4.index <- ss.all.tows |> collapse::fsubset(season == "SPRING" & xtype %in% c(1,5))
ss.summer.4.index <- ss.all.tows |> collapse::fsubset(season == "SUMMER" & xtype %in% c(1,5))
ss.spring.ts <- ss.spring.4.index |> collapse::fgroup_by(year) |> collapse::fsummarise(mn.num.km2 = mean(n.km2,na.rm=T),
                                                                                           mn.wgt.km2 = mean(kg.km2,na.rm=T),
                                                                                           sd.num = sd(n.km2,na.rm=T),
                                                                                           sd.wgt = sd(kg.km2,na.rm=T),
                                                                                           #med.num.km2 = median(n.km2,na.rm=T),
                                                                                           #med.wgt.km2 = median(kg.km2,na.rm=T),
                                                                                           mn.ind.kg = mean(Ind.wgt,na.rm=T),
                                                                                           #med.ind.kg = median(Ind.wgt,na.rm=T),
                                                                                           sd.ind.kg = sd(Ind.wgt,na.rm=T))
# Summer
ss.summer.ts <- ss.summer.4.index |> collapse::fgroup_by(year) |> collapse::fsummarise(mn.num.km2 = mean(n.km2,na.rm=T),
                                                                                       mn.wgt.km2 = mean(kg.km2,na.rm=T),
                                                                                       sd.num = sd(n.km2,na.rm=T),
                                                                                       sd.wgt = sd(kg.km2,na.rm=T),
                                                                                       #med.num.km2 = median(n.km2,na.rm=T),
                                                                                       #med.wgt.km2 = median(kg.km2,na.rm=T),
                                                                                       mn.ind.kg = mean(Ind.wgt,na.rm=T),
                                                                                       #med.ind.kg = median(Ind.wgt,na.rm=T),
                                                                                       sd.ind.kg = sd(Ind.wgt,na.rm=T))

ss.summer.ts$Species <- "Astropecten and Asterias Species"
ss.spring.ts$Species <- "Astropecten and Asterias Species"
# Merge these together...
pred.spring.ts <- rbind(crab.spring.ts,ss.spring.ts)
pred.summer.ts <- rbind(crab.summer.ts,ss.summer.ts)



############################################################ 
# Step 6 Now make the plots
# Make the objects sf objects...
ss.all.tows.sf <- st_as_sf(ss.all.tows)
ss.spring.type.1 <- ss.all.tows.sf[ss.all.tows.sf$xtype ==1 & ss.all.tows.sf$season == "SPRING",]
ss.summer.type.1 <- ss.all.tows.sf[ss.all.tows.sf$xtype ==1 & ss.all.tows.sf$season == "SUMMER",]
ss.spring.all <- ss.all.tows.sf[ss.all.tows.sf$season == "SPRING",]
crab.all.tows.sf <- st_as_sf(crab.all.tows)
crab.spring.type.1 <- crab.all.tows.sf[crab.all.tows.sf$xtype ==1 & crab.all.tows.sf$season == "SPRING",]
crab.summer.type.1 <- crab.all.tows.sf[crab.all.tows.sf$xtype ==1 & crab.all.tows.sf$season == "SUMMER",]
crab.spring.all <- crab.all.tows.sf[crab.all.tows.sf$season == "SPRING",]


# Try to get anything for GB from the new vessels, warning in the plot has something to do with the xlim/ylim, ignore it
ss.con.fac.wgt.plt <- basemap + geom_sf(data=ss.spring.all[ss.spring.all$totwgt >0,], aes(size=totwgt,shape=totwgt),color='grey15',pch=16) + 
                                geom_sf(data=ss.spring.all[ss.spring.all$totwgt ==0,],pch=4) +
                                scale_shape_manual(values = c("Zero" = 4, "Non-zero" = 16),  breaks = c("Zero", "Non-zero")) +
                                facet_wrap(~year) + 
                                ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + 
                                xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) + labs(size = "Kg per km2") +
                                theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_spring_wgt_spatial_with_conversion_factors.png",
          ss.con.fac.wgt.plt,base_height = 8,base_width = 9)

ss.con.fac.num.plt <- basemap + geom_sf(data=ss.spring.all, aes(size=n.km2),pch=19,colour ="blue") + facet_wrap(~year) + 
                                ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + 
                                labs(size = "N per km2") +
                                theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_spring_num_spatial_with_conversion_factors.png",
          ss.con.fac.wgt.plt,base_height = 8,base_width = 9)

ss.ind.wgt.plt <- basemap + geom_sf(data=ss.spring.all, aes(size=Ind.wgt),pch=19,colour ="grey15") + facet_wrap(~year) + 
                            ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + 
                            labs(size = "Mean individual weight per tow (kg)") + #scale_size_continuous(transform = "log10")
                            theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_spring_ind_wgt_spatial_with_conversion_factors.png",
          ss.ind.wgt.plt,base_height = 8,base_width = 9)


################## NOW THE CRAB Spatial Plots

# Try to get anything for GB from the new vessels, warning in the plot has something to do with the xlim/ylim, ignore it
crab.con.fac.wgt.plt <- basemap + geom_sf(data=crab.spring.all[crab.spring.all$totwgt > 0,], aes(size=totwgt,shape=totwgt),pch=16,colour ="grey15") + 
                                  geom_sf(data=crab.spring.all[crab.spring.all$totwgt ==0,],pch=4) +
                                  facet_wrap(~year) + 
                                  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) + labs(size = "Kg per km2") +
                                  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_spring_wgt_spatial_with_conversion_factors.png",
          crab.con.fac.wgt.plt,base_height = 8,base_width = 9)

crab.con.fac.num.plt <- basemap + geom_sf(data=crab.spring.all, aes(size=n.km2),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "N per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_spring_num_spatial_with_conversion_factors.png",
          crab.con.fac.wgt.plt,base_height = 8,base_width = 9)

crab.ind.wgt.plt <- basemap + geom_sf(data=crab.spring.all, aes(size=Ind.wgt),pch=19,colour ="orange") + facet_wrap(~year) + 
  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "Mean individual \nwgt per tow (kg)") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_spring_ind_wgt_spatial_with_conversion_factors.png",
          crab.ind.wgt.plt,base_height = 8,base_width = 9)



# These plots make the presentation cut...
# first the Sea stars
ss.ts.plt <- ggplot(pred.spring.ts[pred.spring.ts$Species == "Astropecten and Asterias Species",]) + geom_line(aes(x=year,y=mn.wgt.km2),size=1.5) + 
  xlab("") + ylab("Seastar Biomass Index (kg\u22C5km\u207B\u00B2)") + ggtitle("Astropecten and Asterias Species (Spring)") +
  scale_color_manual(values=c("blue","firebrick2")) + scale_x_continuous(breaks=seq(2007,2024,by=2))
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_spring_wgt_ts_with_conversion_factor.png",ss.ts.plt,base_height = 8,base_width = 8)


# Now the crabs
crab.ts.plt <- ggplot(pred.spring.ts[pred.spring.ts$Species == "Crab species",]) + geom_line(aes(x=year,y=mn.wgt.km2),size=1.5) + 
  xlab("") + ylab("Crab Biomass Index (kg\u22C5km\u207B\u00B2)") + ggtitle("Crab species (Spring)") +
  scale_color_manual(values=c("blue","firebrick2")) + scale_x_continuous(breaks=seq(2007,2024,by=2))
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crab_spring_wgt_ts_with_conversion_factor.png",crab.ts.plt,base_height = 9,base_width = 8)

# Combine these figures....
theme_set(theme_few(base_size=18))
ss.ts.plt.4.combo <- ggplot(pred.spring.ts[pred.spring.ts$Species == "Astropecten and Asterias Species",]) + geom_line(aes(x=year,y=mn.wgt.km2),size=1.5) + 
  xlab("") + ylab("Seastar Biomass Index (kg\u22C5km\u207B\u00B2)") + 
  scale_color_manual(values=c("blue","firebrick2")) + scale_x_continuous(breaks=seq(2007,2025,by=3),labels=NULL)

crab.ts.plt.4.combo <- ggplot(pred.spring.ts[pred.spring.ts$Species == "Crab species",]) + geom_line(aes(x=year,y=mn.wgt.km2),size=1.5) + 
  xlab("") + ylab("Crab Biomass Index (kg\u22C5km\u207B\u00B2)") +
  scale_color_manual(values=c("blue","firebrick2")) + scale_x_continuous(breaks=seq(2007,2025,by=3))

pred.ts.plt <- plot_grid(ss.ts.plt.4.combo,crab.ts.plt.4.combo,nrow=2)
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/pred_ts_with_conversion_factor.png",pred.ts.plt,base_height = 9,base_width = 6)
#save_plot("D:/Github/GB_SEAM/Figures/Productivity_paper/pred_ts_with_conversion_factor.png",pred.ts.plt,base_height = 9,base_width = 6)
#save_plot("D:/Github/GB_SEAM/Figures/Productivity_paper/pred_ts_with_conversion_factor.tiff",pred.ts.plt,base_height = 9,base_width = 6)


# Save the data used for these two figures.
saveRDS(pred.spring.ts,"D:/Github/GB_SEAM/data/RV_pred_data_spring_with_conversion.Rds")

###############################################################
### SUPPLEMENTAL #1
# Other plots I have made, but I think go in the backup bin for now at least...
# SUmmer Survey type 1
ss.con.fac.wgt.plt <- basemap + geom_sf(data=ss.summer.type.1[ss.summer.type.1$is.zero ==F,], aes(size=totwgt,shape=totwgt),color='firebrick2',pch=16) + 
  geom_sf(data=ss.summer.type.1[ss.summer.type.1$is.zero ==T,],pch=4) +
  scale_shape_manual(
    values = c("Zero" = 4, "Non-zero" = 16),  # 4 = ×, 16 = filled circle
    breaks = c("Zero", "Non-zero"))+
  facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + 
  xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) + labs(size = "Kg per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_summer_wgt_spatial_with_conversion_factors.png",
          ss.con.fac.wgt.plt,base_height = 8,base_width = 9)

ss.con.fac.num.plt <- basemap + geom_sf(data=ss.summer.type.1, aes(size=n.km2),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "N per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_summer_num_spatial_with_conversion_factors.png",
          ss.con.fac.wgt.plt,base_height = 8,base_width = 9)

ss.ind.wgt.plt <- basemap + geom_sf(data=ss.summer.type.1, aes(size=Ind.wgt),pch=19,colour ="orange") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "Mean individual \nwgt per tow (kg)") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_summer_ind_wgt_spatial_with_conversion_factors.png",
          ss.ind.wgt.plt,base_height = 8,base_width = 9)

#All data
ss.con.fac.wgt.plt <- basemap + geom_sf(data=ss.all.tows.sf, aes(size=kg.km2),pch=19,colour ="red") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) + labs(size = "Kg per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_all_data_wgt_spatial_with_conversion_factors.png",
          ss.con.fac.wgt.plt,base_height = 8,base_width = 9)

ss.con.fac.num.plt <- basemap + geom_sf(data=ss.all.tows.sf, aes(size=n.km2),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "N per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_all_data_num_spatial_with_conversion_factors.png",
          ss.con.fac.wgt.plt,base_height = 8,base_width = 9)

ss.ind.wgt.plt <- basemap + geom_sf(data=ss.all.tows.sf, aes(size=Ind.wgt),pch=19,colour ="orange") + facet_wrap(~year) + 
  ggtitle("ASTROPECTEN + ASTERIAS (with conversion factors)") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "Mean individual \nwgt per tow (kg)") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Seastars_all_data_ind_wgt_spatial_with_conversion_factors.png",
          ss.ind.wgt.plt,base_height = 8,base_width = 9)

# Now the other crab figures
# SUmmer Survey type 1
crab.con.fac.wgt.plt <- basemap + geom_sf(data=crab.summer.type.1, aes(size=kg.km2),pch=19,colour ="red") + facet_wrap(~year) + 
  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) + labs(size = "Kg per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_summer_wgt_spatial_with_conversion_factors.png",
          crab.con.fac.wgt.plt,base_height = 8,base_width = 9)

crab.con.fac.num.plt <- basemap + geom_sf(data=crab.summer.type.1, aes(size=n.km2),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "N per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_summer_num_spatial_with_conversion_factors.png",
          crab.con.fac.wgt.plt,base_height = 8,base_width = 9)

crab.ind.wgt.plt <- basemap + geom_sf(data=crab.summer.type.1, aes(size=Ind.wgt),pch=19,colour ="orange") + facet_wrap(~year) + 
  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "Mean individual \nwgt per tow (kg)") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_summer_ind_wgt_spatial_with_conversion_factors.png",
          crab.ind.wgt.plt,base_height = 8,base_width = 9)

#All data
crab.con.fac.wgt.plt <- basemap + geom_sf(data=crab.all.tows.sf, aes(size=kg.km2),pch=19,colour ="red") + facet_wrap(~year) + 
  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) + labs(size = "Kg per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_all_data_wgt_spatial_with_conversion_factors.png",
          crab.con.fac.wgt.plt,base_height = 8,base_width = 9)

crab.con.fac.num.plt <- basemap + geom_sf(data=crab.all.tows.sf, aes(size=n.km2),pch=19,colour ="blue") + facet_wrap(~year) + 
  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "N per km2") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_all_data_num_spatial_with_conversion_factors.png",
          crab.con.fac.wgt.plt,base_height = 8,base_width = 9)

crab.ind.wgt.plt <- basemap + geom_sf(data=crab.all.tows.sf, aes(size=Ind.wgt),pch=19,colour ="orange") + facet_wrap(~year) + 
  ggtitle("Crab Species") + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3))  + labs(size = "Mean individual \nwgt per tow (kg)") +
  theme(legend.position = "bottom",legend.direction = 'horizontal')
save_plot("Y:/Offshore/Assessment/2025/Supporting_tasks/predators_on_GB/Crabs_all_data_ind_wgt_spatial_with_conversion_factors.png",
          crab.ind.wgt.plt,base_height = 8,base_width = 9)


# Time series






########################################
### SUPPLEMENTAL #2
# Do I still wanna do this?


# Now can I get the number of tows in this domain each year...
# And here are the number of unique tows on GB from the RV survey.
num.tows <- as.data.frame(gb.type.1.spring) |> collapse::fgroup_by(year) |> collapse::fsummarise(total.tows = dplyr::n_distinct(paste0(mission, ".", setno)))
missing.years <- setdiff(years,num.tows$year)
mis <- data.frame(year=missing.years,total.tows = rep(NA,length(missing.years)))
num.tows <- rbind(num.tows,mis)
num.tows <- num.tows[order(num.tows$year),]

# Now how many tows actually observed seastars or crabs
ss.tows <- as.data.frame(gb.ss.type.1.spring) |> collapse::fgroup_by(year) |> collapse::fsummarise(ss.tows = dplyr::n_distinct(paste0(mission, ".", setno)))
missing.ss.years <- setdiff(years,ss.tows$year)
mcss <- data.frame(year=missing.ss.years,ss.tows = rep(NA,length(missing.ss.years)))
ss.tows <- rbind(ss.tows,mcss)
ss.tows <- ss.tows[order(ss.tows$year),]

crab.tows <- as.data.frame(gb.crab.type.1.spring) |> collapse::fgroup_by(year) |> collapse::fsummarise(crab.tows = dplyr::n_distinct(paste0(mission, ".", setno)))
missing.crab.years <- setdiff(years,crab.tows$year)
mcy <- data.frame(year=missing.crab.years,crab.tows = rep(NA,length(missing.crab.years)))
crab.tows <- rbind(crab.tows,mcy)
crab.tows <- crab.tows[order(crab.tows$year),]
# Merge these together
tow.data <- left_join(num.tows,crab.tows,by="year")
tow.data <- left_join(tow.data,ss.tows,by="year")

tow.props <- data.frame(year = rep(min(tow.data$year):max(tow.data$year),2),name = c(rep("prop.crab",nrow(tow.data)),rep("prop.ss",nrow(tow.data))),
                        prop = c(tow.data$crab.tows/tow.data$total.tows,tow.data$ss.tows/tow.data$total.tows))
# stack this for ggplot...
tow.data.long <- pivot_longer(tow.data,cols = c("ss.tows","total.tows","crab.tows"))

# Now make some figures...
ggplot(tow.data.long) + geom_line(aes(x=year,y=value,group = name,color=name),size=1.5) + xlab("") + ylab("Number of tows") +
  scale_color_manual(name="",values = c("blue","firebrick2","darkgrey"),labels=c("Crabs","Seastars","Total")) +ylim(c(0,60))

ggplot(tow.props) + geom_line(aes(x=year,y=prop,group = name,color=name),size=1.5) + xlab("") + ylab("Proportion of tows") +
  scale_color_manual(name="Species",values = c("blue","firebrick2"),labels=c("Crabs","Seastars")) + ylim(c(0,1))

basemap + geom_sf(data=gb.type.1.spring) + facet_wrap(~year) + xlim(c(-67.25,-65.6)) + ylim(c(41.12,42.3)) 

