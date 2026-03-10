#RelF

# exploitation rates (relative, not modelled)
# using survey year, so June to May for BBn, Sep to Aug for GBb
load("C:/Users/keyserf/Documents/temp_data/Data/Fishery_data/Summary/2025/OSAC_tidy_logs.RData")
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish.dat <- fish.dat[!is.na(fish.dat$lat),]
fish_sf <- st_as_sf(fish.dat, coords=c("lon", "lat"), crs=4326)
fish_sf2 <- fish_sf 
fish_sf2$sfa[grep(x=fish_sf2$sfa, "25")] <- 25
fish_sf2$sfa[grep(x=fish_sf2$sfa, "26")] <- 26
fish_sf2$sfa[grep(x=fish_sf2$sfa, "27")] <- 27
fish_sf2$sfa[fish_sf2$bank %in% c("Ban", "Sab","Mid")] <- 25
fish_sf2$sfa[fish_sf2$bank %in% c("BBn", "BBs", "Ger")] <- 26
fish_sf2$sfa[fish_sf2$bank %in% c("GBb")] <- 27

fish_sf2$surv.year <- fish_sf2$year 
fish_sf2$surv.year[fish_sf2$month %in% 1:5 & fish_sf2$sfa %in% 25:26 ] <- fish_sf2$year[fish_sf2$month %in% 1:5 & fish_sf2$sfa %in% 25:26]-1
fish_sf2$surv.year[fish_sf2$month %in% 1:8 & fish_sf2$sfa %in% 27] <- fish_sf2$year[fish_sf2$month %in% 1:8 & fish_sf2$sfa %in% 27]-1

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")
strata <- github_spatial_import("survey_boundaries", "survey_boundaries.zip", quiet=T)
fish_sf2 <- fish_sf2 %>% st_transform(32619)
fish_sf2 <- st_intersection(fish_sf2, st_transform(strata, 32619))
fish.dat <- fish_sf2
#st_geometry(fish.dat) <- NULL

by.sy <- fish.dat %>% data.frame() %>%  group_by(bank, surv.year)%>%  dplyr::summarize(sum = sum(pro.repwt/1000))

by.sy[by.sy$bank=="BBn" & by.sy$surv.year>1990,]

# fish.dat$surv.year[fish.dat$month %in% 6:12 & fish.dat$sfa %in% 25:26 ] <- fish.dat$year[fish.dat$month %in% 6:12 & fish.dat$sfa %in% 25:26]+1
# fish.dat$surv.year[fish.dat$month %in% 9:12 & fish.dat$sfa %in% 27] <- fish.dat$year[fish.dat$month %in% 9:12 & fish.dat$sfa %in% 27]+1

#tail(fish.dat[, c("fished", "month", "year", "surv.year")], 50)
survey.yr <- fish.dat %>%
  group_by(surv.year, bank, sfa) %>%
  dplyr::summarize(mt = sum(pro.repwt)/1000) %>%
  dplyr::mutate(year=surv.year)

fishing.yr <- fish.dat %>%
  group_by(year, bank, sfa) %>%
  dplyr::summarize(mt = sum(pro.repwt)/1000) 

load("C:/Users/keyserf/Documents/temp_data/Data/Survey_data/2025/Survey_summary_output/Survey_all_results.Rdata")

banks <- c("BBn","Ger", "GBb")
relative.F <- NULL
for(b in banks) {
  # if(b=="Sab/Mid"){
  #   sab <- cbind(survey.obj[["Sab"]]$model.dat[, c("year", "I")], bank="Sab")
  #   mid <- cbind(survey.obj[["Mid"]]$model.dat[, c("year", "I")], bank="Mid")
  #   sabmid <- full_join(sab, mid) %>%
  #     group_by(year) %>%
  #     summarize(I=sum(I, na.rm=T)) #/0.35) # survey catchability (between 0.2 and 0.5), using this would mean "q-corrected", don't do this
  #   fishing <- survey.yr %>%
  #     filter(bank %in% c("Sab", "Mid")) %>%
  #     group_by(year) %>%
  #     summarize(mt=sum(mt)) %>%
  #     mutate(bank="Sab/Mid")
  #   compare <- full_join(sabmid, fishing[,c("year", "bank", "mt")])
  # }
  
  #if(!b=="Sab/Mid"){
  if(b=="Ger") {
    atow<-800*2.4384/10^6 # area of standard tow in km2
    ger.shape <- st_read("C:/Users/keyserf/Documents/Github/GIS_layers/other_boundaries/WGS_84_German.shp") %>% st_make_valid() %>% st_transform(32619)
    #ger.shape <- ger.shape %>% st_transform(crs = 32619) # BBn is right on the 19/20 border so think they are basically equivalent options here
    # Bring in the survey data
    ger.area.km2 <- st_area(ger.shape)/1e6
    units(ger.area.km2) <- NULL
    # Bring in the survey data
    
    surv.26c <- merged.survey.obj # Need to add in 2020 missing...
    surv.26c[nrow(surv.26c)+1,] <- NA
    surv.26c$year[nrow(surv.26c)] <- 2020
    # and reorder
    surv.26c <- surv.26c[order(surv.26c$year),]
    # Now the units of german are....? If it is grams/tow... this puts us in total tonnes,  must confirm the units coming out of merged survey object.
    surv.26c$I <- surv.26c$I /atow*ger.area.km2/1e6
    
    compare <- full_join(surv.26c[, c("year", "I")], survey.yr[survey.yr$bank==b,c("year", "bank", "mt")])
  }
  if(!b=="Ger") compare <- full_join(survey.obj[[b]]$model.dat[, c("year", "I")], survey.yr[survey.yr$bank==b &!is.na(survey.yr$bank),c("year", "bank", "mt")])
  # }
  compare$relative.F <- compare$mt / (compare$I/0.33) # survey catchability? don't use this
  relative.F <- rbind(relative.F, compare)
}



relFyrs <- expand.grid(year=min(relative.F$year, na.rm=T):max(relative.F$year, na.rm=T), bank=banks)
relative.F <- left_join(relFyrs, relative.F)
#relative.F <- relative.F[!is.na(relative.F$mt),]
#relative.F[relative.F$relative.F==Inf & !is.na(relative.F$relative.F),]$relative.F <- NA
relative.F$sfa[relative.F$bank %in% c("BBn", "BBs", "Ger", "BBn/BBs")] <- as.numeric(26)
relative.F$sfa[relative.F$bank %in% c("Ban", "Ban/Sab/Mid", "Sab/Mid", "Sab", "Mid")] <- as.numeric(25)
relative.F$sfa[relative.F$bank %in% c("GBb")] <- as.numeric(27)

flab <- "Relative fishing mortality"

relative.F$subarea[relative.F$bank=="Ban"] <- "25B"
relative.F$subarea[relative.F$bank=="Sab"] <- "25A-Sab"
relative.F$subarea[relative.F$bank=="Mid"] <- "25A-Mid"
relative.F$subarea[relative.F$bank=="BBn"] <- "26A"
relative.F$subarea[relative.F$bank=="BBs"] <- "26B"
relative.F$subarea[relative.F$bank=="Ger"] <- "26C"
relative.F$subarea[relative.F$bank=="GBb"] <- "27B"


exp26 <- ggplot() + geom_line(data=relative.F[relative.F$sfa==26,], aes(year, relative.F)) + 
  geom_point(data=relative.F[relative.F$sfa==26,], aes(year, relative.F)) +
  ylim(0,2) +
  facet_wrap(~subarea, ncol=1) +
  xlab("Year") +
  ylab(flab) +
  theme_bw()

exp27 <- ggplot() + geom_line(data=relative.F[relative.F$sfa==27,], aes(year, relative.F)) + 
  geom_point(data=relative.F[relative.F$sfa==27,], aes(year, relative.F)) +
  ylim(0,2) +
  facet_wrap(~subarea, ncol=1) +
  xlab("Year") +
  ylab(flab) +
  theme_bw()


relative.F[relative.F$year==2025, c("year", "bank", "I", "mt", "relative.F")]

relative.F %>% group_by(bank) %>%
  summarize(LTM.q.I = median(I/0.33, na.rm=T),
            LTM.RelF = median(relative.F, na.rm=T))


survey.obj$BBn$model.dat[survey.obj$BBn$model.dat$year==2025, ]


median(survey.obj$GBb$model.dat$I/0.33, na.rm=T)

load("C:/Users/keyserf/Documents/GitHub/Assessment_fns/Survey_and_OSAC/bookdown_report/officedown/summary4.Rdata")
View(summary_data$highlights)
