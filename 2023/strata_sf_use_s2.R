# interactions between spherical geometry and our strata shapefiles, using two different read in functions (combo.shp and github_spatial_import)
# and also defining UTM zone
# informs survey design tow allocation

banks <- c("GBa", "GBb", "BBn", "BBs", "Sab")
s2_read <- c(T, F)
s2_calc <- c(T, F)
df <- data.frame(expand.grid(banks, s2_read, s2_calc))
names(df) <- c("label", "s2_read", "s2_calc")
df <- arrange(df, label, s2_read, s2_calc)

gis.repo <- "C:/Users/keyserf/Documents/GitHub/GIS_layers/"

# read in functions and load packages
funs <- c("https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/combo_shp.R",
          "https://raw.githubusercontent.com/freyakeyser/Assessment_fns/master/Maps/github_spatial_import.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
} 

# calculate strata areas and allocations for every combination of s2_use_sf, the two read-in functions, 
# coded UTM, 32619, and 32620

df3 <- NULL
df5 <- NULL
for(i in 1:nrow(df)){
  print(df[i,])
  sf_use_s2(df$s2_read[i])
  
  surv.polyset <- tryCatch({
    combo.shp(paste0(gis.repo, "/offshore_survey_strata"),make.sf=T,make.polys=F, quiet=T)},
    error=function(err) {return(NA)})
  
  surv.polyset.2 <- tryCatch({
    github_spatial_import(subfolder = "offshore_survey_strata", zipname = "offshore_survey_strata.zip", quiet=T)},
    error=function(err) {return(NA)})
  
  # uses combo.shp
  if(is.data.frame(surv.polyset)){
    bank <- df$label[i]
    shp_strata <- surv.polyset[surv.polyset$label==bank,]
    sf_use_s2(df$s2_calc[i])
    
    # guess UTM code
    shp_strata$utm <-  tryCatch({
      SEBDAM::find_utm_code(st_coordinates(st_centroid(st_simplify(st_combine(shp_strata)))))},
      error= function(err) {return(NA)})
    
    shp_strata$area_utm <- tryCatch({
      st_area(st_transform(shp_strata, crs=unique(shp_strata$utm)))},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata$area_utm))){
      shp_strata$alloc_utm <- st_area(st_transform(shp_strata, crs=unique(shp_strata$utm)))/sum(st_area(st_transform(shp_strata, crs=unique(shp_strata$utm))))
    }
    if(any(is.na(shp_strata$area_utm))){
      shp_strata$alloc_utm <- NA
    }
    
    shp_strata$area_19 <- tryCatch({
      st_area(st_transform(shp_strata, crs=32619))},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata$area_19))){
      shp_strata$alloc_19 <- st_area(st_transform(shp_strata, crs=32619))/sum(st_area(st_transform(shp_strata, crs=32619)))
    }
    if(any(is.na(shp_strata$area_19))){
      shp_strata$alloc_19 <- NA
    }
    
    shp_strata$area_20 <- tryCatch({
      st_area(st_transform(shp_strata, crs=32620))},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata$area_20))){
      shp_strata$alloc_20 <- st_area(st_transform(shp_strata, crs=32620))/sum(st_area(st_transform(shp_strata, crs=32620)))
    }
    if(any(is.na(shp_strata$area_20))){
      shp_strata$alloc_20 <- NA
    }
    
    shp_strata$area <- tryCatch({
      st_area(shp_strata)},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata$area))){
      shp_strata$alloc <- st_area(shp_strata)/sum(st_area(shp_strata))
    }
    if(any(is.na(shp_strata$area))){
      shp_strata$alloc <- NA
    }
  
    df2 <- full_join(df[i,], shp_strata)
    df3 <- rbind(df3, df2)
  }
  
  # uses github_spatial_import
  if(is.data.frame(surv.polyset.2)){
    bank <- df$label[i]
    shp_strata.2 <- surv.polyset.2[surv.polyset.2$label==bank,]
    sf_use_s2(df$s2_calc[i])
    
    # guess UTM code
    shp_strata.2$utm <- tryCatch({
      SEBDAM::find_utm_code(st_coordinates(st_centroid(st_simplify(st_combine(shp_strata.2)))))},
      error= function(err) {return(NA)})
    
    shp_strata.2$area_utm <- tryCatch({
      st_area(st_transform(shp_strata.2, crs=unique(shp_strata.2$utm)))},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata.2$area_utm))){
      shp_strata.2$alloc_utm <- st_area(st_transform(shp_strata.2, crs=unique(shp_strata.2$utm)))/sum(st_area(st_transform(shp_strata.2, crs=unique(shp_strata.2$utm))))
    }
    if(any(is.na(shp_strata.2$area_utm))){
      shp_strata.2$alloc_utm <- NA
    }
  
    shp_strata.2$area_19 <- tryCatch({
      st_area(st_transform(shp_strata.2, crs=32619))},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata.2$area_19))){
      shp_strata.2$alloc_19 <- st_area(st_transform(shp_strata.2, crs=32619))/sum(st_area(st_transform(shp_strata.2, crs=32619)))
    }
    if(any(is.na(shp_strata.2$area_19))){
      shp_strata.2$alloc_19 <- NA
    }
    
    shp_strata.2$area_20 <- tryCatch({
      st_area(st_transform(shp_strata.2, crs=32620))},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata.2$area_20))){
      shp_strata.2$alloc_20 <- st_area(st_transform(shp_strata.2, crs=32620))/sum(st_area(st_transform(shp_strata.2, crs=32620)))
    }
    if(any(is.na(shp_strata.2$area_20))){
      shp_strata.2$alloc_20 <- NA
    }
    
    shp_strata.2$area <- tryCatch({
      st_area(shp_strata.2)},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata.2$area))){
      shp_strata.2$alloc <- st_area(shp_strata.2)/sum(st_area(shp_strata.2))
    }
    if(any(is.na(shp_strata.2$area))){
      shp_strata.2$alloc <- NA
    }
    
    df4 <- full_join(df[i,], shp_strata.2)
    df5 <- rbind(df5, df4)
  }
  
}

df3[is.na(df3$area),]
df3[is.na(df3$alloc),]

round(df3$alloc_19*100,0) == round(df3$alloc_20*100,0)
round(df3$alloc_utm*100,0) == round(df3$alloc_20*100,0)
round(df3$alloc_utm*100,0) == round(df3$alloc_19*100,0)
# UTM 19 and 20 give the same allocations

df3[which(!round(df3$alloc*100,0) == round(df3$alloc_19*100,0)),]
df3[df3$label=="BBn" & df3$PID %in% 1:2, c("s2_read", "s2_calc", "PID", "alloc_utm", "alloc_19", "alloc_20", "alloc")]
# there is an issue with tow allocation for BBn very low and low between WGS and Projected when s2_read and s2_calc are TRUE
# UTM gives 44 stations in very low, 17 stations in low; while WGS gives 43 and 18

ggplot() + geom_point(data=df3[!is.na(df3$area),], aes(s2_read, s2_calc, colour=as.numeric(area))) + facet_wrap(~label)
ggplot() + geom_point(data=df3[!is.na(df3$alloc),], aes(s2_read, s2_calc, colour=as.numeric(alloc))) + facet_wrap(~label)
ggplot() + geom_point(data=df5[!is.na(df5$area),], aes(s2_read, s2_calc, colour=as.numeric(area))) + facet_wrap(~label)
ggplot() + geom_point(data=df5[!is.na(df5$alloc),], aes(s2_read, s2_calc, colour=as.numeric(alloc))) + facet_wrap(~label)
# so it only totally breaks for GBa when using WGS area

ggplot() + geom_point(data=df3[!is.na(df3$area_utm),], aes(s2_read, s2_calc, colour=as.numeric(area_utm))) + facet_wrap(~label)
ggplot() + geom_point(data=df3[!is.na(df3$alloc_utm),], aes(s2_read, s2_calc, colour=as.numeric(alloc_utm))) + facet_wrap(~label)
ggplot() + geom_point(data=df5[!is.na(df5$area_utm),], aes(s2_read, s2_calc, colour=as.numeric(area_utm))) + facet_wrap(~label)
ggplot() + geom_point(data=df5[!is.na(df5$alloc_utm),], aes(s2_read, s2_calc, colour=as.numeric(alloc_utm))) + facet_wrap(~label)
# and only works when s2_calc is FALSE for UTM

ggplot() + geom_point(data=df3[df3$label=="GBa",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
ggplot() + geom_point(data=df5[df5$label=="GBa",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
ggplot() + geom_point(data=df3[df3$label=="GBa",], aes(are_km2, as.numeric(area_utm)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
ggplot() + geom_point(data=df5[df5$label=="GBa",], aes(are_km2, as.numeric(area_utm)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
# for GBa, use S2_read TRUE and s2_calc FALSE
round(df3[df3$label=="GBa" & df3$s2_read==TRUE & df3$s2_calc==FALSE,]$alloc*200)
round(df5[df5$label=="GBa" & df5$s2_read==TRUE & df5$s2_calc==FALSE,]$alloc*200)
round(df3[df3$label=="GBa" & df3$s2_read==TRUE & df3$s2_calc==FALSE,]$alloc_utm*200)
round(df5[df5$label=="GBa" & df5$s2_read==TRUE & df5$s2_calc==FALSE,]$alloc_utm*200)


ggplot() + geom_point(data=df3[df3$label=="BBn",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
ggplot() + geom_point(data=df3[df3$label=="BBn",], aes(are_km2, as.numeric(area_utm)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
# for BBn and all others, use S2_read FALSE and S2_calc FALSE (OR TRUE and TRUE, but you'll run into UTM issues!) 
round(df3[df3$label=="BBn" & df3$s2_read==FALSE & df3$s2_calc==FALSE,]$alloc*100)
round(df3[df3$label=="BBn" & df3$s2_read==FALSE & df3$s2_calc==FALSE,]$alloc_utm*100)

ggplot() + geom_point(data=df3[df3$label=="BBs",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
ggplot() + geom_point(data=df3[df3$label=="BBs",], aes(are_km2, as.numeric(area_utm)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
round(df3[df3$label=="BBs" & df3$s2_read==FALSE & df3$s2_calc==FALSE,]$alloc*25)
round(df3[df3$label=="BBs" & df3$s2_read==FALSE & df3$s2_calc==FALSE,]$alloc_utm*25)

ggplot() + geom_point(data=df3[df3$label=="Sab",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
ggplot() + geom_point(data=df3[df3$label=="Sab",], aes(are_km2, as.numeric(area_utm)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
round(df3[df3$label=="Sab" & df3$s2_read==FALSE & df3$s2_calc==FALSE,]$alloc*100)
round(df3[df3$label=="Sab" & df3$s2_read==FALSE & df3$s2_calc==FALSE,]$alloc_utm*100)

ggplot() + geom_point(data=df3[df3$label=="GBb",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
ggplot() + geom_point(data=df3[df3$label=="GBb",], aes(are_km2, as.numeric(area_utm)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
round(df3[df3$label=="GBb" & df3$s2_read==FALSE & df3$s2_calc==FALSE,]$alloc*30)
round(df3[df3$label=="GBb" & df3$s2_read==FALSE & df3$s2_calc==FALSE,]$alloc_utm*30)

### compare UTM to non UTM area and to original area
ggplot() + geom_point(data=df3, aes(as.numeric(area)/1000000, as.numeric(area_utm)/1000000)) + facet_grid(s2_read ~ s2_calc) + 
  geom_abline(slope=1, intercept=0)

ggplot() + geom_point(data=df5, aes(as.numeric(area)/1000000, as.numeric(area_utm)/1000000)) + facet_grid(s2_read ~ s2_calc) + 
  geom_abline(slope=1, intercept=0)

df3$area_utm/1000000 - df3$area/1000000
df3$area == df3$area
df3$area_utm == df3$area_utm

df5$area_utm/1000000 - df5$area/1000000
df5$area == df3$area
df5$area_utm == df3$area_utm

df3[is.na(df3$area_utm),]

df3 <- df3[(df3$label %in% c("GBb", "BBn", "BBs", "Sab") & df3$s2_read==FALSE & df3$s2_calc==FALSE) |
      (df3$label %in% c("GBa") & df3$s2_read==TRUE & df3$s2_calc==FALSE),]

df5 <- df5[(df5$label %in% c("GBb", "BBn", "BBs", "Sab") & df5$s2_read==FALSE & df5$s2_calc==FALSE) |
             (df5$label %in% c("GBa") & df5$s2_read==TRUE & df5$s2_calc==FALSE),]

### compare to survey.information

survey.info <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/survey_information.csv")
head(survey.info)
tail(survey.info)
names(survey.info)[which(names(survey.info)=="startyear")] <- "startyr"

info3 <- full_join(survey.info, dplyr::select(df3, -startyr))
info5 <- full_join(survey.info, dplyr::select(df5, -startyr))

info3[info3$label=="BBn" & !is.na(info3$area),]

# info3; area vs. csv and area_utm vs. csv
ggplot() + geom_point(data=info3, aes(as.numeric(area)/1000000, area_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales = "free") +
  xlab("calculated area (non-utm)") +
  ylab("recorded area_csv")

ggplot() + geom_point(data=info3, aes(as.numeric(area_utm)/1000000, area_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales = "free") +
  xlab("calculated area (utm)") +
  ylab("recorded area_csv")
# SAME

# info5; area vs. csv and area_utm vs. csv
ggplot() + geom_point(data=info5, aes(as.numeric(area)/1000000, area_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("calculated area (non-utm)") +
  ylab("recorded area_csv")

ggplot() + geom_point(data=info5, aes(as.numeric(area_utm)/1000000, area_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("calculated area (utm)") +
  ylab("recorded area_csv")
# SAME

# info5; area vs. shp and area_utm vs. shp
ggplot() + geom_point(data=info5, aes(as.numeric(area)/1000000, are_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("calculated area (non-utm)") +
  ylab("recorded area_shp")

ggplot() + geom_point(data=info5, aes(as.numeric(area_utm)/1000000, are_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("calculated area (utm)") +
  ylab("recorded area_shp")
# SAME

# info3; area vs. shp and area_utm vs. shp
ggplot() + geom_point(data=info3, aes(as.numeric(area)/1000000, are_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("calculated area (non-utm)") +
  ylab("recorded area_shp")

ggplot() + geom_point(data=info3, aes(as.numeric(area_utm)/1000000, are_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("calculated area (utm)") +
  ylab("recorded area_shp")

# info3; shp vs. csv
ggplot() + geom_point(data=info3, aes(are_km2, area_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("recorded area_shp") +
  ylab("recorded area_csv")

# info5; shp vs. csv
ggplot() + geom_point(data=info5, aes(are_km2, area_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("recorded area_shp") +
  ylab("recorded area_csv")

# UTM vs non UTM
ggplot() + geom_point(data=info3, aes(as.numeric(area)/1000000, as.numeric(area_utm)/1000000)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("area non-utm") +
  ylab("area utm")
# negligible difference
info3$area/info3$area_utm
round(info3$alloc,0) == round(info3$alloc_utm,0)

ggplot() + geom_point(data=info3, aes(as.numeric(area)/1000000, as.numeric(area_utm)/1000000)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales="free")+
  xlab("area non-utm") +
  ylab("area utm")


### the first two are the most important, because we rely on the csv for these measurements normally.
# plot them again here
ggplot() + geom_point(data=info3, aes(as.numeric(area)/1000000, area_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales = "free") +
  xlab("calculated area (non-utm)") +
  ylab("recorded area_csv")

ggplot() + geom_point(data=info3, aes(as.numeric(area_utm)/1000000, area_km2)) +
  geom_abline(slope=1, intercept=0) + facet_wrap(~label+startyr, scales = "free") +
  xlab("calculated area (utm)") +
  ylab("recorded area_csv")

info3 <- info3 %>%
  group_by(label, startyr, s2_read, s2_calc) %>%
  summarize(total_area = sum(area_km2)) %>%
  ungroup() %>%
  full_join(info3) %>% 
  mutate(alloc_cur = round(area_km2/total_area * 100,0))

dplyr::select(info3[which(!as.numeric(info3$alloc_cur) == round(as.numeric(info3$alloc_utm*100),0)),], -geometry, -ID, -towbl_r, -border, -PName, -X, -col, -Strata_ID, -Strt_ID, -towable_area)
# keep going here... need to figure out whether tow allocations from CSV match the ones using WGS, UTM, and s2 on/off

unique(dplyr::select(info3, label, utm))
#UTM 19: German, BBn, GBa, GBb
#UTM 20: BBs, Sab, Mid
#UTM 21: Ban, SPB

UTMs <- st_read("Y:/GISdata/Private/UTM_Zone_Boundaries/UTM_Zone_Boundaries.shp")
domain <- github_spatial_import(subfolder = "survey_boundaries", zipname = "survey_boundaries.zip", quiet=T)

ggplot() + geom_sf(data=domain) + 
  geom_sf(data=UTMs[UTMs$ZONE%in% 19:21 & UTMs$HEMISPHERE=="n",], fill=NA) +
  ylim(40, 50)

# conclusions:
# UTM 19 and 20 give the same tow allocations
# UTM vs. non-UTM give different tow allocations for BBn when s2_read and s2_calc are TRUE
# UTM gives 44 stations in very low, 17 stations in low; while WGS gives 43 and 18. We need to use WGS since that is consistent with past.


