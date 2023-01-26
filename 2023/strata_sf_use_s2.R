# interactions between spherical geometry and our strata shapefiles
# informs survey design tow allocation

banks <- c("GBa", "GBb", "BBn", "BBs", "Sab")
s2_read <- c(T, F)
s2_calc <- c(T, F)
df <- data.frame(expand.grid(banks, s2_read, s2_calc))
names(df) <- c("label", "s2_read", "s2_calc")
df <- arrange(df, label, s2_read, s2_calc)

df3 <- NULL
for(i in 1:nrow(df)){
  print(df[i,])
  sf_use_s2(df$s2_read[i])
  
  surv.polyset <- tryCatch({
    combo.shp(paste0(gis.repo, "/offshore_survey_strata"),make.sf=T,make.polys=F, quiet=T)},
    error=function(err) {return(NA)})
  if(is.data.frame(surv.polyset)){
    bank <- df$label[i]
    shp_strata <- surv.polyset[surv.polyset$label==bank,]
    sf_use_s2(df$s2_calc[i])
    
    shp_strata$area <- tryCatch({
      st_area(shp_strata)},
      error= function(err) {return(NA)})
    if(any(!is.na(shp_strata$area))){
      shp_strata$alloc <- st_area(shp_strata)/sum(st_area(shp_strata))
    }
    if(any(is.na(shp_strata$area))){
      shp_strata$alloc <- NA
    }
  }
  df2 <- full_join(df[i,], shp_strata)
  df3 <- rbind(df3, df2)
  
}

df3[is.na(df3$area),]
df3[is.na(df3$alloc),]

ggplot() + geom_point(data=df3[!is.na(df3$area),], aes(s2_read, s2_calc, colour=as.numeric(area))) + facet_wrap(~label)
ggplot() + geom_point(data=df3[!is.na(df3$alloc),], aes(s2_read, s2_calc, colour=as.numeric(alloc))) + facet_wrap(~label)
# so it only totally breaks for GBa

ggplot() + geom_point(data=df3[df3$label=="GBa",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
# for GBa, use S2_read TRUE and s2_calc FALSE
round(df3[df3$label=="GBa" & df3$s2_read==TRUE & df3$s2_calc==FALSE,]$alloc*200)


ggplot() + geom_point(data=df3[df3$label=="BBn",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
# for BBn and all others, use S2_read TRUE and S2_calc TRUE
round(df3[df3$label=="BBn" & df3$s2_read==TRUE & df3$s2_calc==TRUE,]$alloc*100)

ggplot() + geom_point(data=df3[df3$label=="BBs",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
round(df3[df3$label=="BBs" & df3$s2_read==TRUE & df3$s2_calc==TRUE,]$alloc*25)

ggplot() + geom_point(data=df3[df3$label=="Sab",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
round(df3[df3$label=="Sab" & df3$s2_read==TRUE & df3$s2_calc==TRUE,]$alloc*100)

ggplot() + geom_point(data=df3[df3$label=="GBb",], aes(are_km2, as.numeric(area)/1000000, colour=s2_read)) + facet_grid(s2_read~s2_calc) +
  geom_abline(slope=1, intercept=0)
round(df3[df3$label=="GBb" & df3$s2_read==TRUE & df3$s2_calc==TRUE,]$alloc*30)
