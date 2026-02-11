# RV survey check

files <- list.files("C:/Users/keyserf/Downloads/RV_data/")
zip <- files[grep(x = files, pattern = ".zip")]
xlsx <- files[grep(x = files, pattern = ".xlsx")]
files <- files[!files %in% c(zip, xlsx)]
df <- NULL
for(i in 1:length(files)){
  dfname <- paste0("df_", gsub(x=files[i], ".csv", ""))
  print(dfname)

  file <- tryCatch(
    expr = {
      read.csv(paste0("C:/Users/keyserf/Downloads/RV_data/", files[i]))
    },
    error = function(e){
      NA
    }
  )

  df <- c(df, dfname)
  assign(x = dfname, value = file)
}


df_4VSW_2020_GSSPECIES[grep(x=df_4VSW_2020_GSSPECIES$SPEC, pattern="ARCTICA"),]
# 4304

head(df_4VSW_2020_GSCAT)
head(df_4VSW_2020_GSDET)
head(df_4VSW_2020_GSINF)

front <- unlist(strsplit(x = df, split="_GS"))
front <- unique(front[grep(x=front, pattern = "df")])
front <- front[-grep(x=front, pattern="MISSIONS")]
back <- unlist(strsplit(x = df, split="_GS"))
back <- unique(back[-grep(x=back, pattern = "df")])
back <- paste0("GS", back)

out <- NULL
for (i in 1:length(front)){
  gscat <- get(paste0(front[i], "_GSCAT"))
  gsinf <- get(paste0(front[i], "_GSINF"))

  temp <- dplyr::left_join(gscat[gscat$SPEC==4304,], gsinf)
  if(dim(temp)[1] >0)  {
    temp$run <- front[i]
    out <- rbind(out, temp)
  }
}

require(lubridate)
out$year <- year(ymd(out$SDATE))

require(sf)
out <- st_as_sf(out[!is.na(out$SLONG),], coords=c("SLONG", "SLAT"), crs=4326)
out$year <- as.factor(out$year)
out <- out %>% dplyr::group_by(year, run, geometry) %>%
  dplyr::summarize(number = sum(TOTNO))
sum(out$number)
unique(out$year)

require(ggplot2)
require(mapview)
mapview(out, zcol="year", cex="number")
ggplot() + geom_sf(data=out) + facet_wrap(~year)



