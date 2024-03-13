# adapted from 2020_07 request for the updated 2024_02 request

# GBa <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2023/Summer/GBa/Survey1984-2023.csv", stringsAsFactors = F)
# GBb <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2023/Summer/GBb/Survey1984-2023.csv", stringsAsFactors = F)
# GBspring <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2023/Spring/GB/Survey1984-2023.csv", stringsAsFactors = F)
# 
# GBsummer <- rbind(GBa, GBb)

load("Y:/Offshore/Assessment/Data/Survey_data/2023/Survey_summary_output/Survey_all_results.Rdata") 

GBsummer <- SurvDB$SHF[SurvDB$SHF$bank %in% c("GBa", "GBb"),]

bins <- names(GBsummer[13:52])

GBsummer$lon<-with(GBsummer,apply(cbind(elon,slon),1,mean))
GBsummer$lat<-with(GBsummer,apply(cbind(elat,slat),1,mean))

GBsummer <- GBsummer[GBsummer$state == "live",c("year", "tow", "cruise", "bank", "lat", "lon", "date", "depth", "state", bins)]

require(reshape2)
require(lubridate)
require(dplyr)
require(tidyverse)

GBsummer_long <- melt(GBsummer, id.vars = 1:9)
head(GBsummer_long)
summary(GBsummer_long$value)

GBsummer_long$variable <- gsub(x=GBsummer_long$variable, "h", "")
table(GBsummer_long$variable)

GBsummer_long$variable <- as.numeric(GBsummer_long$variable)

GBsummer_long <- GBsummer_long[GBsummer_long$variable>40,]

# somehow got some duplicated rows in here. get rid of 'em
GBsummer_long$ID <- paste0(GBsummer_long$cruise, ".", GBsummer_long$tow)

IDcheck <- as.data.frame(table(GBsummer_long$ID))
IDcheck[IDcheck$Freq>32,] # so it's only CK15 and CK17
unique(GBsummer_long[which(duplicated(GBsummer_long)),]$cruise)
GBsummer_long$depth <- round(GBsummer_long$depth, 0)

GBsummer_long <-  unique(GBsummer_long)
GBsummer_long <- dplyr::select(GBsummer_long, -ID)

GBsummer_bins <- GBsummer_long %>%
  dplyr::group_by(year, tow, cruise, lat, lon, date, depth, variable, value) %>%
  dplyr::summarize(sqm = value/(800*2.4384))

GBsummer_bins$date <- ymd_hms(GBsummer_bins$date)
GBsummer_bins$date <- floor_date(GBsummer_bins$date,unit="day")

GBsummer_bins <- GBsummer_bins %>%
  dplyr::rename(n=value)

GBsummer_bins$bin <- paste0("[", GBsummer_bins$variable-5, ",", GBsummer_bins$variable, ")")

GBsummer_bins <- GBsummer_bins[,c("lat", "lon", "date", "depth", "bin", "n", "sqm")]

# decided to provide these in long format after all so ignore this part...
# GBsummer_cast <- GBsummer_bins %>%
#   pivot_wider(names_from = bin, names_prefix = "sqm_", values_from = sqm) #lat + lon + date + depth ~ bin, value.var="sqm")
# GBsummer_cast2 <- GBsummer_bins %>%
#   pivot_wider(names_from = bin, names_prefix = "n_", values_from = n)
#
# GBsummer_bins <- join(GBsummer_cast, GBsummer_cast2, type="left")
# GBsummer_bins <- GBsummer_bins[,c("lat", "lon", "date", "depth", "density", "std.count", "density_bin_<75", "density_bin_>75", "stdcount_bin_<75", "stdcount_bin_>75")]

names(GBsummer_bins) <- c("latitude_dd.ddddd", "longitude_dd.ddddd", "date_yyyy-mm-dd", "depth_m", "bin_mm", "n", "density_sq_m")
GBsummer_bins$year <- year(GBsummer_bins$`date_yyyy-mm-dd`)
GBsummer_bins_78_97 <- GBsummer_bins[GBsummer_bins$year %in% 1978:1997,]
GBsummer_bins_13_23 <- GBsummer_bins[GBsummer_bins$year %in% 2013:2023,]
GBsummer_bins_78_97 <- dplyr::select(GBsummer_bins_78_97, -year)
GBsummer_bins_13_23 <- dplyr::select(GBsummer_bins_13_23, -year)

write.csv(GBsummer_bins_78_97, "Y:/Offshore/Data Requests/2024/DR2024_02_UMass/GBsummer_densities_DFOSurvey_5mm_1978-1997.csv")
write.csv(GBsummer_bins_13_23, "Y:/Offshore/Data Requests/2024/DR2024_02_UMass/GBsummer_densities_DFOSurvey_5mm_2013-2023.csv")
