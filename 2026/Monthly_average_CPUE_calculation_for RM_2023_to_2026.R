# Quick script to compare 2025 and 2026 (up to April) catch rates
logs <- read.csv("Y:/Offshore/Assessment/Data/Fishery_data/Logs/Preliminary/2026log.csv")
logs.2025 <- read.csv("Y:/Offshore/Assessment/Data/Fishery_data/Logs/Preliminary/2025log.csv")
logs.2024 <- read.csv("Y:/Offshore/Assessment/Data/Fishery_data/Logs/QAQC_logs/2024log.csv")
logs.2023 <- read.csv("Y:/Offshore/Assessment/Data/Fishery_data/Logs/QAQC_logs/2023log.csv")

gba <- logs[logs$FISHING_AREA == "27A",]
gba$year <- 2026
gba$date <- lubridate::dmy(gba$DATE_FISHED)
gba$month <- lubridate::month(gba$date)
gba.25 <- logs.2025[logs.2025$FISHING_AREA == "27A",]
gba.25$year <- 2025
gba.25$date <- lubridate::dmy(gba.25$DATE_FISHED)
gba.25$month <- lubridate::month(gba.25$date)

gba.24 <- logs.2024[logs.2024$FISHING_AREA == "27A",]
gba.24$year <- 2024
gba.24$date <- lubridate::dmy(gba.24$DATE_FISHED)
gba.24$month <- lubridate::month(gba.24$date)

gba.23 <- logs.2023[logs.2023$FISHING_AREA == "27A",]
gba.23$year <- 2023
gba.23$date <- lubridate::dmy(gba.23$DATE_FISHED)
gba.23$month <- lubridate::month(gba.23$date)

# combine them
combo <- rbind(gba,gba.25,gba.24,gba.23)
# Adjust for the gear size on the two vessels with smaller gear
combo$gear.size <- 15
combo$gear.size[combo$VR_NUMBER == 152320] <- 14.5
combo$gear.size[combo$VR_NUMBER == 106881] <- 12

# calculate CPUE
combo$h <- combo$NO_TOWS_PER_WATCH*combo$AVG_TOW_TIME/60
combo$m <- combo$NO_RAKES_FISHED * combo$gear.size * 0.3048
combo$hm <- combo$h*combo$m
combo$kg.hm <- combo$PRORATED_RPTD_WEIGHT_KGS / combo$hm

gba.2025 <- combo[combo$year == 2025,]
gba.2026 <- combo[combo$year == 2026,]
# Are these very different from the mean?
kg.hm.25 <- sum(gba.2025$PRORATED_RPTD_WEIGHT_KGS)/sum(gba.2025$hm)
kg.hm.26 <- sum(gba.2026$PRORATED_RPTD_WEIGHT_KGS)/sum(gba.2026$hm)

# Make a plot of CPUE by month
month.cpue <- combo |> group_by(month,year) |> summarize(mn = mean(kg.hm))
# Checking the numbers are close to the OSAC numbers
mean(gba.2025$kg.hm[gba.2025$month < 11])

# Plot
ggplot(month.cpue) + geom_line(aes(month,mn,group=as.factor(year),color=as.factor(year)),size=1.5) + 
                    scale_x_continuous(name = "month",breaks = 1:12) + 
                    scale_y_continuous(name= "Average catch rate (kg/hm)") + theme(legend.title = element_blank())


