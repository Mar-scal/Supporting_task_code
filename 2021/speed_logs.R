direct_fns <- "C:/Documents/Assessment_fns/"

source(paste0(direct_fns, "Other_functions/ScallopQuery.R"))

year <- 2018

qu.log <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",year-2000,"'",sep="")

log <- ScallopQuery(package="ROracle", un="keyserf", pw="", db.con="ptran", SQLtext= qu.log)

names(log)

require(readxl)

xlfile <- read_xlsx(path="Y:/Projects/OffshoreLogDatawSpeed/Final_Feb062018/2017log_speed_complete.xlsx", sheet = 1, n_max = 3)

which(!names(log) %in% names(xlfile))
names(log)[24]
which(!names(xlfile) %in% names(log))
names(xlfile)[which(!names(xlfile) %in% names(log))]

log$DOCUMENT_NO <- as.numeric(log$DOCUMENT_NO)
log$LICENCE_ID <- as.numeric(log$LICENCE_ID)
log$WATCH <- as.numeric(substr(log$WATCH, 1,1))
log$NO_RAKES_FISHED <- as.numeric(log$NO_RAKES_FISHED)
log$NO_TOWS_PER_WATCH <- as.numeric(log$NO_TOWS_PER_WATCH)
log$AVG_TOW_TIME <- as.numeric(log$AVG_TOW_TIME)
log$DEPTH_FM <- as.numeric(log$DEPTH_FM)
log$NO_OF_BAGS <- as.numeric(log$NO_OF_BAGS)

require(dplyr)
require(lubridate)
full <- full_join(log, xlfile)
full$YEAR <- year(ymd_hms(full$DATE_FISHED))
full$MONTH <- month(ymd_hms(full$DATE_FISHED))
full$DAY <- day(ymd_hms(full$DATE_FISHED))

full <- full[full$YEAR==2018,]

full <- full %>%
  select(c(names(xlfile), "COMMENTS"))

full$COMMENTS...24 <- full$COMMENTS

full <- select(full, -COMMENTS)

require(writexl)
write_xlsx(x=full, path = paste0("Y:/Offshore/Assessment/2021/Supporting_tasks/Speed_logs/", year, "log_speed_marfis.xlsx"))
