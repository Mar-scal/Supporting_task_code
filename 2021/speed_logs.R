direct_fns <- "C:/Users/keyserf/Documents/Github/FK/Assessment_fns/"

source(paste0(direct_fns, "Other_functions/ScallopQuery.R"))

year <- 2018

qu.log <- paste("select * from marfissci.P_OFFSHORE_SCALLOP_LOG_2008 where DATE_FISHED like '%-",year-2000,"'",sep="")

log <- ScallopQuery(package="ROracle", un="keyserf", pw="Decade06", db.con="ptran", SQLtext= qu.log)

names(log)

require(readxl)

xlfile <- read_xlsx(path="Y:/Projects/OffshoreLogDatawSpeed/Final_Feb062018/2017log_speed_complete.xlsx", sheet = 1, n_max = 3)

names(xlfile)

which(!names(log) %in% names(xlfile))
names(log)[24]
which(!names(xlfile) %in% names(log))
names(xlfile)[which(!names(xlfile) %in% names(log))]

require(dplyr)

full_join(log, xlfile)

summary(log)
summary(xlfile)
