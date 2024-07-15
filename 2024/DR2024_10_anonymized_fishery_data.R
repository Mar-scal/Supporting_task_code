# DR2023_09 Anonymized fishery data for PhD use

## RUNME log and slip data compiling
# set your directory
direct <- "Y:/Offshore/Assessment/"
# read the function from github
funs <- c("https://raw.githubusercontent.com/Mar-scal/Assessment_fns/master/Fishery/logs_and_fishery_data.r")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}

# to export csv (adjust years to whatever you want):
logs_and_fish(loc="offshore", year=1980:2023, get.marfis = F, export = F, direct = direct)

key <- data.frame(ves=unique(new.log.dat$ves), ves.anon=1:length(unique(new.log.dat$ves)))
key.vrnum <- data.frame(vrnum=unique(new.log.dat$vrnum), vrnum.anon=1:length(unique(new.log.dat$vrnum))*100000)
key.vesid <- data.frame(vesid=unique(old.log.dat$vesid), vesid.anon=1:length(unique(old.log.dat$vesid))*1000)

new.keep <- new.log.dat
old.keep <- old.log.dat

require(dplyr)
new.log.dat <- left_join(new.log.dat, key)
new.log.dat <- left_join(new.log.dat, key.vrnum)
new.log.dat <- select(new.log.dat, -ves, -vrnum, -trip.id) %>%
  filter(bank %in% c("GBa", "GBb"))

old.log.dat <- left_join(old.log.dat, key.vesid) %>% select(-vesid, -trip.id) %>%
  filter(bank %in% c("GBa", "GBb"))

write.csv(new.log.dat, "Y:/Offshore/Data requests/2024/DR2024_10_RaphPhD_NonCA/newlogdat.csv")
write.csv(old.log.dat, "Y:/Offshore/Data requests/2024/DR2024_10_RaphPhD_NonCA/oldlogdat.csv")
write.csv(data.frame(column = unique(c(names(new.log.dat), names(old.log.dat)))), "Y:/Offshore/Data requests/2024/DR2024_10_RaphPhD_NonCA/logkey.csv")