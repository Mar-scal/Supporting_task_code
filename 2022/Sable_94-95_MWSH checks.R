# Sable 1994/1995 MWSH sampling discrepancy

# save(x=comm_hist21, file="C:/Users/keyserf/Documents/temp_data/comm_hist21.Rdata")
# save(x=comm, file="C:/Users/keyserf/Documents/temp_data/comm.Rdata")
# save(x=db, file="C:/Users/keyserf/Documents/temp_data/db.Rdata")

load("C:/Users/keyserf/Documents/temp_data/comm_hist21.Rdata")
load("C:/Users/keyserf/Documents/temp_data/comm.Rdata")
load("C:/Users/keyserf/Documents/temp_data/db.Rdata")

weights82 <- read.csv("Y:/Offshore/Assessment/Data/Hydration/Weights_82-92.csv")
weights92 <- read.csv("Y:/Offshore/Assessment/Data/Hydration/Weights_92-08.csv")

unique(weights92$Bank)
unique(weights92[weights92$Bank=="Sable" & weights92$Year==1994,c("Cruise", "Stn")])
unique(weights92[weights92$Bank=="Sable" & weights92$Year==1995,c("Cruise", "Stn")])
table(weights92[weights92$Bank=="Sable" & weights92$Year==1995,]$Month)

# "comm_hist21" contains commercial and pre-2001 samples on Sable, originating from:
# Y:\Offshore\Assessment\Data\Hydration\Weights_82-92.csv and
# Y:\Offshore\Assessment\Data\Hydration\Weights_92-08.csv
# this is what we used for mwsh data before the database update

# "comm" contains Sable commercial samples from the above CSV files (anything that was not uploaded to SCALOFF)
# "db" contains Sable survey samples, for all years (from current SCALOFF)

length(unique(comm_hist21[comm_hist21$year %in% 1994:1995,]$ID))
length(unique(comm[comm$year %in% 1994:1995,]$ID)) + length(unique(db[db$year %in% 1994:1995,]$ID))
# it's odd that these don't match. There are 8 tows of data in the db that were never used before

# see how 1996 has the same number of tows before and after?
length(unique(comm_hist21[comm_hist21$year %in% 1996,]$ID))
length(unique(comm[comm$year %in% 1996,]$ID)) + length(unique(db[db$year %in% 1996,]$ID))

# 1997 doesn't either
length(unique(comm_hist21[comm_hist21$year %in% 1997,]$ID))
length(unique(comm[comm$year %in% 1997,]$ID)) + length(unique(db[db$year %in% 1997,]$ID))

head(comm_hist21[comm_hist21$ID=="4305.0" & comm_hist21$year==1997,])
head(comm_hist21[comm_hist21$ID=="5000.0" & comm_hist21$year==1997,])
# OK that's because we re-labelled the IDs to be 1997.0 instead of 4305.0 and 5000.0. So this is actually fine!

# back to the 94/95 issue at hand though...

# "before": commercial and historical samples on sable in 94/95, summarized by tow
comm_hist21[comm_hist21$year %in% 1994:1995,] %>%
  mutate(UID = 1:nrow(.)) %>%
  group_by(cruise, tow, month, year) %>%
  summarize(meansh = mean(sh),
            meanwmw = mean(wmw), 
            numsamples = length(unique(UID))) %>%
  arrange(year, cruise, tow)

# "after part 1": commercial samples on sable in 94/95, summarized by tow
comm[comm$year %in% 1994:1995,] %>%
  mutate(UID = 1:nrow(.)) %>%
  group_by(cruise, tow, month, year) %>%
  summarize(meansh = mean(sh),
            meanwmw = mean(wmw),, 
            numsamples = length(unique(UID))) %>%
  arrange(year, cruise, tow)

# "after part 2": commercial samples on sable in 94/95, summarized by tow
db[db$year %in% 1994:1995,] %>%
  mutate(UID = 1:nrow(.)) %>%
  group_by(cruise, tow, month, year) %>%
  summarize(meansh = mean(sh),
            meanwmw = mean(wmw),, 
            numsamples = length(unique(UID))) %>%
  arrange(year, cruise, tow)


# each tow from comm_hist21 should have a match in comm OR db, and the mean SH and mean WMW should match as well. 
# But there are some oddballs where it seems like the tow numbering has changed because the mean SH/mean WMW values are found in another tow:
# e.g. CK04.37, P454.27, and more.  

