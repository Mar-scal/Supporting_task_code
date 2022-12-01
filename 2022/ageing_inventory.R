require(ROracle)
chan <-dbConnect(dbDriver("Oracle"),username="SCALOFF", password="fgb256k","ptran")

#####################################################################################################################
# Jessica has new views for these calls, ,all this prorating is not necessary anymore as she's taken care of it in SQL
# Key is to import those tables and send it out of this file looking identical!  
######################################################################################################################
db <- "SCALOFF" ### CHANGE HUMF TO SCALOFF!!!
#message("reminder that this is pulling data from HUMF views, not production SCALOFF")

#qu.strata <- "select * from SCALOFF.OSSTRATA"
# DK Oct 29, 2015, don't need tow data either, we don't ever use it.... 
qu.age <- paste0("Select * from SCALOFF.OSSAMPLES_SS_VW")
#qu.tow <- "select * from HUMF.OSTOWS"



# Grab the SQL data from the respective database tables
#strata <- sqlQuery(chan, qu.strata)
# Revised to be ROracle query
age <- dbGetQuery(chan, qu.age)
#tow <- sqlQuery(chan, qu.tow)
dbDisconnect(chan)


dim(age[!is.null(age$AGE) & !is.na(age$AGE) & age$AGE>0,])
table(age[!is.null(age$AGE) & !is.na(age$AGE) & age$AGE>0,]$MGT_AREA_CD)


inventory2010 <- read.xlsx("Y:/GrowthAging/Shell Inventory Working.xlsx", "2010")
inventory2003 <- read.xlsx("Y:/GrowthAging/Shell Inventory Working.xlsx", "2003")
ages <- read.xlsx("Y:/Offshore/Assessment/Data/Ageing/OS.scallop.age.all.v8.xls", 1)
oldages <- read.csv("Y:/Offshore/Assessment/Data/Ageing/BBnOldIncrementAge.csv")

te10 <- ages[ages$Survey.ID=="TE10",]
inventory2010_10 <- inventory2010[inventory2010$CRUISE=="TE10",]
unique(inventory2010_10$TOWNO[inventory2010_10$TOWNO %in% te10$Tow])
unique(te10$Tow[te10$Tow %in% inventory2010_10$TOWNO])

te08 <- ages[ages$Survey.ID=="TE08",]
inventory2010_08 <- inventory2010[inventory2010$CRUISE=="TE08",]
unique(inventory2010_08$TOWNO[inventory2010_08$TOWNO %in% te08$Tow])
unique(te08$Tow[te08$Tow %in% inventory2010_08$TOWNO])

ck20 <- oldages[oldages$cruise=="CK20",]
inventory2003_20 <- inventory2003[inventory2003$CRUISE=="CK20",]
unique(inventory2003_20$TOWNO[inventory2003_20$TOWNO %in% ck20$tow])
unique(ck20$tow[ck20$tow %in% inventory2003_20$TOWNO])

ck21 <- oldages[oldages$cruise=="CK21",]
inventory2003_21 <- inventory2003[inventory2003$CRUISE=="CK21",]
unique(inventory2003_21$TOWNO[inventory2003_21$TOWNO %in% ck21$tow])
unique(ck21$tow[ck21$tow %in% inventory2003_21$TOWNO])


#compare 2003 to database ages
names(age)
names(oldages)[1:3] <- c("CRUISE", "YEAR", "TOW_NO")
require(dplyr)

age$YEAR <- as.numeric(age$YEAR)

ages <- left_join(age, oldages)

table(ages[!is.na(ages$Scall.no),]$MGT_AREA_CD)
table(ages[is.na(ages$Scall.no),]$MGT_AREA_CD)


names(inventory2003)[names(inventory2003) %in% "TOWNO"] <- c("TOW_NO")
inventory2003$TOW_NO <- as.numeric(inventory2003$TOW_NO)
inventory2003$inventory <- "yes"
joined <- left_join(age[age$YEAR==2003,], inventory2003)

table(joined[joined$inventory=="yes",]$MGT_AREA_CD, joined[joined$inventory=="yes",]$CRUISE)



head(joined[joined$inventory=="yes" & !is.na(joined$AGE),])
length(unique(joined$TOW_NO[joined$inventory=="yes"& !is.na(joined$AGE)]))
joined[joined$inventory=="yes"& !is.na(joined$AGE),]
# 10 tows from database are still in inventory?
