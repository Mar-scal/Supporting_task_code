# ancient PS data
require(dplyr)
require(tidyr)
years <- list.files("Y:/Offshore/Assessment/Data/Archive/PortSampling/")
years <- 1981:2005
paths <- NULL
for(i in years){
  files <- list.files(paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i))
  if(length(files)==1) dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files)
  if(length(files)>1){
    if(any(grepl(pattern="reg.", x=files, fixed=T))) {
      dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files[grep(pattern="reg.", x=files, fixed=T)])
    }
    if(!any(grepl(pattern="reg.", x=files, fixed=T)) & any(grepl(pattern="all.", x=files, fixed = T))) {
      dat <- paste0("Y:/Offshore/Assessment/Data/Archive/PortSampling/", i, "/", files[grep(pattern="all.", fixed=T, x=files)])
    }
    if(any(grepl(pattern="FT", x=dat))) {
      dat <- dat[-grep(pattern="FT", x=dat)]
    }
    if(any(grepl(pattern="WF", x=dat))) {
      dat <- dat[-grep(pattern="WF", x=dat)]
    }
  }
  print(i)
  print(dat)
  paths <- c(paths, dat)
}

ps <- NULL
for(i in 1:length(paths)){
  ps[[i]] <- read.csv(paths[i], header=F, sep="\t")
  if(ncol(ps[[i]])==1) ps[[i]] <- read.csv(paths[i], header=F, sep=" ")
}
save(ps, file="C:/Users/keyserf/Documents/temp_data/old_ps.RData")
load("C:/Users/keyserf/Documents/temp_data/old_ps.RData")
#fails: old.ps <- do.call("rbind",ps)

for(i in 1:length(ps)){
  ps[[i]][is.na(ps[[i]])] <- 0
  nas <- colSums(ps[[i]])
  ps[[i]] <- ps[[i]][which(nas>0)]
  names(ps[[i]])[1:5] <- c("date","boat","port","fished", "id")
  ps[[i]] <- ps[[i]] %>%
    pivot_longer(
      cols = starts_with("V"),
      names_to = "scalnum",
      names_prefix = "V",
      values_to = "wmw",
      values_drop_na = TRUE
    )
  ps[[i]]$scalnum <- as.numeric(ps[[i]]$scalnum)-5
  ps[[i]] <- ps[[i]][ps[[i]]$wmw>0,]
  # print(i)
  ps[[i]]$date2 <- dmy(ps[[i]]$date)
  #print(any(is.na(ps[[i]]$date2)))
  if(all(is.na(ps[[i]]$date2)) | all(year(ps[[i]]$date2) < 1981)) ps[[i]]$date2 <- ymd(ps[[i]]$date)
  # ps[[i]][is.na(ps[[i]]$date2),]
  #print(any(is.na(ps[[i]]$date2)))
}

ps <- do.call("rbind", ps)

unique(ps[is.na(ps$date2),]$date)
# records on June 36 1989 and April 31 2002 need to be fixed
ps[(which(ps$date == 360689)[1]-5):(which(ps$date == 360689)[1]),]
table(ps[ps$date==260689,]$id)
hist(ps[ps$date==260689,]$id)
ps[ps$date==260689 & ps$id==111,]$wmw %in% ps[ps$date==360689 & ps$id==111,]$wmw
ps[ps$date==360689 & ps$id==111,]$wmw %in% ps[ps$date==260689 & ps$id==111,]$wmw
ps[ps$date==360689,]$date <- 260689
ps[ps$date==260689,]$date2 <- dmy(ps[ps$date==260689,]$date)
# changed June 36 1989 to June 26 1989

ps[(which(ps$date == 20020431)[1]-5):(max(which(ps$date == 20020431))),]
tail(ps[(which(ps$date == 20020431)[1]-5):(max(which(ps$date == 20020431)))+1,])
table(ps[ps$date==20020431,]$id)
table(ps[ps$date==20020420,]$id)
table(ps[ps$date==20020430,]$id)
hist(ps[ps$date==20020420,]$id)
ps[ps$date==20020420 & ps$id==111,]$wmw %in% ps[ps$date==20020431 & ps$id==111,]$wmw
ps[ps$date==20020431 & ps$id==111,]$wmw %in% ps[ps$date==20020420 & ps$id==111,]$wmw
ps[ps$date==20020431,]$date <- 20020420
ps[ps$date==20020420,]$date2 <- dmy(ps[ps$date==20020420,]$date)
# changed Apr 31 2022 to Apr 20 2002

unique(ps$fished)
unique(ps$boat)
unique(ps$port)
unique(ps$id)

unique(year(ps[ps$id<100,]$date2))

ps$fished <- ps$date2
ps$date2 <- NA

head(ps)
old_ps <- ps
# write.csv(old_ps, file = "Y:/Offshore/Assessment/Data/PortSampling/PS_data_reorg_4_analysis/Old_PS_data_clean_1981-2005.csv")
write.csv(old_ps, file = "C:/Users/keyserf/Documents/temp_data/Old_PS_data_clean_1981-2005.csv")
save(old_ps, file = "Y:/Offshore/Assessment/Data/PortSampling/PS_data_reorg_4_analysis/Old_PS_data_clean_1981-2005.Rdata")
