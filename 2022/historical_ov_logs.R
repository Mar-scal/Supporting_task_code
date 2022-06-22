# looking at log files over time. Were they accidentally cropped? Why are olex tows consistently longer? 

folders <- c(
  "Y:/Offshore/Assessment/Data/Survey_data/2022/Database loading/LE15/",
  "Y:/Offshore/Assessment/Data/Survey_data/2021/Database loading/",
  "Y:/Offshore/Assessment/Data/Survey_data/2020/Database loading/",
  "Y:/Offshore/Assessment/Data/Survey_data/2019/Database loading/",
  "Y:/People/Alan/LE08/Logfiles/",
  "Y:/Offshore/Assessment/Data/Tow_tracks/",
  "Y:/People/Alan/TE16/log files/",
  "Y:/People/Alan/TE15/log files/",
  "Y:/People/Alan/TE14/",
  "Y:/People/Alan/TE13/DATA/Prelim/log files/",
  "Y:/People/Alan/TE12/log files/",
  "Y:/People/Alan/TE11/logfiles/",
  "Y:/Offshore/Survey/Cruises/TE10-GB10/Alan/Tow Tracks/",
  "Y:/Offshore/Survey/Cruises/TE07-GB09/TE07Tows/",
  "Y:/Offshore/Survey/Cruises/TE04-SS09/Te04ss09tows/",
  "Y:/Offshore/Survey/Cruises/TE02-GB08/TE02tows/",
  "Y:/Offshore/Survey/Cruises/EE05-SS08/",
  "Y:/Offshore/Survey/Cruises/EE03/EE03tows/",
  "Y:/Offshore/Survey/Cruises/EE01/"
)

rows <- NULL
for(f in rev(folders)){
  logs <- list.files(f, recursive = T)
  logs <- logs[sort(c(grep(x=logs, ".log"), grep(x=logs, ".LOG")))]
  if(any(grepl(x=logs, ".FRK"))) logs <- logs[-sort(c(grep(x=logs, ".FRK")))]
  if(any(grepl(x=logs, "archive"))) logs <- logs[-sort(c(grep(x=logs, "archive")))]
  if(any(grepl(x=logs, "validation"))) logs <- logs[-sort(c(grep(x=logs, "validation")))]
  if(any(grepl(x=logs, ".ovd"))) logs <- logs[-sort(c(grep(x=logs, ".ovd")))]
  if(any(grepl(x=logs, "csv"))) logs <- logs[-sort(c(grep(x=logs, ".csv")))]
  for(i in logs){
    tryCatch({
      #if(!grepl(x=f, pattern="TE1")) log <- read.table(paste0(f, i), sep=",")
      #if(grepl(x=f, pattern="TE1")) 
      log <- read.table(paste0(f, i), skip=which(grepl(x=readLines(paste0(f, i), n=10), "-6") | grepl(x=readLines(paste0(f, i), n=10), "-5"))[1] - 1, sep=",")
      log <- data.frame(folder=f, log=i, rows=nrow(log), header=log[1,1])
      rows <- rbind(rows, log)
    },
    error = function(e) print(paste0(f, i, " failed")))
  }
  print(ggplot() + geom_point(data=rows, aes(log, rows)) + ggtitle(f))
  print(f)
  print(summary(rows$header))
  print(summary(rows$rows))
  print(dim(rows))
}

rows$folder <- as.factor(rows$folder)
rows$folder <- factor(rows$folder, levels=c(folders))

rows_sum <- rows %>%
  group_by(folder) %>%
  summarize(nfiles = n())
rows_bin <- rows %>%
  group_by(folder, rows) %>%
  summarize(nbin=n())

rows_bin <- left_join(rows_bin, rows_sum)

ggplot() + geom_point(data=rows, aes(log, rows)) + facet_wrap(~folder, scales="free_x") + ylim(125,250)
png("Y:/Offshore/Assessment/2022/Supporting_tasks/OV_rows.png", height=11, width=8.5, units='in', res=420)
ggplot() + geom_bar(data=rows_bin, aes(y=nbin/nfiles, x=rows), stat="identity") + 
  facet_wrap(~folder, ncol=1, strip.position="right") +
  xlim(min(rows_bin$rows), max(rows_bin$rows)) +
  theme(strip.text.y.right=element_text(angle=360))
dev.off()