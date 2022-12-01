source("C:/Users/keyserf/Documents/GitHub/Assessment_fns/Other_functions/ScallopQuery.R")

comm_amy <- ScallopQuery(package="ROracle", un=un.ID, pw=pwd.ID, db.con="ptran", 
                         SQLtext = "SELECT * FROM CHISHOLMA.COMM_SAMPLES_SCALOFF")
comm_amy_sub <- comm_amy %>%
  filter(year(dmy(comm_amy$TOW_DATE)) %in% 1982:2000)

comm_amy_sum <- comm_amy_sub %>%
  group_by(SURVEY_NAME, MGT_AREA_CD, TOW_DATE, TOW_NUM, VRN, LAT_DDMM_MM, LON_DDMM_MM) %>%
  summarize(numscal = length(unique(SCALLOP_NUM)), 
            meansh = mean(SHELL_HEIGHT),
            meanwmw = mean(WET_MEAT_WGT))

head(comm_amy_sub)

comm_amy_sum

windows()
ggplot() + geom_line(data=comm_amy_sum, aes(dmy(TOW_DATE), numscal)) + facet_wrap(~MGT_AREA_CD, scales="free_y") +
  ggtitle("Amy") 

windows()
ggplot() + geom_line(data=comm_amy_sum, aes(dmy(TOW_DATE), meansh)) + facet_wrap(~MGT_AREA_CD, scales="free_y") +
  ggtitle("Amy") 


MW.dat.sum <- MW.dat %>%
  mutate(date = ymd(paste0(year, "-", month, "-", day))) %>%
  group_by(cruise, bank, date, tow, lon, lat) %>%
  summarize(numscal = length(unique(scalnum)), 
            meansh = mean(sh),
            meanwmw = mean(wmw))

windows()
ggplot() + geom_line(data=MW.dat.sum[!MW.dat.sum$cruise %in% MW.dat.new$cruise,], aes(date, numscal)) + facet_wrap(~bank,scales="free_y") +
  ggtitle("old")

windows()
ggplot() + geom_line(data=MW.dat.sum[!MW.dat.sum$cruise %in% MW.dat.new$cruise,], aes(date, meansh)) + facet_wrap(~bank,scales="free_y") +
  ggtitle("old")

head(MW.dat.sum)
unique(MW.dat.sum$cruise)
unique(MW.dat.new$cruise)
                     