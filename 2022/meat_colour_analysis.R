source("./Survey_and_OSAC/get.offshore.survey.r")
source("./Maps/convert_coords.R")
source("./Maps/pectinid_projector_sf.R")

require(lubridate)
require(ggplot2)
require(ROracle)

### read in OSSURVEYS, OSTOWS and OSHFREQ_SAMPLES
chan <-dbConnect(dbDriver("Oracle"),username=un.id, password=pwd.id,"ptran")
qu.meats <- paste0("select to_char(tow_date,'yyyy') year, cruise, survey_name, area_cd, t.tow_no, 
		mgt_area_cd, strata_id, species_id, tow_date, depth_f, start_lat, start_lon, end_lat, end_lon, 
		scallop_num, shell_height, sex_id, maturity_id, infected, sampler_id, age, ager_id, 
		wet_meat_wgt, wet_gonad_wgt, dry_meat_wgt, dry_gonad_wgt, wet_soft_parts_wgt, meat_colour_id
	from ossurveys s, ostows t, ossamples s
	where s.survey_seq = t.survey_seq
	and t.tow_seq = s.tow_seq
	and species_id = 1 /*sea scallop*/
")
qu.meats <- dbGetQuery(chan, qu.meats)
dbDisconnect(chan)

chan <-dbConnect(dbDriver("Oracle"),username=un.id, password=pwd.id,"ptran")
qu.meats.in <- paste0("SELECT * from SCALLSUR.SCWGTHGT where cruise in ('BF2018','BF2019','BF2021','BI2018','BI2019','BI2021','GM2018','GM2019','GM2021','SFA292018','SFA292019','SFA292021')")
qu.meats.in <- dbGetQuery(chan, qu.meats.in)
dbDisconnect(chan)

qu.meats.in$YEAR <- year(ymd_hms(qu.meats.in$TOW_DATE))

table(qu.meats$YEAR, qu.meats$MEAT_COLOUR_ID)
table(qu.meats$SURVEY, qu.meats$MEAT_COLOUR_ID)
table(qu.meats$SAMPLER_ID, qu.meats$MEAT_COLOUR_ID)

table(qu.meats[qu.meats$YEAR>2018,]$CRUISE, qu.meats[qu.meats$YEAR>2018,]$SAMPLER_ID)
qu.meats[qu.meats$YEAR==2021,]$MEAT_COLOUR_ID
table(qu.meats.in$YEAR, qu.meats.in$MEAT_COLOUR)

qu.meats$START_LAT_dec <- convert.dd.dddd(x=qu.meats$START_LAT, "dec.deg")
qu.meats$START_LON_dec <- convert.dd.dddd(x=qu.meats$START_LON, "dec.deg")
qu.meats.in$START_LAT_dec <- convert.dd.dddd(x=qu.meats.in$START_LAT, "dec.deg")
qu.meats.in$START_LON_dec <- convert.dd.dddd(x=qu.meats.in$START_LONG, "dec.deg")

meat.sum <- qu.meats[qu.meats$YEAR>2018,] %>%
  group_by(SAMPLER_ID, MEAT_COLOUR_ID, YEAR) %>%
  summarize(count=n())

ggplot() + geom_bar(data=qu.meats[qu.meats$YEAR>2018,], aes(fill=as.factor(SAMPLER_ID), as.factor(MEAT_COLOUR_ID)), position="dodge") +facet_wrap(~YEAR) +
  theme_bw() +
  geom_point(data=meat.sum, aes(x=as.factor(MEAT_COLOUR_ID), count, group=as.factor(SAMPLER_ID)), position=position_dodge(0.9))

meat.sum2 <- qu.meats[qu.meats$YEAR==2019,] %>%
  group_by(SAMPLER_ID, MEAT_COLOUR_ID, YEAR, SURVEY_NAME) %>%
  summarize(count=n())

ggplot() + geom_bar(data=qu.meats[qu.meats$YEAR==2019,], aes(fill=as.factor(SAMPLER_ID), as.factor(MEAT_COLOUR_ID)), position="dodge") +facet_wrap(~SURVEY_NAME) + theme_bw()+
  geom_point(data=meat.sum2, aes(x=as.factor(MEAT_COLOUR_ID), count, group=as.factor(SAMPLER_ID)), position=position_dodge(0.9))

qu.meats.myco <- qu.meats
qu.meats <- qu.meats[qu.meats$SAMPLER_ID==6,]


qu.meats.in$MEAT_COLOUR_ID[qu.meats.in$MEAT_COLOUR=="Normal white colour"] <- 0
qu.meats.in$MEAT_COLOUR_ID[qu.meats.in$MEAT_COLOUR=="moderate (light brown/gray)"] <- 1
qu.meats.in$MEAT_COLOUR_ID[qu.meats.in$MEAT_COLOUR=="severe (dark brown/gray)"] <- 2

p <- pecjector(area="NL")
p + geom_point(data=qu.meats[qu.meats$YEAR>2018 & qu.meats$MEAT_COLOUR_ID==0,], aes(START_LON_dec, START_LAT_dec), colour="black") +
  geom_point(data=qu.meats[qu.meats$YEAR>2018 & qu.meats$MEAT_COLOUR_ID %in% c(1,2),], aes(START_LON_dec, START_LAT_dec), colour="red") +
  theme_bw() +
  geom_point(data=qu.meats.in[qu.meats.in$MEAT_COLOUR_ID==0,], aes(START_LON_dec, START_LAT_dec), colour="black") +
  geom_point(data=qu.meats.in[!qu.meats.in$MEAT_COLOUR_ID==0,], aes(START_LON_dec, START_LAT_dec), colour="red") +
  scale_x_continuous(limits=c(-67.5, -58)) +
  scale_y_continuous(limits=c(41, 47))

qu.meats$unid <- paste0(qu.meats$SURVEY_NAME, ".", qu.meats$TOW_NO, ".", qu.meats$SCALLOP_NUM)
ggplot() + geom_point(data=qu.meats[qu.meats$YEAR==2019,], aes(unid, MEAT_COLOUR_ID))




p + geom_point(data=qu.meats.in[qu.meats.in$MYCO_INFECTED=="N",], aes(START_LON_dec, START_LAT_dec), colour="black") +
  geom_point(data=qu.meats.in[qu.meats.in$MYCO_INFECTED=="Y",], aes(START_LON_dec, START_LAT_dec), colour="red") +
  scale_x_continuous(limits=c(-67.5, -58)) +
  scale_y_continuous(limits=c(42.9, 47)) +
  theme_bw()

