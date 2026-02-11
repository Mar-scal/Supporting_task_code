require(sf)

funs <- c("https://raw.githubusercontent.com/mar-scal/Assessment_fns/master/Maps/pectinid_projector_sf.R")
# Now run through a quick loop to load each one, just be sure that your working directory is read/write!
for(fun in funs) 
{
  download.file(fun,destfile = basename(fun))
  source(paste0(getwd(),"/",basename(fun)))
  file.remove(paste0(getwd(),"/",basename(fun)))
}


mpa <- st_read("Y:/GISdata/Private/networksites_proposed_OEM_MPA_20221128/networksites_proposed_OEM_MPA_20221128.shp")
plot(mpa)
head(mpa)
unique(fc$NAME)
fc <- mpa[mpa$NAME =="Fundian Channel-Browns Bank",]
plot(fc)


load("Y:/Offshore/Assessment/Data/Survey_data/2022/Survey_summary_output/Survey_all_results.RData")

tows <- st_as_sf(all.surv.dat[all.surv.dat$state=="live",], coords=c(X="slon", y="slat"), crs=4326) %>%
  st_transform(st_crs(fc))

tows <- st_intersection(tows, fc)

by_year <- tows %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(ntows = length(unique(paste0(cruise,".",tow))))

years <- data.frame(year=min(by_year$year): max(by_year$year))

by_year <- dplyr::left_join(years, by_year)
total <- sum(by_year$ntows[by_year$year %in% 2013:2022], na.rm=T)


require(ggplot2)

lims <- fc %>% 
  st_combine() %>% 
  st_convex_hull() %>% 
  st_buffer(dist=10000) %>% 
  st_bbox()

bp <- pecjector(area=list(y = c(lims$ymin[[1]], lims$ymax[[1]]),
                          x =c(lims$xmin[[1]], lims$xmax[[1]]),
                          crs = st_crs(fc)),
                repo = 'github',c_sys = st_crs(fc), add_layer = list(bathy = c(50,'c')),plot=F, quiet=T)# + 

png("Y:/Offshore/Assessment/2023/Supporting_tasks/FundianChannel/FC-BB survey tow maps_facet.png", height=12, width=12, units="in", res=420)
bp + geom_sf(data=fc) + geom_sf(data=tows[tows$year %in% 2013:2022,]) + facet_wrap(~year) +
  theme_bw() + 
  ggtitle("Scallop survey tows within proposed FC-BB boundary (2013-2022)") +
  geom_text(data=by_year[!is.na(by_year$ntows) & by_year$year %in% 2013:2022,], 
            aes(x=lims$xmin[[1]], y=lims$ymin[[1]], label=paste0("N tows = ", ntows)), 
            nudge_x=60000, nudge_y=50000) +
  coord_sf(expand=F)
dev.off()
  
png("Y:/Offshore/Assessment/2023/Supporting_tasks/FundianChannel/FC-BB survey tow maps_total.png", height=6, width=7, units="in", res=420)
bp + geom_sf(data=fc) + geom_sf(data=tows[tows$year %in% 2013:2022,]) + 
  theme_bw() + coord_sf() +
  ggtitle("Scallop survey tows within proposed FC-BB boundary (2013-2022)") + 
  annotate(geom="text", x=lims$xmin[[1]]+50000, y=lims$ymin[[1]]+50000, label=paste0("Total number of tows = ", total)) + 
  coord_sf(expand=F)
dev.off()

png("Y:/Offshore/Assessment/2023/Supporting_tasks/FundianChannel/FC-BB survey tow_ts.png", height=6, width=6, units="in", res=420)
ggplot() + geom_line(data=by_year[by_year$year %in% 2013:2022,], aes(year, ntows)) +
  geom_point(data=by_year[by_year$year %in% 2013:2022,], aes(year, ntows)) +
  ylab("Number of scallop survey tows within proposed FC-BB boundary") +
  xlab("year") +
  theme_bw() + 
  scale_x_continuous(breaks=seq(2013,2022, 2))
dev.off()

write.csv(by_year[,c("year", "ntows")], "Y:/Offshore/Assessment/2023/Supporting_tasks/FundianChannel/FC-BB survey tow summary.csv")

towtable <- all.surv.dat[all.surv.dat$year %in% 2013:2022 & all.surv.dat$state=="live" & all.surv.dat$bank %in% c("BBs", "BBn"), c("year", "cruise", "bank", "date", "tow", "stratum", "slat", "slon", "elat", "elon", "depth")]
towtable <- dplyr::arrange(towtable, year, cruise, bank, tow)
write.csv(towtable, "Y:/Offshore/Assessment/2023/Supporting_tasks/FundianChannel/offshore_survey_tows_Browns_2013-2022.csv")

