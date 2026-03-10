
load("Y:/Offshore/Assessment/Data/Fishery_data/Summary/2025/OSAC_tidy_logs.RData")
#logs_and_fish(loc="offshore", get.local=T, direct="Y:/Offshore/Assessment/", year = 1981:2022)
#fish.dat<-merge(new.log.dat,old.log.dat,all=T)
fish.dat$ID<-1:nrow(fish.dat)
fish.dat <- fish.dat[!is.na(fish.dat$lon),]
fish.dat <- fish.dat[!is.na(fish.dat$lat),]
fish.dat <- fish.dat[!fish.dat$lon==0,]
fish.dat <- fish.dat[!fish.dat$lat==0,]
fish_sf <- st_as_sf(fish.dat, coords=c(X="lon", Y="lat"), crs=4326)

offshore <- github_spatial_import("offshore", "offshore.zip", quiet=T)
#ggplot() + geom_sf(data=offshore) + facet_wrap(~ID)

sf_use_s2(FALSE)
fish_sf <- st_intersection(fish_sf, offshore)
sf_use_s2(TRUE)
unique(fish_sf$ID.1)
unique(fish_sf$bank)
unique(fish_sf$sfa)
fish_sf$ID.1 <- gsub(x=fish_sf$ID.1, pattern="SFA", replacement="")

# if shp ID doesn't match log sfa:
dim(fish_sf[!fish_sf$ID.1==fish_sf$sfa & !is.na(fish_sf$sfa),])
# if shp ID matches log sfa:
dim(fish_sf[fish_sf$ID.1==fish_sf$sfa,])
unique(fish_sf$sfa)
unique(fish_sf$ID.1)

# look at the "bad" matches
check <- fish_sf[!fish_sf$ID.1==fish_sf$sfa & !is.na(fish_sf$sfa),]
ggplot() + geom_point(data=check, aes(sfa, ID.1))
#unique(fish_sf[fish_sf$bank=="Ban",]$ID.1)
#unique(fish_sf[fish_sf$bank=="Sab",]$ID.1)
table(check$sfa, check$ID.1) # relatively few misattributed records. Use intersected results

fish_sf$sfa <- fish_sf$ID.1

fishery <- fish_sf %>%
  dplyr::filter(!bank %in% c("GBa", "SPB")) %>%
  dplyr::filter(!is.na(bank))

cpue <- NULL
for(bank in unique(fishery$bank)){
  sub <- fish_sf[fish_sf$bank==bank & fish_sf$datclass==1,]
  jack <- jackknife(data = data.frame(year=sub$year, catch=sub$pro.repwt, effort=sub$hm))
  jack$bank <- bank
  cpue <- rbind(cpue, jack)
}

fishery <- fishery %>%
  dplyr::group_by(bank, year) %>%
  dplyr::summarize(catch_raw = sum(pro.repwt, na.rm=T),
                   effort_raw=sum(hm, na.rm=T))

fishery <- full_join(fishery, cpue)

#unique(fishery[!fishery$catch_raw==fishery$catch,]$year) # all earlier than 2009, good
# fixing the calculations of effort based on cpue pre-2009
fishery$effort[!fishery$catch_raw==fishery$catch & !is.na(fishery$catch)] <- fishery$catch_raw[!fishery$catch_raw==fishery$catch & !is.na(fishery$catch)] / fishery$cpue[!fishery$catch_raw==fishery$catch & !is.na(fishery$catch)]
fishery$catch <- fishery$catch_raw

fishery <- dplyr::select(fishery, -catch_raw, -effort_raw)

years <- expand.grid(year=1980:2022, bank=unique(fishery$bank))
fishery <- left_join(years, as.data.frame(fishery))

fishery$sfa[fishery$bank %in% c("Ban", "Sab", "Mid")] <- 25
fishery$sfa[fishery$bank %in% c("BBn", "BBs", "Ger", "BB")] <- 26
fishery$sfa[fishery$bank %in% c("GBb")] <- 27

fishery <- fishery[fishery$sfa %in% 25:27,]

if(french==F) cpuelab <- expression(paste("Catch per unit effort  ",bgroup("(",frac(kg,hm)   ,")")))
if(french==T) cpuelab <- expression(paste("Captures par unité d’effort  ",bgroup("(",frac(kg,hm)   ,")")))
if(french==T) fishery$subarea[fishery$bank=="Mid"] <- "25A-Mi"

if(french==F) man.unit <- "Management\nunit"
if(french==T) man.unit <- "Zone de\ngestion"

if(french==F) fishery$subarea[fishery$bank=="Mid"] <- "25A-Mid"
fishery$subarea[fishery$bank=="Sab"] <- "25A-Sab"
fishery$subarea[fishery$bank=="Ban"] <- "25B"
fishery$subarea[fishery$bank=="BBn"] <- "26A"
fishery$subarea[fishery$bank=="BBs"] <- "26B"
fishery$subarea[fishery$bank=="Ger"] <- "26C"
fishery$subarea[fishery$bank=="GBb"] <- "27B"

for(sfa in 25){
  #png(filename=paste0(plotsGo, "/Fishery/fishery_", sfa, fr, ".png"),width=6.5*1.5, height=4*1.5, units = "in", res=420)
  kg <- ggplot() + geom_point(data=fishery[fishery$sfa==sfa,], aes(year, catch/1000, colour=subarea, shape=subarea), size=2) +
    geom_line(data=fishery[fishery$sfa==sfa,], aes(year, catch/1000, colour=subarea)) +
    theme_bw() +
    scale_colour_manual(values=c("black", "darkorange", "blue"), name=man.unit) +
    scale_shape_discrete(name=man.unit) +
    xlab(en2fr("Year", translate = french, custom_terms=rosetta_terms)) +
    ylab(paste0(en2fr("Catch", translate=french, custom_terms=rosetta_terms), " (t)"))+
    guides(colour="none", shape="none") +
    theme(axis.title.y = element_text(vjust=0.5))

  hm <- ggplot() + geom_point(data=fishery[fishery$sfa==sfa,], aes(year, effort, colour=subarea, shape=subarea), size=2) +
    geom_line(data=fishery[fishery$sfa==sfa,], aes(year, effort, colour=subarea)) +
    theme_bw() +
    scale_colour_manual(values=c("black", "darkorange", "blue"), name=man.unit) +
    scale_shape_discrete(name=man.unit) +
    xlab(en2fr("Year", translate = french, custom_terms=rosetta_terms)) +
    ylab(paste0(en2fr("Effort", translate=french, custom_terms=rosetta_terms), " (", en2fr("hour-metre", translate=french, custom_terms=rosetta_terms), ")")) +
    theme(axis.title.y = element_text(vjust=0.5))

  cp <- ggplot() + geom_point(data=fishery[fishery$sfa==sfa,], aes(year, cpue, colour=subarea, shape=subarea), size=2) +
    geom_line(data=fishery[fishery$sfa==sfa,], aes(year, cpue, colour=subarea)) +
    geom_errorbar(data=fishery[fishery$sfa==sfa,], aes(x=year, ymax=UCI, ymin=LCI, colour=subarea), width=0) +
    theme_bw() +
    scale_colour_manual(values=c("black", "darkorange", "blue"), name=man.unit) +
    scale_shape_discrete(name=man.unit) +
    xlab(en2fr("Year", translate = french, custom_terms=rosetta_terms)) +
    ylab(cpuelab) +
    guides(colour="none", shape="none")

  #regular:
  print(kg / hm / cp)
  #presentation:
  #print(kg / hm / cp + plot_layout(guides = "collect") & theme(legend.position = 'bottom'))
  #dev.off()
}


#labels for mapping
labels <- github_spatial_import(subfolder = "other_boundaries/labels", zipname = "labels.zip")
labels <- labels[grepl('offshore_detailed',labels$region),]

sfa.labels <- labels[grep(x=labels$lab_short, "SFA"),]

sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="A ", replacement="A", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=" (", replacement="\n", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=")", replacement="", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern=" Bank", replacement="", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="-BAN", replacement="", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="north", replacement="North", fixed=T)
sfa.labels$lab_short <- gsub(x = sfa.labels$lab_short, pattern="south", replacement="South", fixed=T)
sfa.labels$lab_short[sfa.labels$lab_short=="SFA27A"] <- "SFA27A\nGeorges 'a'"
sfa.labels$lab_short[sfa.labels$lab_short=="SFA27B"] <- "SFA27B\nGeorges 'b'"
sfa.labels <- sfa.labels[-grep(pattern = "Includes", x=sfa.labels$lab_short),]
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nG", replacement="26C\nG", fixed=T)
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nBrowns North", replacement="26A\nBrowns North", fixed=T)
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="26\nBrowns South", replacement="26B\nBrowns South", fixed=T)
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="25\nBa", replacement="25B\nBa", fixed=T)
sfa.labels$lab_short <- gsub(x=sfa.labels$lab_short, pattern="25\nEa", replacement="25A\nEa", fixed=T)

sfa.labels <- sfa.labels %>%
  tidyr::separate(lab_short, into=c("SFA", "bank"), sep="\n", remove=F)

sf_use_s2(FALSE)
joined <- st_join(offshore, sfa.labels)
sf_use_s2(TRUE)

joined <- joined[!(joined$bank=="Banquereau" & joined$ID.x=="SFA25A"),]
joined$fr <- joined$lab_short
joined$fr <- gsub(x=joined$fr, pattern="Browns South", replacement ="Secteur sud du banc de Browns")
joined$fr <- gsub(x=joined$fr, pattern="Browns North", replacement ="Secteur nord du banc de Browns")
joined$fr <- gsub(x=joined$fr, pattern="Georges 'a'", replacement ="Zone \u00ABa\u00BB du banc de Georges")
joined$fr <- gsub(x=joined$fr, pattern="Georges 'b'", replacement ="Zone \u00ABb\u00BB du banc de Georges")
joined$fr <- gsub(x=joined$fr, pattern="Eastern Scotian Shelf", replacement ="Est du plateau n\u00E9o-\u00E9cossais")
joined$fr <- gsub(x=joined$fr, pattern="Banquereau", replacement ="Banc Banquereau")
joined$fr <- gsub(x=joined$fr, pattern="SFA", replacement ="ZPP")

nonGB <- joined[!joined$SFA %in% c("SFA27A", "SFA27B"),]
nonGB$nudgex <- NA
nonGB$nudgey <- NA
nonGB$nudgex[nonGB$SFA %in% "SFA26A"] <- -10000
nonGB$nudgex[nonGB$SFA %in% "SFA26B"] <- -60000
nonGB$nudgex[nonGB$SFA %in% "SFA26C"] <- -40000
nonGB$nudgey[nonGB$SFA %in% "SFA26A"] <- 0
nonGB$nudgey[nonGB$SFA %in% "SFA26B"] <- 60000
nonGB$nudgey[nonGB$SFA %in% "SFA26C"] <- 0
nonGB$nudgex[nonGB$SFA %in% "SFA25B"] <- -100000
nonGB$nudgey[nonGB$SFA %in% "SFA25B"] <- 10000
nonGB$nudgex[nonGB$SFA %in% "SFA25A"] <- 0
nonGB$nudgey[nonGB$SFA %in% "SFA25A"] <- 0

# calculating footprints
sfa <- 25
if(sfa==25) {
  crs <- 32620
  foot <- st_as_sf(fish_sf[grep(x=fish_sf$sfa, pattern="25"),]) %>%
    st_transform(crs)
  base <- offshore[grep(x=offshore$ID, pattern="25"),] %>% st_transform(crs)
  base$ID <- gsub(x=base$ID, pattern=".shp", replacement="")
}

sort(unique(foot$year))

r <- create_grid(gridsize = sqrt(10)*1000, polygon = base)

#plot(r)

foot <- foot %>%
  st_intersection(r) %>%
  group_by(bank, cell) %>%
  dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
                   hm = sum(hm, na.rm=T),
                   nvessels = length(unique(vrnum)) + length(unique(vesid)))

st_geometry(foot) <- NULL

foot <- dplyr::left_join(r, foot)

lims <- foot[!is.na(foot$kg),] %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_buffer(dist=50000) %>%
  st_bbox()


if(french==T) nonGB$final <- nonGB$fr
if(french==F) nonGB$final <- nonGB$lab_short

bp <- pecjector(area=list(y = c(lims$ymin[[1]], lims$ymax[[1]]),
                          x =c(lims$xmin[[1]], lims$xmax[[1]]),
                          crs = crs),
                repo = 'github',c_sys = 32620, add_layer = list(bathy = c(200,'c'), land="grey", sfa = 'offshore', scale.bar="br"),plot=F, quiet=T)# +

#manual adjustment to scalebar text size
bp$layers[[5]]$geom_params$text_cex <- 1.5

#png(filename=paste0(plotsGo, "/Fishery/footprint_", sfa, fr, ".png"),width=6.5*2, height=4*2, units = "in", res=420)
print(bp +
        geom_sf(data=foot[!is.na(foot$kg),], aes(fill=kg/1000), colour=NA, show.legend=T) +
        theme(legend.position="right") +
        geom_sf(data=foot[!is.na(foot$kg) & foot$nvessels<5,], aes(),fill="red", colour=NA, show.legend=T) +
        theme(legend.position="right") +
        scale_fill_viridis_c(name=paste0(en2fr(x = "Catch", translate=french, custom_terms=rosetta_terms), " (t)"), option="G", begin=0.95, end=0)+
        coord_sf(expand=F)+
        geom_sf_text(data = nonGB[nonGB$SFA %in% base$ID,],
                     aes(label = final),
                     size=6,
                     fun.geometry = sf::st_centroid,
                     nudge_x = nonGB[nonGB$SFA %in% base$ID,]$nudgex,
                     nudge_y = nonGB[nonGB$SFA %in% base$ID,]$nudgey
        )#+
      #theme(legend.position="bottom",legend.key.width= unit(2, 'cm'))

foot$kg_clean <- foot$kg
foot$kg_clean[!is.na(foot$kg_clean) & foot$nvessels<5] <- "Y"
foot$kg[foot$kg_clean=="Y"]<- NA
foot$kg_clean[!is.na(foot$kg) & foot$nvessels>=5] <- "Y"
foot$kg_clean[is.na(foot$kg) & is.na(foot$kg_clean)] <- "N"

foot$hm_clean <- foot$hm
foot$hm_clean[!is.na(foot$hm_clean) & foot$nvessels<5] <- "Y"
foot$hm[foot$hm_clean=="Y"]<- NA
foot$hm_clean[!is.na(foot$hm) & foot$nvessels>=5] <- "Y"
foot$hm_clean[is.na(foot$hm) & is.na(foot$hm_clean)] <- "N"

coords <- as.data.frame(st_coordinates(st_centroid(foot)))
coords$X_utm <- coords$X
coords$Y_utm <- coords$Y
foot$lon_UTM <- coords$X_utm
foot$lat_UTM <- coords$Y_utm

coords_wgs <- as.data.frame(st_coordinates(st_transform(st_centroid(foot), 4326)))
coords_wgs$X_wgs <- coords_wgs$X
coords_wgs$Y_wgs <- coords_wgs$Y
foot$lon_WGS <- coords_wgs$X_wgs
foot$lat_WGS <- coords_wgs$Y_wgs

# ggplot() + geom_sf(data=foot[c(1:10, 1000:1010, 2000:2010),]) +
#   geom_text(data=foot[c(1:10, 1000:1010, 2000:2010),], aes(lon, lat, label=cell))

out <- dplyr::select(foot, cell, lon_UTM, lat_UTM, lon_WGS, lat_WGS, kg, kg_clean, hm, hm_clean)
st_geometry(out) <- NULL
out <- out[out$kg_clean=="Y" | out$hm_clean=="Y",]

summary(out)
out$privacy_screen <- NA
out$privacy_screen[is.na(out$kg)] <- "screened out"
out$privacy_screen[!is.na(out$kg)] <- "screened in"

out <- dplyr::select(out, lon_UTM, lat_UTM, lon_WGS, lat_WGS, kg, hm, privacy_screen)

ggplot() + geom_point(data=out,aes(lon_UTM,lat_UTM, colour=kg))

write.csv(x=out, file="Y:/Offshore/Data requests/2025/DR2025_12_Lahave/DR2025_12.csv", row.names = F)
