require(ggrepel)
require(ggsflabel)
source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/pectinid_projector_sf.R")


mapall <-  pecjector(area = list(y = c(40,46.5),x = c(-68.1,-57),crs = 4326), 
                     add_layer = list(land = 'grey',
                                      eez = 'eez',
                                      bathy=c(100, 'c')), c_sys = 4326, quiet=T)

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")
offshore_survey <- github_spatial_import(subfolder = "survey_boundaries", zipname = "survey_boundaries.zip")
offshore_strata <- github_spatial_import(subfolder = "offshore_survey_strata", zipname = "offshore_survey_strata.zip")
inshore_survey <- github_spatial_import(subfolder = "inshore_boundaries", zipname = "inshore_boundaries.zip")
inshore_strata <- github_spatial_import(subfolder = "inshore_boundaries/inshore_survey_strata", zipname = "inshore_survey_strata.zip")
labels <- github_spatial_import(subfolder = "other_boundaries/labels", zipname = "labels.zip")
offshore_survey <- offshore_survey[!offshore_survey$ID %in% c("SPB", "BBs", "GBa", "GBb", "Sab", "BBn"),]

col <- offshore_strata$col
names(col) <- offshore_strata$Strt_ID

labels <- labels[!labels$lab_short %in% c("A", "B", "C", "D", "E", "SFA29E", "SPA2") & labels$region %in% c("inshore", "offshore"),]
ggplot()+ geom_sf(data=inshore_survey) + 
  geom_sf_text(data=labels[labels$lab_short%in% c("SPA6", "SPA3", "SPA1B"),], aes(label=lab_short, colour=as.factor(EID)))
ggplot()+ geom_sf(data=inshore_survey) + 
  geom_sf_text(data=labels[!labels$EID %in% c(11, 12, 13, 15, 16, 19, 20, 21, 31),], aes(label=lab_short, colour=as.factor(EID)))
labels <- labels[!labels$EID %in% c(11, 13, 16, 20, 21),]

mapall2 <- mapall + geom_sf(data=offshore_survey, fill="blue", alpha=0.1) +
  geom_sf(data=offshore_strata, aes(fill=Strt_ID), show.legend=F) +
  geom_sf(data=st_difference(inshore_survey), fill="green", alpha=0.1) +
  geom_sf_text(data=labels[labels$region=="offshore",], aes(label=lab_short)) +
  geom_sf_text(data=labels[labels$region=="inshore",], aes(label=lab_short)) +
  #geom_sf(data=inshore_strata) + 
  coord_sf(expand=F) +
  scale_y_continuous(limits=c(40, 46.5)) +
  scale_fill_manual(values=col) +
  theme_bw() +
  annotate(geom="point", x=-65.1, y=45.3, colour="red", size=3) +
  annotate(geom="point", x=-66.75, y=44.9, colour="red", size=3) +
  annotate(geom="point", x=-65.82, y=44.7, colour="red", size=3)
  

png("Y:/Projects/VIMS_sampling/Canadian_survey_areas.png", height=10, width=10, units="in", res=400)
print(mapall2)
dev.off()


