# This is a 1 off function used to tidy up the inshore strata layers in our GIS_layers repo, the inshore are currently a hot mess, which was rescued by JS identifying some really nice
# (yet not 100% correct) boundaries for inshore.  
# I am also going to change the area labels from csv files to shapefiles, with sf seamlessly keeping the meta data these can be make into point class shapefiles and added to plots easily (I think!)
# and I want to associate the traditional colour scheme of the offshore layers to each layer by adding a colour field.
# Frist bring in the files from github
require(tidyverse)
require(sf)
require(concaveman)
require(PBSmapping)
require(sp)
require(maptools)
require(dplyr)
require(scales)
options(stringsAsFactors = F)
source("D:/Github/Offshore/Assessment_fns/DK/Maps/pectinid_projector_sf.R")

off.pbs <- read.csv("D:/R/Data/Maps/approved/Survey/survey_detail_polygons.csv")
# Get rid of the old sable polygon
sab <- off.pbs[off.pbs$label=="Sab" & off.pbs$startyear == 2018,]
off.pbs <- rbind(off.pbs[off.pbs$label != "Sab",],sab)
# Need to get PID's sequential
off.pbs$PID[off.pbs$label == "BBn"] <- off.pbs$PID[off.pbs$label == "BBn"] + 5
off.pbs$PID[off.pbs$label == "GBa"] <- off.pbs$PID[off.pbs$label == "GBa"] + 10
off.pbs$PID[off.pbs$label == "GBb"] <- off.pbs$PID[off.pbs$label == "GBb"] + 17
off.pbs$PID[off.pbs$label == "BBs"] <- off.pbs$PID[off.pbs$label == "BBs"] + 22
off.pbs <- as.PolySet(off.pbs,projection = "LL")
off.pbs <- off.pbs[order(off.pbs$PID),]
table(off.pbs$PID) # good..



# Now we can add the metadata to each of these, kinda clumsy, but hey here we goes..
meta <- read.csv("D:/R/Data/Survey_data/survey_information.csv",stringsAsFactors = F)
# Get rid of old Sable
meta <- meta[-which(meta$label == "Sab" & meta$startyear == 1900),]


# Now this annoyingly strips all the meta data from off.pbs object
off.sp <- PolySet2SpatialPolygons(off.pbs, close_polys=TRUE)

# But unique returns them in proper order, thank you order
labs <- unique(off.pbs$Strata_ID)
# And each Strata gets it's own row in SF, so this = easy!
off.sf <- st_as_sf(off.sp)
off.sf$Strata_ID <- labs


# Now we can merge on strata ID, beauty!
off.final <- left_join(off.sf,meta,by="Strata_ID")
off.final$Strata_ID <- as.character(off.final$Strata_ID)
off.final$col <- as.character(off.final$col)

# SF is a little bit stupid with ordering (or I'm a little be stupid figuring it out), either way make this the color palette and the below shall work
tst <- off.final[order(off.final$Strata_ID),]

ggplot(off.final) + geom_sf(aes(fill = col)) + scale_fill_manual(values = tst$col)

# Write each of these to it's own shapefile layer, these were checked and the moved into the main offshore_survey_strata folder.
st_write(obj = off.final[off.final$label =="BBn" ,], dsn = "D:/Github/Offshore/GIS_layers/DK/offshore_survey_strata/BBn", layer = paste0(gsub(x="BBn", pattern=".shp", replacement="", fixed=T)), driver="ESRI Shapefile")
st_write(obj = off.final[off.final$label =="Sab" ,], dsn = "D:/Github/Offshore/GIS_layers/DK/offshore_survey_strata/Sab", layer = paste0(gsub(x="Sab", pattern=".shp", replacement="", fixed=T)), driver="ESRI Shapefile")
st_write(obj = off.final[off.final$label =="GBa" ,], dsn = "D:/Github/Offshore/GIS_layers/DK/offshore_survey_strata/GBa", layer = paste0(gsub(x="GBa", pattern=".shp", replacement="", fixed=T)), driver="ESRI Shapefile")
st_write(obj = off.final[off.final$label =="GBb" ,], dsn = "D:/Github/Offshore/GIS_layers/DK/offshore_survey_strata/GBb", layer = paste0(gsub(x="GBb", pattern=".shp", replacement="", fixed=T)), driver="ESRI Shapefile")
st_write(obj = off.final[off.final$label =="BBs" ,], dsn = "D:/Github/Offshore/GIS_layers/DK/offshore_survey_strata/BBs", layer = paste0(gsub(x="BBs", pattern=".shp", replacement="", fixed=T)), driver="ESRI Shapefile")

# Check these out if you like
tst <- st_read("D:/Github/Offshore/GIS_layers/DK/offshore_survey_strata/BBs.shp")
ggplot(tst) + geom_sf(aes(fill = Strt_ID)) + scale_fill_manual(values=tst$col)


# Now to sort out the mess that is inshore. Wait, the shapefiles came from this, which is from the database in 2017. This is now used for our inshore strata layers.
#inshore <- st_read("F:/ess_new/INSHORE SCALLOP/Databases/Scallsur/SCSTRATA/shp/PolygonSCSTRATAINFO_AccessedSept102017.shp")

# Figure out where your tempfiles are stored
temp <- tempfile()
# Download this to the temp directory you created above
download.file("https://raw.githubusercontent.com/Mar-scal/GIS_layers/master/inshore_survey_strata/inshore_survey_strata.zip", temp)
# Figure out what this file was saved as
temp2 <- tempfile()
# Unzip it
unzip(zipfile=temp, exdir=temp2)
#browser()
# This pulls in all the layers from the above location
inshore.strata <- all.layers(temp2,make.sf=T,make.polys=F)
# Now lets go backwards to make these sp files and then PBS mapping objects (yukky!)

# Now lets turn the labels into a shapefile we put in the GIS_layers folder.
source("D:/Github/Offshore/Assessment_fns/DK/Maps/pectinid_projector_sf.R")

windows(11,11)
p <-pecjector(area = "NL",direct_fns = "D:/Github/Offshore/Assessment_fns/DK/",repo = "D:/Github/Offshore/GIS_layers/DK/",
              add_layer = list(survey = c("all","outline"),
                               eez='eez',
                               sfa='offshore'))


off.labs <- read.csv("D:/R/Data/Maps/approved/Fishing_Area_Borders/offshore_Labels.csv")
off.labs <- st_as_sf(off.labs,coords = c("X","Y"),crs = 4326)

st_write()


p2 <- p + geom_sf_text(data=off.labs,aes(label = Names_short))
p2

repo <- "D:/R/Data/Maps/approved/GIS_layers/"
loc <- paste0(repo,"survey_boundaries")
# This pulls in all the layers from the above location
os <- all.layers(loc,make.sf=T,make.polys=F)

#browser()
# This pulls in all the layers from the above location, and puts some metadata in there matching offshore structure
p3 <- p + geom_sf(data=os,fill=NA)
p3
