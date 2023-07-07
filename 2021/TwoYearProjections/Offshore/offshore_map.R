source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/pectinid_projector_sf.R")
require(sf)
extent <- data.frame(x=c(-68, -64), y=c(41, 44), crs=4326)
sf_use_s2(FALSE)
basemap <- pecjector(area = extent, 
          repo="github", 
          add_layer=list(
            bathy=c(100, 'c', 200),
            eez="eez"),
          plot_package = "ggplot2", plot = T, quiet=T)

basemap$layers[[2]]$aes_params$colour <- "grey"

source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/github_spatial_import.R")


lines <- github_spatial_import(subfolder = "offshore", zipname="offshore.zip", quiet=T)


source("C:/Users/keyserf/Documents/Github/Assessment_fns/Maps/convert_coords.R")

coords <- convert.coords(plot.extent = list(y=extent$y,x=extent$x),in.csys = extent$crs,out.csys = 4326, make.sf=T)
b.box <- st_make_valid(coords$b.box)

lines_2 <- st_intersection(lines[lines$ID%in% c("GBa", "GBb", "BBn", "BBs"),], b.box)

coords_lines <- as.data.frame(st_coordinates(lines_2))
coords_lines$UID <- paste0(coords_lines$L1, ".", coords_lines$L2)

st_sfc(st_cast(st_combine(lines_2), "MULTILINESTRING"))

basemap + geom_sf(data=lines_2, fill=NA) 

#install.packages("nngeo")
lines_3 <- nngeo::st_segments(lines_2)

lines_3$line <- 1:nrow(lines_3)

require(plotly)
ggplot() + geom_sf(data=lines_3) +
  geom_sf_label(data=lines_3, aes(label=line)) +
  geom_sf(data=lines_3[9,], colour="red")

gbline <- lines_3[lines_3$line %in%c(220),]
plot(gbline)

bbline <- lines_3[lines_3$line %in%c(9),]
plot(bbline)

require(rosettafish)
rosetta_terms <- as.data.frame(readr::read_csv("terms.csv"))
french <- T

if(french==T) {
  georgesbank <- "Banc de\nGeorges"
  ICJ <- "Ligne de la Cour Internationale de la Justice (Hague)"
}
if(french==F) {
  georgesbank <- "Georges\nbank"
  ICJ <- "International Court of Justice (Hague) Line"
}

resdocmap <- basemap + geom_sf(data=bbline) + geom_sf(data=gbline) + 
  annotate(geom="text", label=ICJ, x=-67.1, y=41.9, angle=-58) +
  annotate(geom="text", label=georgesbank, x=-66.5, y=41.85) +
  annotate(geom="text", label=en2fr("'a'", french,custom=rosetta_terms,case = 'lower'), x=-66.6, y=42.1) +
  annotate(geom="text", label=en2fr("'b'", french,custom=rosetta_terms,case = 'lower'), x=-66.35, y=42.2) +
  annotate(geom="text", label=en2fr("Browns Bank", french,custom=rosetta_terms), x=-65.8, y=42.75) +
  annotate(geom="text", label=en2fr("north", french,custom=rosetta_terms,case = 'lower'), x=-66.05, y=42.6) +
  annotate(geom="text", label=en2fr("south", french,custom=rosetta_terms,case = 'lower'), x=-65.65, y=42.5) +
  annotate(geom="text", label=en2fr("German Bank", french,custom=rosetta_terms), x=-66.1, y=43.2) +
  theme(axis.ticks=element_line(colour="black"), panel.border = element_rect(colour="black", fill=NA), panel.background=element_blank()) +
  coord_sf(xlim=c(-67.8, -64.18), ylim=c(41.18, 43.8)) +xlab(NULL) + ylab(NULL)
  
resdocmap$theme$plot.margin <- margin(5.5, 11, 1, 1, "points")


png(filename="Y:/Projects/CSAS/2y_Projection_RAP/figures/offshore_map_fr.png", height=8/1.3, width=12/1.3, units = "in", res=400)
print(resdocmap)
dev.off()
#so EEZ, GB a/b line, GB label, BB label, and BB N/S line- and labels for the a/b sides of GB and N/S sides of BB