source("C:/Documents/Assessment_fns/Maps/pectinid_projector_sf.R")

extent <- data.frame(x=c(-68, -64), y=c(41, 44), crs=4326)

basemap <- pecjector(area = extent, 
          repo="github", 
          add_layer=list(
            bathy=c(100, 'c', 200),
            eez="eez"),
          plot_package = "ggplot2", plot = T, quiet=T)

basemap$layers[[2]]$aes_params$colour <- "grey"

source("C:/Documents/Assessment_fns/Maps/github_spatial_import.R")


lines <- github_spatial_import(subfolder = "offshore", zipname="offshore.zip", quiet=T)


source("C:/Documents/Assessment_fns/Maps/convert_coords.R")

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
  geom_sf(data=lines_3[29,], colour="red")

gbline <- lines_3[lines_3$line %in%c(220),]
plot(gbline)

bbline <- lines_3[lines_3$line %in%c(29),]
plot(bbline)

resdocmap <- basemap + geom_sf(data=bbline) + geom_sf(data=gbline) + 
  annotate(geom="text", label="International Court of Justice (Hague) Line", x=-67.1, y=41.9, angle=-58) +
  annotate(geom="text", label="Georges\nBank", x=-66.5, y=41.85) +
  annotate(geom="text", label="'a'", x=-66.6, y=42.1) +
  annotate(geom="text", label="'b'", x=-66.35, y=42.2) +
  annotate(geom="text", label="Browns Bank", x=-65.8, y=42.75) +
  annotate(geom="text", label="north", x=-66.05, y=42.6) +
  annotate(geom="text", label="south", x=-65.65, y=42.5) +
  annotate(geom="text", label="German Bank", x=-66.1, y=43.2) +
  theme(axis.ticks=element_line(colour="black"), panel.border = element_rect(colour="black", fill=NA)) +
  coord_sf(xlim=c(-67.8, -64.18), ylim=c(41.18, 43.8))
  
resdocmap$theme$plot.margin <- margin(5.5, 11, 1, 1, "points")


png(filename="Y:/Projects/CSAS/2y_Projection_RAP/figures/offshore_map.png", height=8, width=9, units = "in", res=400)
print(resdocmap)
dev.off()
#so EEZ, GB a/b line, GB label, BB label, and BB N/S line- and labels for the a/b sides of GB and N/S sides of BB