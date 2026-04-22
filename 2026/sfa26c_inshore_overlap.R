# The following script examines overlaps and gaps between the SFA26C offshore 
# scallop boundary and SFA29 inshore scallop boundary. Any overlap and gaps were 
# corrected in QGIS and are examined below. 

# Set working directory 
setwd("C:/Users/HARDERSOP/Documents/GitHub/Supporting_task_code/2026")

github_folder <- "C:/Users/HARDERSOP/Documents/GitHub " # Path to your Github folder
output_path <- "C:/Users/HARDERSOP/Documents/Spatial/SFA26C" # Change to a local drive

# Load required packages: Check for, and install missing packages 
required_packages <- c("sf", "dplyr", "ggplot2", "terra", "mapview")

install_if_missing <- function(required_packages) {
  if (!requireNamespace(required_packages, quietly = TRUE)){
    install.packages(required_packages)
  }
}
lapply(required_packages, library, character.only = TRUE)

# Read in shapefiles (issues using the GitHub pathway - update to your own C-drive pathway
SFA29_subareas <- st_read("C:/Users/HARDERSOP/Documents/GitHub/GIS_layers/inshore_boundaries/SFA29_subareas_utm19N.shp")
SFA29_boundaries <- st_read("C:/Users/HARDERSOP/Documents/GitHub/GIS_layers/inshore_boundaries/SFA29_BoundariesFollowing12nmDD_NoSubareas_WGS84.shp") 
survey_boundaries <- st_read("C:/Users/HARDERSOP/Documents/GitHub/GIS_layers/survey_boundaries/Ger.shp") 
SFA26c <- st_read("C:/Users/HARDERSOP/Documents/GitHub/GIS_layers/offshore/SFA26C.shp") 
german <- st_read("C:/Users/HARDERSOP/Documents/GitHub/GIS_layers/other_boundaries/WGS_84_German.shp") 
SFAs_2024 <- st_read("C:/Users/HARDERSOP/Documents/GitHub/GIS_layers/scallop_management_zones/ScallopFishingAreas_2024.shp") 

################################################################################
# Step 1: Plot all shapefiles on one map
################################################################################

### 1.1: Check CRS of all files
# List of shapefiles
layers <- list(SFA29_subareas, SFA29_boundaries, survey_boundaries, SFA26c, german, SFAs_2024)

# Check crs of all listed shapefiles
crs_info <- lapply(layers, st_crs)
print(crs_info) # Print output

# There are 3 different CRSs in use between the six layers

# Project SFA29_subareas, german_wgs84 and SFAs_2024 to the WGS84 - recommended that all layers have same spherical CRS before projecting to UTM
SFA29_subareas_wgs84 <- st_transform(SFA29_subareas, 4326)
german_wgs84 <- st_transform(german, 4326)
SFAs_2024_wgs84 <-  st_transform(SFAs_2024, 4326)


# Map WGS84 layers with mapview (WGS84 basemap)
everything_map <- mapview(SFA29_subareas_wgs84, col.region = "red", layer.name = "SFA29 Subareas", legend = TRUE) +
  mapview(SFA29_boundaries, col.region = "blue", layer.name = "SFA29 Boundary", legend = TRUE) +
  mapview(survey_boundaries, col.region = "green", layer.name = "Survey Boundary", legend = TRUE) +
  mapview(SFA26c, col.region = "orange", layer.name ="SFA26C Boundary", legend = TRUE) +
  mapview(german_wgs84, col.region = "yellow", layer.name = "German Bank", legend = TRUE) +
  mapview(SFAs_2024_wgs84, col.region = "purple", layer.name = "SFAs 2024", legend = TRUE) 
print(everything_map)

# Map SFA29 boundary and SFA26C boundary for closer comparison 
SFA29_26_overlap_map <- mapview(SFA29_boundaries, layer.name = "SFA29 Boundary", col.region = "blue", legend = TRUE) +
  mapview(SFA26c, layer.name ="SFA26C Boundary", col.region = "orange", legend = TRUE)
print(SFA29_26_overlap_map)

# Plot SFA29 boundary and SFA26C boundary with ggplot
plot_SFA_overlap <- ggplot() +
  geom_sf(data = SFA29_boundaries$geometry, fill = NA, color = "blue", size = 1) +
  geom_sf(data = SFA26c, color = "orange", size = 0.5) +
  theme_minimal() 
labs (title = "SFA 29 and SFA 26C Boundary Comparison")
print(plot_SFA_overlap)

################################################################################
# Step 2: Prepare layers for export (for working in QGIS)
################################################################################

# Project the focus layers (SFA29 boundary and SFA 26C boundary) to UTM projected CRS (necessary for any distance/area calculations).
SFA29_boundaries_utm <- st_transform(SFA29_boundaries, 32619)
SFA26c_utm <- st_transform(SFA26c, 32619 )

# Increase the number of vertices in the SFA26C boundary to allow for a smoother/more manipulatable line 
SFA26C_vert_dense <- st_segmentize(SFA26c_utm, dfMaxLength = 100) # max_distance = maximum distance between consecutive vertices

# Check number of points before and after vertex addition
n_points_before <- length(st_coordinates(SFA26c_utm)[,1])
n_points_after  <- length(st_coordinates(SFA26C_vert_dense)[,1])
cat("Points before:", n_points_before, "\n")
cat("Points after :", n_points_after, "\n")

# Confirm that SFA26C_vert_dense is in the correct CRS (UTM 19N)
crs(SFA26C_vert_dense)

# Write to shapefiles (to remove overlap in QGIS - not working in R)
tryCatch({
  st_write(SFA29_boundaries_utm, file.path(output_path, "SFA29_boundaries_utm.shp"), delete_layer = TRUE)
  st_write(SFA26C_vert_dense, file.path(output_path, "SFA26c_utm.shp"), delete_layer = TRUE)
  message("Shapefiles written successfully.")
}, error = function(e) {
  message("Error writing shapefiles: ", e$message)
})

# Used the Vertex Tool in QGIS with snapping turned on for SFA29 boundary (no overlap allowance) to manipulate the SFA26C boundary to match that of SFA29

################################################################################
# Step 3: Check new SFA26C boundary for overlap
################################################################################

# Read in new SFA26C boundary files - two new boundaries were made, one with a 10m buffer from SFA29, and one that touches the boundary of SFA29
# change to Github path
SFA26C_buffered <- st_read("C:/Users/HARDERSOP/Documents/Spatial/SFA26C/SFA26C_buffered.shp") # Buffered boundary
SFA26C_no_buffer <- st_read("C:/Users/HARDERSOP/Documents/Spatial/SFA26C/SFA26C_nobuffer.shp") # Non-buffered boundary

# Confirm that the new SFA26C layer does not overlap/intersect with SFA29:
# Buffered boundary
SFA_buffered_intersection <- st_intersection(SFA26C_buffered, SFA29_boundaries_utm)
print(SFA_buffered_intersection) # There are no observed intersections between this SFA26C boundary and the buffered SFA29 boundary

# Non-buffered (touching) boundaries
SFA_intersection <- st_intersection(SFA26C_no_buffer, SFA29_boundaries_utm)
print(SFA_intersection) # One observation (Multilinestring), which makes sense as the two boundaries would touch along their (now) shared boundary


################################################################################
# Step 5: Map SFA29 boundary and SFA26C boundary for closer comparison 
################################################################################

# Project new SFA26C layers CRS' back to WGS84 to match the CRS of mapview map
SFA26C_wgs <- st_transform(SFA26C_no_buffer, 4326)
SFA26C_buffered_wgs <- st_transform(SFA26C_buffered, 4326)

# Map WGS84 layers 
SFA_realigned_map <- mapview(SFA29_boundaries, layer.name = "SFA29 Boundary", col.region = "blue", legend = TRUE) +
  mapview(SFA26C_wgs, layer.name ="SFA26C Boundary (Not Buffered)", col.region = "darkorange", legend = TRUE) +
  mapview(SFA26C_buffered_wgs, layer.name ="SFA26C Boundary (Buffered)", col.region = "orange", legend = TRUE)
print(SFA_realigned_map)

# Write WGS84 boundaries to shapefiles 
tryCatch({
  st_write(SFA26C_wgs, file.path(output_path, "SFA26C_BoundaryUpdate_NoBuffer_WGS.shp"), delete_layer = TRUE)
  st_write(SFA26C_buffered_wgs, file.path(output_path, "SFA26C_BoundaryUpdate_Buffered_WGS.shp"), delete_layer = TRUE)
  message("Shapefiles written successfully.")
}, error = function(e) {
  message("Error writing shapefiles: ", e$message)
})
