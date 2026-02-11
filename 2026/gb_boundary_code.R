# George's Bank (A & B) Boundary Merge 

## The following script merges the GBa and GBb boundaries, removes holes/gaps in three steps, and writes the merged boundary to a new shapefile 


github_folder <- "C:/Users/HARDERSOP/Documents/GitHub/Assessment_fns" # Path to your Github folder
output_path <- "C:/Users/HARDERSOP/Documents/Spatial/GB_Survey" # Change to a local drive

# Require necessary packages 
require(sf)
require(tidyverse)
require(dplyr)
require(ggplot2)

# Get GBa and GBb boundary files
source(paste0(github_folder, "/Maps/github_spatial_import.R"))

gba_boundary <- github_spatial_import(subfolder="survey_boundaries", zipname="survey_boundaries.zip", specific_shp = "GBa.shp")
gbb_boundary <- github_spatial_import(subfolder="survey_boundaries", zipname="survey_boundaries.zip", specific_shp = "GBb.shp")

#plot(gba_boundary)
#plot(gbb_boundary)

################################################################################
# step 1: Merge GBa and GBb boundaries 
#plot(st_union(gba_boundary, gbb_boundary)) # Old/original solution (with holes)

# Merge the two boundaries
merge_boundaries <- bind_rows(gba_boundary,gbb_boundary)

################################################################################
# Step 2: Remove holes
# Merged boundaries still have holes that must be removed:

remove_holes <- function(x) {
  # Keep only the exterior ring (first polygon part)
  st_cast(
    st_sfc(
      lapply(st_geometry(x), function(geom) {
        if (inherits(geom, "POLYGON")) {
          st_polygon(list(geom[[1]]))  # Keep only outer boundary
        } else if (inherits(geom, "MULTIPOLYGON")) {
          st_multipolygon(lapply(geom, function(poly) list(poly[[1]])))
        } else {
          geom
        }
      }),
      crs = st_crs(x)
    ),
    st_geometry_type(x, by_geometry = TRUE))
}
# The following function deals with a switch() warning from the remove_holes function during the mutate process below (creating polygon)
remove_holes_safe <- function(x, ...) {
  # Ensure input is sf or sfc
  if (!inherits(x, c("sf", "sfc"))){
    stop("remove_holes_safe() expects an sf or sfc object.")
  }
  # Suppress factor warning from internal switch()
  suppressWarnings({
    remove_holes(x, ...)
  })
}
# Create new polygon without holes
merged_no_holes <- merge_boundaries %>%
  st_union() %>%       # Merge into one geometry
  st_cast("POLYGON") %>% # Ensure polygons
  st_sf() %>% # Convert back to sf
  mutate(geometry = remove_holes_safe(geometry))

# Plot merged & cleaned boundary
plot(merged_no_holes,
     main = "Georges Bank (A & B) Boundary",
     col = "blue3",
     border = "black")

################################################################################
# Step 3: Remove/infill the 'inlet' where boundaries don't quite meet
# Create a dotted outline of the boundary border, then remove the dots that create the inlet/hole

# Get coordinates: Converts line geometry to point geometry
boundary_coords <- st_coordinates(merged_no_holes)
plot(boundary_coords)

# Coordinates need to be transformed to an sf object for mapping- df transformation required first
df_boundary <- data.frame(lon = boundary_coords[, 1], lat = boundary_coords[, 2])

# Transform into sf object (also sets crs- required for ggplot)
sf_boundary_coords <- st_as_sf(x = df_boundary,
                               coords = c("lon", "lat"),
                               crs = 4326
)

# Optional: Map coordinates (remove #)
#ggplot(data = sf_boundary_coords) +
#geom_sf( color = "blue") +
#labs (title = "George's Bank Boundary Vertices") +
#theme_minimal()

# Create bounding box around points we want to delete (this was eye-balled and adjusted through trial-and-error)
bbox_coords <- matrix(c(
  -66.100, 41.850,
  -66.000, 41.790,
  -65.950, 41.810,
  -66.050, 41.870,
  -66.100, 41.850
), ncol = 2, byrow = TRUE)

# Write bbox to polygon 
bbox <- st_polygon(list(bbox_coords))

# Convert back to sf object
sf_bbox <- st_sf(
  id = 1,
  geometry = st_sfc(bbox),
  crs = 4326)

# Plot point boundary and new bounding box 
plot_bbox <- ggplot() +
  geom_sf(data = sf_bbox, fill = NA, color = "red", size = 1) +
  geom_sf(data = sf_boundary_coords, color = "blue", size = 1) +
  theme_minimal() +
  labs (title = "George's Bank Boundary Vertices + Bbox") 
print(plot_bbox)

# Find which points are inside the bbox
inside_bbox <- st_intersects(sf_boundary_coords, sf_bbox, sparse = FALSE)[, 1]

# Remove points inside the bbox
gb_boundary_clean <- sf_boundary_coords[!inside_bbox, ]

# Optional:Plot new point boundary (remove #)
#ggplot() +
#geom_sf(data = gb_boundary_clean, color = "blue", size = 1) +
#theme_minimal() +
#labs (title = "George's Bank Merged (A & B) Boundary") 

# Turn back into sf polygon
gb_ab_coords<- st_coordinates(gb_boundary_clean) # get/separate coordinates of points 
gb_ab_boundary <- st_sfc(
  st_polygon(list(as.matrix(gb_ab_coords))),
  crs = 4326
)
# Plot new GB boundary containing no holes or 'inlets'
plot_gb_boundary_clean <- ggplot() +
  geom_sf(data = gb_ab_boundary, fill = "blue", color = "blue", size = 1) +
  theme_minimal() +
  labs (title = "George's Bank Merged (A & B) Boundary") 
print(plot_gb_boundary_clean)

# Plot of clean GB boundary with original Gba and GBb boundaries for comparison
plot_gb_comp <- ggplot() +
  geom_sf(data = gb_ab_boundary, fill = "blue", color = "blue", size = 3) +
  geom_sf(data = gba_boundary, fill = "white", color = "green", size = 1) +
  geom_sf(data = gbb_boundary, fill = "white", color = "red", size = 1) +
  theme_minimal() +
  labs ( title = " George's Bank Pre- and post - Merged Boundaries")
print(plot_gb_comp)

# Write boundary to shapefile (comment out if not needed)
tryCatch({
  st_write(gb_ab_boundary, output_path, driver = "ESRI Shapefile", delete_layer = TRUE)
  message("Shapefile successfully written to: ", output_path)
}, error = function(e) {
  message("Error writing shapefile: ", e$message)
})

# Check quality of shapefile 
r_shp_path <-"C:/Users/HARDERSOP/Documents/Spatial/GB_Survey/GB_Survey.shp" # Your output pathway
r_shp <-st_read(r_shp_path)
plot(r_shp)
class(r_shp)