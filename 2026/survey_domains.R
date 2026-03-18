################################################################################
# The following script assesses the survey domain polygons against the survey (strata) boundary files 
# Provided to/for Offshore Wind project (Jessicia Sameoto and Kayla Silver, 2026)
# Started March 16th, 2026 by Sophia Harder

# instructions: 

# Set working directory 
setwd("C:/Users/HARDERSOP/Documents/GitHub/Supporting_task_code/2026")

# Import required packages
# Check for, and install missing packages 
required_packages <- c("sf", "lwgeom", "dplyr", "ggplot2", "terra", "mapview")

install_if_missing <- function(required_packages) {
  if (!requireNamespace(required_packages, quietly = TRUE)){
    install.packages(required_packages)
  }
}
lapply(required_packages, library, character.only = TRUE)

github_folder <- "C:/Users/HARDERSOP/Documents/GitHub" # Path to your Github folder
output_path <- "C:/Users/HARDERSOP/Documents/Spatial/Survey Domains" # Change to a local drive

source(paste0(github_folder, "/Assessment_fns/Maps/github_spatial_import.R"))

# Import survey strata
survey_strata_sab <- github_spatial_import(subfolder="offshore_survey_strata", zipname="offshore_survey_strata.zip", specific_shp = "Sab.shp") 
survey_strata_bbn <- github_spatial_import(subfolder="offshore_survey_strata", zipname="offshore_survey_strata.zip", specific_shp = "BBn.shp") 
survey_strata_bbs <- github_spatial_import(subfolder="offshore_survey_strata", zipname="offshore_survey_strata.zip", specific_shp = "BBs.shp") 
survey_strata_gba <- github_spatial_import(subfolder="offshore_survey_strata", zipname="offshore_survey_strata.zip", specific_shp = "GBa.shp") 
survey_strata_gbb <- github_spatial_import(subfolder="offshore_survey_strata", zipname="offshore_survey_strata.zip", specific_shp = "GBb.shp") 

# Import survey boundaries 
survey_boundary_sab <- github_spatial_import(subfolder="survey_boundaries", zipname="survey_boundaries.zip", specific_shp = "Sab.shp")
survey_boundary_bbn <- github_spatial_import(subfolder="survey_boundaries", zipname="survey_boundaries.zip", specific_shp = "BBn.shp")  
survey_boundary_bbs <- github_spatial_import(subfolder="survey_boundaries", zipname="survey_boundaries.zip", specific_shp = "BBs.shp")  
survey_boundary_gba <- github_spatial_import(subfolder="survey_boundaries", zipname="survey_boundaries.zip", specific_shp = "GBa.shp")  
survey_boundary_gbb <- github_spatial_import(subfolder="survey_boundaries", zipname="survey_boundaries.zip", specific_shp = "GBb.shp")  

# Check CRS' of all imported files
layer_list <- list(survey_strata_sab, survey_strata_bbn, survey_strata_bbs, survey_strata_gba, survey_strata_gbb, survey_boundary_sab, survey_boundary_bbn, survey_boundary_bbs, survey_boundary_gba, survey_boundary_gbb)
crs_info <- lapply(layer_list, st_crs)
print(crs_info) # All layers are in the WGS84 CRS

################################################################################
# Convert CRS to utm in order to calculate area
################################################################################
# UTM zone 20
sab_strata_utm <- st_transform(survey_strata_sab, crs = 32620)
sab_boundary_utm <-  st_transform(survey_boundary_sab, crs = 32620)
bbs_strata_utm <- st_transform(survey_strata_bbs, crs = 32620)
bbs_boundary_utm <- st_transform(survey_boundary_bbs, crs = 32620)

# UTM zone 19
bbn_strata_utm <- st_transform(survey_strata_bbn, crs = 32619)
bbn_boundary_utm <- st_transform(survey_boundary_bbn, crs = 32619)
gba_strata_utm <- st_transform(survey_strata_gba, crs = 32619)
gba_boundary_utm <-st_transform(survey_boundary_gba, crs = 32619)
gbb_strata_utm <- st_transform(survey_strata_gbb, crs = 32619)
gbb_boundary_utm <- st_transform(survey_boundary_gbb, crs = 32619)

################################################################################
# Calculate and compare areas of strata and boundaries
################################################################################

# Sable bank (Sab)
sab_boundary_area<- st_area(sab_boundary_utm)/ 1e6
print(sab_boundary_area) #4293.727 Km2

sab_stratum_area <- sum(st_area(sab_strata_utm)) / 1e6
print(sab_stratum_area) #4271.922 Km2 (matches survey design spreadsheet)

# Brown's bank North
bbn_boundary_area <- st_area(bbn_boundary_utm)/ 1e6
print(bbn_boundary_area) #1160.459 Km2 (matches survey design spreadsheet)

bbn_strata_area <- sum(st_area(bbn_strata_utm))/ 1e6
print(bbn_strata_area) #1160.459 Km2 (matches survey design spreadsheet)

# Brown's bank South
bbs_boundary_area <- st_area(bbs_boundary_utm)/ 1e6
print(bbs_boundary_area) #676.0275 Km2

bbs_strata_area <- sum(st_area(bbs_strata_utm))/ 1e6
print(bbs_strata_area) #524.124 Km2 (matches survey design spreadsheet)

# George's bank A
gba_boundary_area <- st_area(gba_boundary_utm)/ 1e6
print(gba_boundary_area) #3712.386 Km2

gba_strata_area <- sum(st_area(gba_strata_utm))/ 1e6
print(gba_strata_area) #3714.386 Km2

# George's bank B
gbb_boundary_area <- st_area(gbb_boundary_utm)/ 1e6
print(gbb_boundary_area) #536.4609 Km2

gbb_strata_area <- sum(st_area(gbb_strata_utm))/ 1e6
print(gbb_strata_area) #537.6865 (matches survey design spreadsheet)

################################################################################
# New/accurate survey domains
################################################################################
# for SFAs where the boundary area and strata areas are not equal, survey strata are used to create new boundaries/domains

######################## Sable Bank ############################################
sab_union <- st_union(sab_strata_utm)
print(sab_union)
plot(sab_union)

sab_union_area <- st_area(sab_union)/ 1e6
print(sab_union_area) # matches area of survey strata and survey design spreadsheet

require(mapview)
sab_map <- mapview(sab_strata_utm, layer.name = "Sable Bank Strata", col.region = "blue", legend = TRUE) +
  mapview(sab_union, layer.name = "Sable Bank Unioned Boundary", 
          alpha.regions = 0.25,
          col.region = "red", 
          color = "red", 
          lwd = 3) 
print(sab_map)

# Write to .shp
st_write(sab_union, file.path(output_path, "Sab_Survey_Domain.shp"), delete_layer = TRUE)

####################### Brown's Bank North #####################################
# Though the BBN strata and boundary don't have an area discrepancy, we will still create a new boundary for file consistency 
bbn_union <- st_union(bbn_strata_utm)
print(bbn_union)
plot(bbn_union)

bbn_union_area <- st_area(bbn_union)/ 1e6
print(bbn_boundary_area) # matches area of survey strata and survey design spreadsheet

bbn_map <- mapview(bbn_strata_utm, layer.name = "Brown's Bank North Strata", col.region = "blue", legend = TRUE) +
  mapview(bbn_union, layer.name = "Brown's Bank North Unioned Boundary", 
          alpha.regions = 0.25,
          col.region = "red", 
          color = "red", 
          lwd = 3) 
print(bbn_map)

# Write to .shp
st_write(bbn_union, file.path(output_path, "BBN_Survey_Domain.shp"), delete_layer = TRUE)


###################### Brown's Bank South ######################################
bbs_union <-  st_union(bbs_strata_utm)
print(bbs_union)

bbs_union_area <- st_area(bbs_union)/ 1e6
print(bbs_union_area) # matches area of survey strata and survey design spreadsheet
plot(bbs_union)

bbs_map <- mapview(bbs_strata_utm, layer.name = "Brown's Bank South Strata", col.region = "blue", legend = TRUE) +
  mapview(bbs_union, layer.name = "Brown's Bank South Unioned Boundary", 
          alpha.regions = 0.25,
          col.region = "red", 
          color = "red", 
          lwd = 3) 
print(bbs_map)

# write to .shp
st_write(bbs_union, file.path(output_path, "BBS_Survey_Domain.shp"), delete_layer = TRUE)

##################### George's Bank A ##########################################
# Survey strata, boundary, and survey design.xlsx all have different area values 
# Visually compare strata and boundary layers (also want to confirm the small interior polygons should not be holes)
pal = mapviewPalette("mapviewTopoColors")

gba_comp_map <- mapview(gba_boundary_utm, layer.name = "GBa Boundary", alpha.region = 0.25, col.region = "blue", legend = TRUE) +
  mapview(gba_strata_utm, zcol = "PName", layer.name = "Strata", alpha.region = 0.25, col.region = pal(7), legend = TRUE)  
print(gba_comp_map) # small polygons are in-fact survey strata

# gba_union <- st_union(gba_strata_utm) 
# invalid geometry error - could be due to the line delineating N/S falls outside the boundary of the SFA (will try cropping/clipping)

# Clip survey strata layer using the boundary layer to remove the straight line (Done in QGIS)
st_write(gba_strata_utm, file.path(output_path, "gba_survey_strata.shp"), delete_layer = TRUE)
st_write(gba_boundary_utm, file.path(output_path, "gba_survey_boundary.shp"), delete_layer = TRUE)

# Clipped in QGIS - read in new shapefiles
gba_strata_fix <- st_read(file.path(output_path, "/gba_survey_strata_fixed.shp"))
plot(gba_strata_fix$geometry)

# Check validity of geometry 
gba_validity <-  st_is_valid(gba_strata_fix)
cat("Is geometry valid?", gba_validity, "/n") # Not all valid

# Fix invalid geometries 
gba_strata_fix[!gba_validity, ] <- st_make_valid(gba_strata_fix[!gba_validity, ])
gba_valid <- all(st_is_valid(gba_strata_fix))
cat("All geometries valid? ", gba_valid, "\n")
plot(gba_strata_fix$geometry)
# Re-try union
gba_union <- st_union(gba_strata_fix)
plot(gba_union) # two lines still remain within the boundary 

# Get a closer look at the unioned boundary
gba_map <- mapview(gba_union, layer.name = "GBA Strata Union", alpha.region = 0.25, col.region = pal(7), legend = TRUE) +
  mapview(gba_strata_fix, zcol = "PName", layer.name = "Fixed Strata", alpha.region = 0.25, col.region = pal(7), legend = TRUE) 
print(gba_map)

# Fix in QGIS - deleted vertices from middle of the strata
st_write(gba_union, file.path(output_path, "gba_union.shp"), delete_layer = TRUE)
gba_union_fix <- st_read(file.path(output_path, "/gba_union_fix.shp"))
plot(gba_union_fix)

# Calculate area
gba_union_area <- st_area(gba_union_fix)/ 1e6
print(gba_union_area) # 3712.386 KM2 - matches the area of the GBa boundary but does not match the area of the strata or the area listed in the survey design
plot(gba_union_area)

mapview(gba_union_fix, layer.name = "GBA Corrected Union/ Survey Domain", col.region = "blue", legend = TRUE)

# Write to .shp
st_write(gba_union_fix, file.path(output_path, "GBA_Survey_Domain.shp"), delete_layer = TRUE)

######################## George's Bank B #######################################
gbb_union <- st_union(gbb_strata_utm)
plot(gbb_union)

gbb_union_area <- st_area(gbb_union)/ 1e6
print(gbb_union_area) # matches area of survey strata and survey design spreadsheet


gbb_map <- mapview(gbb_strata_utm, layer.name = "George's Bank B Strata", col.region = "blue", legend = TRUE) +
  mapview(gbb_union, layer.name = "George's Bank B Unioned Boundary", 
          alpha.regions = 0.25,
          col.region = "red", 
          color = "red", 
          lwd = 3) 
print(gbb_map)

# write to .shp
st_write(gbb_union, file.path(output_path, "GBB_Survey_Domain.shp"), delete_layer = TRUE)


####### George's Bank Comparison ###############################################
# It was discovered that GBa and GBb survey domains overlap. Both survey domains were sent off to Offshore wind without any changes, 
# but this is something to be aware of. 

# map comparing Gba and GBb survey domains 
gb_map <- mapview(gbb_union, layer.name = "George's Bank B Survey Domain", col.region = "blue", legend = TRUE) +
  mapview(gba_union_fix, layer.name = "George's Bank A Survey Domain", 
          alpha.regions = 0.25,
          col.region = "red", 
          color = "red", 
          lwd = 3) 
print(gb_map)

# Quantifying intersection 
gb_intersect <- st_intersection(gbb_union, gba_union_fix)
print(gb_intersect) # 6.135659 Km2 intersected
plot(gb_intersect)
gb_intersect_area <- st_area(gb_intersect)/ 1e6 # 6.135659 Km2 intersected
print(gb_intersect_area)
