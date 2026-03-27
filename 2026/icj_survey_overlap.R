# ICJ Survey Overlap 
# The following script shows that strata from George's Bank survey cross the ICJ line. 
# CRS incompatibilities are ruled out and strata overlap are quantified. 
# Completed by Sophia Harder in March, 2026.

# Clean Environment
rm(list = ls())

# Set working directory 
setwd("C:/Users/HARDERSOP/Documents/GitHub/Supporting_task_code/2026")

# Load required packages
# Check for, and install missing packages 
required_packages <- c("sf", "lwgeom", "dplyr", "ggplot2", "terra", "mapview")

install_if_missing <- function(required_packages) {
  if (!requireNamespace(required_packages, quietly = TRUE)){
    install.packages(required_packages)
  }
}
lapply(required_packages, library, character.only = TRUE)


# Get ICJ and survey strata files
github_folder <- "C:/Users/HARDERSOP/Documents/GitHub/Assessment_fns" # Path to your Github folder
output_path <- "C:/Users/HARDERSOP/Documents/Spatial/ICJ" # Change to a local drive

source(paste0(github_folder, "/Maps/github_spatial_import.R"))

survey_strata <- github_spatial_import(subfolder="offshore_survey_strata", zipname="offshore_survey_strata.zip", specific_shp = "Sab.shp")  
survey_strata_GBa <- github_spatial_import(subfolder="offshore_survey_strata", zipname="offshore_survey_strata.zip", specific_shp = "GBa.shp")
plot(survey_strata)
plot(survey_strata_GBa) # This is where overlap with ICJ line occurs

icj <- github_spatial_import(subfolder="EEZ", zipname="EEZ.zip", specific_shp = "EEZ.shp")
plot(icj)

# Filter ICJ line - the EEZ file contains multiple vectors, we only want the ICJ line 
icj_filtered <- icj %>%
  filter(TYPE_E == "EEZ_BB", SRC_AGENCY == "IBC")
plot(icj_filtered)

# Plot survey strata and ICJ line together
plot_strata <- ggplot() +
  geom_sf(data = survey_strata_GBa, fill = NA, color = "red", size = 1) +
  geom_sf(data = icj_filtered, color = "blue", size = 1) +
  theme_minimal() +
  labs (title = "Survey Strata + ICJ Boundary")
print(plot_strata)
# Visual examination shows that GBa survey strata overlaps the ICJ boundary 
################################################################################
# Step 1: Ensure CRS is not due to CRS differences
################################################################################

# Check CRS' of ICJ line and survey strata
st_crs(icj_filtered)
st_crs(survey_strata_GBa)

# CRS of ICJ is NAD83 and WGS84 for survey strata. 
# Transform NAD83 CRS to WGS84
icj_wgs <- st_transform(icj_filtered, crs = 4326)

# Plot
plot_wgs <- ggplot() +
  geom_sf(data = survey_strata_GBa, fill = NA, color = "red", size = 1) +
  geom_sf(data = icj_wgs, color = "blue", size = 1) +
  theme_minimal() +
  labs (title = "Survey Strata + ICJ Boundary (WGS84)")
print(plot_wgs)
# Overlap still present 
# Transform survey strata to NAD83 CRS
survey_nad <- st_transform(survey_strata_GBa, crs = 4269) # NAD83

# Plot
plot_nad<- ggplot() +
  geom_sf(data = survey_nad, fill = NA, color = "red", size = 1) +
  geom_sf(data = icj_filtered, color = "blue", size = 1) +
  theme_minimal() +
labs (title = "Survey Strata + ICJ Boundary (NAD83)")
print(plot_nad)

# Overlap still present 
# Project both ICJ line and survey strata to planar CRS 

################################################################################
# Step 2: Clip ICJ line to the extent of survey strata
################################################################################
# Get extent/ bounding box of survey strata
bbox <- st_bbox(survey_strata_GBa)

# Crop ICJ line to the survey strata extent
icj_crop <- st_crop(icj_wgs, bbox)

################################################################################
# Step 3: Project to planar CRS (GB = UTM zone 19)
################################################################################

# Project to UTM zone 19
icj_utm <- st_transform(icj_crop, "EPSG:32619")
survey_utm <- st_transform(survey_strata_GBa, "EPSG:32619")

# Plot
plot_utm<- ggplot() +
  geom_sf(data = survey_utm, fill = NA, color = "red", size = 1) +
  geom_sf(data = icj_utm, color = "blue", size = 1) +
  theme_minimal() +
labs (title = "Survey Strata + ICJ Boundary (UTM)")
print(plot_utm)



################################################################################
# Step 4: Remove survey polygons overlapping with ICJ boundary 
################################################################################

# Separate survey multipolygon into individual polygons
survey_polygons <- st_cast(survey_utm, "POLYGON")
print(survey_polygons)


# Write to shapefiles (to clip in QGIS cause clipping is not working in R)
tryCatch({
  st_write(icj_utm, file.path(output_path, "ICJ_UTM19.shp"), delete_layer = TRUE)
  st_write(survey_polygons, file.path(output_path, "Survey_Strata_UTM19.shp"), delete_layer = TRUE)
  message("Shapefiles written successfully.")
}, error = function(e) {
  message("Error writing shapefiles: ", e$message)
})

# In QGIS: survey_polyongs properly separated into individual polygons based on PNAME column and each of the resulting polygons split by ICJ line 
# Import individual survey strata shapefiles (created in QGIS)
tryCatch({
  survey_low <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/PName_Low (50-115 scallops_tow).shp")
  survey_mediumS <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/PName_Medium South (115-251 scallops_tow).shp")
  survey_mediumN <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/PName_Medium North (115-251 scallops_tow).shp")
  survey_highS <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/PName_High South (251-431 scallops_tow).shp")
  survey_vhighN <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/PName_Very High North (_431 scallops_tow).shp")
  survey_vhighS <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/PName_Very High South (_431 scallops_tow).shp")
  survey_highN <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/PName_High North (251-431 scallops_tow).shp")
})

# Double check CRS (should be UTM)
# list survey strata shapefiles 
strata_list <- list(survey_low, survey_mediumN, survey_mediumS, survey_highN, survey_highS, survey_vhighN, survey_vhighS)
# function to check CRS
crs_info <- function(obj){
  crs <- st_crs(obj)
  if(is.na(crs)) {
    return(NA_character_)
  } else {
    return(crs$epsg)
  }
}
# Apply to survey strata
crs_results <- sapply(strata_list, crs_info)
print(crs_results) # All EPSG: 32619

# Plot survey strata
plot_survey <- ggplot() +
  geom_sf(data = survey_low, fill = "red")+
  geom_sf(data = survey_mediumN, fill = "darkorange") +
  geom_sf(data = survey_mediumS, fill = "darkorange") +
  geom_sf(data = survey_highN, fill = "yellow") +
  geom_sf(data = survey_highS, fill = "yellow") +
  geom_sf(data = survey_vhighN, fill = "darkgreen") +
  geom_sf(data = survey_vhighS, fill = "darkgreen") +
  geom_sf(data = icj_utm, fill = NA, color = "black") +
  theme_minimal() +
  labs(title = "Survey Strata (UTM)")
print(plot_survey)

# Check validity of geometry 
survey_mediumN_validity<-  st_is_valid(survey_mediumN)
cat("Is geometry valid?", survey_mediumN_validity, "/n") # Not all valid
# Fix geometry of survey_split_mediumN
# Fix invalid geometries 
survey_mediumN[!survey_mediumN_validity, ] <- st_make_valid(survey_mediumN[!survey_mediumN_validity, ])
survey_mediumN_valid <- all(st_is_valid(survey_mediumN))
cat("All geometries valid? ", survey_mediumN_valid, "\n")

#st_write(survey_mediumN, file.path(output_path, "survey_mediumN.shp", delete_layer = TRUE))

# Import survey_split_ shapefiles created in QGIS
tryCatch({
  survey_split_low <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/split_low.shp")
  survey_split_mediumS <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/split_mediumS.shp")
  survey_split_mediumN <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/split_mediumN.shp")
  survey_split_highS <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/split_highS.shp")
  survey_split_highN <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/split_highN.shp")
  survey_split_vhighS <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/split_VhighS.shp")
  survey_split_vhighN <- st_read("C:/Users/HARDERSOP/Documents/Spatial/ICJ/split_VhighN.shp")
})

# Double check CRS 
# Split strata list
split_strata_list <- list(survey_split_low, survey_split_mediumN, survey_split_mediumS, survey_split_highN, survey_split_highS, survey_split_vhighN, survey_split_vhighS)
# Apply to survey strata
crs_results <- sapply(split_strata_list, crs_info)
print(crs_results) # All EPSG: 32619

plot_survey_split <- ggplot() +
  geom_sf(data = survey_split_low, fill = "red")+
  geom_sf(data = survey_split_mediumN, fill = "darkorange") +
  geom_sf(data = survey_split_mediumS, fill = "darkorange") +
  geom_sf(data = survey_split_highN, fill = "yellow") +
  geom_sf(data = survey_split_highS, fill = "yellow") +
  geom_sf(data = survey_split_vhighN, fill = "darkgreen") +
  geom_sf(data = survey_split_vhighS, fill = "darkgreen") +
  geom_sf(data = icj_utm, fill = NA, color = "black") +
  theme_minimal() +
  labs(title = "Split Survey Strata (UTM)")
print(plot_survey_split)

################################################################################
# Step 5: Quantify overlap 
################################################################################
# Difference the strata and split layers to quantify overlap 
# Low Strata 
low_area <- sum(st_area(survey_low))/1e6
print(low_area) #1101.367 Km2

int_low <- st_intersection(survey_low, survey_split_low)
plot(int_low$geometry)

int_low_area <- sum(st_area(int_low$geometry))/ 1e6
print(int_low_area) #1101.37

low_diff <- low_area - int_low_area
print(low_diff) #5.997047 Km2

# Medium North Strata
# Check geometry validity 
medN_validity <-  st_is_valid(survey_mediumN)
cat("Is geometry valid?", medN_validity, "/n") # Not all valid

survey_mediumN[!medN_validity, ] <- st_make_valid(survey_mediumN[!medN_validity, ])
survey_mediumN_valid <- all(st_is_valid(survey_mediumN))
cat("All geometries valid? ", survey_mediumN_valid, "\n")

medN_split_validity <-  st_is_valid(survey_split_mediumN)
cat("Is geometry valid?", medN_split_validity, "/n") # All valid

mediumN_area <- sum(st_area(survey_mediumN))/1e6
print(mediumN_area) #584.2781 Km2

int_mediumN <- st_intersection(survey_mediumN, survey_split_mediumN)
plot(int_mediumN$geometry)

int_mediumN_area <- sum(st_area(int_mediumN$geometry))/ 1e6
print(int_mediumN_area) #580.8809 Km2

mediumN_diff <- mediumN_area - int_mediumN_area
print(mediumN_diff) #3.397272 Km2

# Medium South Strata
mediumS_area <- sum(st_area(survey_mediumS))/1e6
print(mediumS_area) #689.0489 Km2


int_mediumS <- st_intersection(survey_mediumS, survey_split_mediumS)

int_mediumS_area <- sum(st_area(int_mediumS$geometry))/ 1e6
print(int_mediumS_area) #685.0177 Km2

mediumS_diff <- mediumS_area - int_mediumS_area
print(mediumS_diff) #4.031249 Km2

# High North Strata
# Invalid geometries error when trying to intersect high north layers- check geometry validity and fix
highN_validity <-  st_is_valid(survey_highN)
cat("Is geometry valid?", highN_validity, "/n") # Not all valid

split_highN_validity <- st_is_valid(survey_split_highN)
cat("Is geometry valid?", split_highN_validity, "/n") # Not all valid

# Fix invalid geometries: survey highN
survey_highN[!highN_validity, ] <- st_make_valid(survey_highN[!highN_validity, ])
highN_valid <- all(st_is_valid(survey_highN))
cat("All geometries valid? ", highN_valid, "\n")
plot(survey_highN$geometry)

# Fix invalid geometries: split highN
survey_split_highN[!split_highN_validity, ] <- st_make_valid(survey_split_highN[!split_highN_validity, ])
split_highN_valid <- all(st_is_valid(survey_split_highN))
cat("All geometries valid? ", split_highN_valid, "\n")
plot(survey_split_highN$geometry)

# Calculate areas and difference
highN_area <- sum(st_area(survey_highN))/1e6
print(highN_area) #535.5044 Km2

int_highN <- st_intersection(survey_highN, survey_split_highN)

int_highN_area <- sum(st_area(int_highN$geometry))/ 1e6 
print(int_highN_area) # 533.5926 Km2

# HighN_map <- mapview(survey_highN, layer.name = "Survey High North Strata", col.region = "red", legend = TRUE) +
#   mapview(survey_split_highN, layer.name = "Survey High North Strata Split by ICJ Line", col.region = "darkblue", legend = TRUE)+
#   mapview(survey_VhighN, layer.name = "Survey Very High, North Strata", col.region = "darkgreen", legend = TRUE)
# print(HighN_map) # survey_highN file is actually the survey_VhighN strata 

highN_diff <- highN_area - int_highN_area
print(highN_diff) #1.91187 Km2


# High South Strata
highS_area <- sum(st_area(survey_highS))/1e6
print(highS_area) #209.1321 Km2

int_highS <- st_intersection(survey_highS, survey_split_highS)
int_highS_area <- sum(st_area(int_highS$geometry))/ 1e6
print(int_highS_area)#206.6749 Km2

highS_diff <- highS_area - int_highS_area
print(highS_diff) #2.457256 Km2

# Very high North Strata
vhighN_validity <-  st_is_valid(survey_vhighN)
cat("Is geometry valid?", vhighN_validity, "/n") # Not all valid

split_vhighN_validity <- st_is_valid(survey_split_vhighN)
cat("Is geometry valid?", split_vhighN_validity, "/n") # All valid

# Fix invalid geometries 
survey_vhighN[!vhighN_validity, ] <- st_make_valid(survey_vhighN[!vhighN_validity, ])
vhighN_valid <- all(st_is_valid(survey_vhighN))
cat("All geometries valid? ", vhighN_valid, "\n")
# plot(survey_vhighN$geometry)

vhighN_area <- sum(st_area(survey_vhighN))/1e6
print(vhighN_area) #425.2528 Km2

int_vhighN <- st_intersection(survey_vhighN, survey_split_vhighN)
int_vhighN_area <- sum(st_area(int_vhighN$geometry))/ 1e6
print(int_vhighN_area) #425.2512 Km2

vhighN_diff <- vhighN_area - int_vhighN_area
print(vhighN_diff) #0.001544708 Km2

# Very high South Strata
vhighS_area <- sum(st_area(survey_vhighS))/1e6
print(vhighS_area) #163.8726 Km2

int_vhighS <- st_intersection(survey_vhighS, survey_split_vhighS)
int_vhighS_area <- sum(st_area(int_vhighS$geometry))/ 1e6
print(int_vhighS_area)

vhighS_diff <- vhighS_area - int_vhighS_area
print(vhighS_diff) #2.782391 Km2

################################################################################
# Calculate Overlap Percentage
################################################################################

# Sum all the differences to get total area of survey strata overlapping ICJ line
icj_overlap_area <- (sum(low_diff, mediumN_diff, mediumS_diff, highN_diff, highS_diff, vhighN_diff, vhighS_diff))  
print(icj_overlap_area) #20.57863 Km2 

# Calculate percentage of survey strata that overlaps ICJ line

# Calculate total area by adding strata area 
icj_area <- sum(low_area, mediumN_area, mediumS_area, highN_area, highS_area, vhighN_area, vhighS_area)
print(icj_area) #3714.456 Km2 

overlap_perc <- (icj_overlap_area/icj_area)*100
print(overlap_perc) #0.554%

(low_diff/low_area)*100 # 0.542% of "low" stratum over the ICJ line
(mediumN_diff/mediumN_area)*100 # 0.581% of "Medium North" stratum over the ICJ line
(mediumS_diff/mediumS_area)*100 # 0.585% of "Medium South" stratum over the ICJ line
(highN_diff/highN_area)*100 # 0.357% of "High North" stratum over the ICJ line
(highS_diff/highS_area)*100 # 1.175% of "High South" stratum over the ICJ line
(vhighN_diff/vhighN_area)*100 # 0.0004% of "Very High North" stratum over the ICJ line
(vhighS_diff/vhighS_area)*100 # 1.698% of "Very High South" stratum over the ICJ line

