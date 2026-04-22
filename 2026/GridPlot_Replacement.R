
# GridPlot Function Replacement (Issue #99) ----------------------------------

## Written by Sophia Harder (April 2026)


# ==============================================================================
# Re-create GridPlot Function (for all offshore SFAs)
# ==============================================================================

# Function Arguments ------------------------------------------------------

## sfa: the sfas you're interested in, must be named "sfa_vec" -- can be 1 or multiple/all SFAs ex.sfa_vec <- c("25A", "25B", "26A","26B", "26C", "27A", "27B")
## fisherydata: fishery log data 
## polygon: sfa boundaries -- use "offshore" -- offshore <- github_spatial_import("offshore", "offshore.zip", quiet=T)
## crs_list: list of sfas and their corresponding correct CRS : crs_list <- c("25A" = 32620,
#                                                                             "25B" = 32621,
#                                                                             "26A" = 32619,
#                                                                             "26B" = 32620,
#                                                                             "26C" = 32619,
#                                                                             "27A" = 32619,
#                                                                             "27B" = 32619)
## github_folder: pathway to your local github folder - this function calls the create_grid function found in //Assement_fns/Maps/create_grid.r
## gridsize: desired size of grid here we are using gridsize = sqrt(10) * 1000


gridPlot2 <- function(sfa, fisherydata, polygon, crs_list, github_folder, gridsize = sqrt(10)* 1000){
    ## load require packages
    require(sf)
    require(stars)
    require(dplyr)

    ## sanity checks
    missing_crs <- setdiff(sfa_vec, names(crs_list))
    if (length(missing_crs) > 0) {
      stop("Missing CRS for the following SFAs: ", paste(missing_crs, collapse = ", "))
    }

    ## load create_grid function
    source(paste0(github_folder, "/Maps/create_grid.R"))

      
      ## clean fisherydata
      fisherydata <- fisherydata %>% 
        dplyr::filter(
          !is.na(lon), !is.na(lat),
          lon != 0, lat != 0
        )
      
      ## convert fisherydata to sf
      fisherydata <- st_as_sf(fisherydata, coords = c(X = "lon", Y = "lat"), crs = 4326, remove = FALSE)
      
      ## output container 
      results <- vector("list", length(sfa_vec))
      names(results) <- sfa_vec
      
      ## loop over SFAs 
      for (sfa in sfa_vec){
        message("processing SFA", sfa, "...")
        
        crs <- crs_list[[sfa]]
      
      ## Subset, transform fisherydata
      foot <- st_as_sf(fisherydata[grep(x=fisherydata$sfa, pattern= sfa), ]) %>%
        st_transform(crs) 

      ## subset and transform polygon
      base <- polygon[grep(x=polygon$ID, pattern= sfa), ] %>%
        st_transform(crs)

      base$ID <- gsub(".shp", "", base$ID)

      ## create grid
      grid <- create_grid(gridsize = gridsize, polygon = base)

      ## Spatial aggregation
      foot_sum <- foot %>%
        st_intersection(grid) %>%
        group_by(cell) %>%
        dplyr::summarize(kg = sum(pro.repwt, na.rm = T),
                         hm = sum(hm, na.rm=T),
                         nvessels = length(unique(vrnum)),
                         .groups = "drop")

      ## drop geometry for join
      st_geometry(foot_sum) <- NULL

      ## join back to grid
      foot_sum <- dplyr::right_join(grid, foot_sum)  ## changed to a right join --> left join was dropping data

      ## store results
      results [[sfa]] <-  list(
        sfa = sfa,
        grid = grid,
        foot = foot_sum
      )
    }
    return(results)
  }
# Test Function -----------------------------------------------------------

sfa_vec <- c("25A", "25B", "26A","26B", "26C", "27A", "27B") ## Includes all the offshore SFAs but this list can be altered
## CRS for each sfa 
crs_list <- c(
  "25A" = 32620,
  "25B" = 32621,
  "26A" = 32619,
  "26B" = 32620,
  "26C" = 32619,
  "27A" = 32619,
  "27B" = 32619
)  

## polygon/sfa boundary data 
source(paste0(github_folder, "/Maps/github_spatial_import.R"))
offshore<- github_spatial_import("offshore", "offshore.zip", quiet=T)

## fishery data 
fish.dat <- read.csv("Y:/Offshore/Assessment/Data/Fishery_data/Logs/Compiled/2009-2022log_revisedMar182026.csv")


github_folder <- "C:/Users/keyserf/Documents/GitHub/Assessment_fns" # Path to your Github folder
test1 <- gridPlot2(sfa = sfa_vec, fisherydata = fish.dat, polygon = offshore, crs_list = crs_list, github_folder = github_folder, gridsize = sqrt(10)* 1000)
