
# return any points that are within a given km radius of a specified single point

within_radius <- function(buffer_dist_km, locations, point_lat, point_lon, crs_epsg, plot){
  
  require(sf) || stop("please install sf")
  require(ggplot2) || stop("please install ggplot2")

  point <- st_as_sf(x=data.frame(x=point_lon, y=point_lat), coords = c("x", "y"), crs = crs_epsg, remove=F) %>%
    st_transform(crs = 26920)
  
  buffered <- st_buffer(x = point, dist=buffer_dist_km*1000)
  
  if(!"SET_LONG" %in% names(locations) | !"SET_LAT" %in% names(locations)) message("please use the names SET_LONG and SET_LAT for your X and Y coordinates")
  
  locations <- st_as_sf(x=unique(locations), coords=c("SET_LONG", "SET_LAT"), crs=crs_epsg, remove=F)  %>%
    st_transform(crs = 26920)
  
  locations_within <- st_intersection(buffered, locations)
  
  if(plot==T) {
    p <- ggplot() + geom_sf(data=buffered) + 
      #geom_sf(data=locations) +
      geom_sf(data=locations_within, colour="red") +
      geom_sf(data=point, shape=8, size=5) +
      theme_classic() +
      ggtitle(paste0(buffer_dist_km, " km buffer zone"))
  
  print(p)
  }
  
  return(locations_within)

}


locs <- read.csv("C:/Users/keyserf/Downloads/Locations.csv")

station_list_5 <- within_radius(buffer_dist_km=5, locations=locs, point_lat=44.60500, point_lon=-66.07483, crs_epsg=4269, plot=T)
station_list_5$ID


station_list_10 <- within_radius(buffer_dist_km=10, locations=locs, point_lat=44.60500, point_lon=-66.07483, crs_epsg=4269, plot=T)
station_list_10$ID


station_list_15 <- within_radius(buffer_dist_km=15, locations=locs, point_lat=44.60500, point_lon=-66.07483, crs_epsg=4269, plot=T)
station_list_15$ID

