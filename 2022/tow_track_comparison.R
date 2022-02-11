# comparing tow tracks
 
require(ggplot2)
require(tidyverse)
require(dplyr)
require(sf)

# read in the file for a specific bank (collected using Oceanvision). Edit the file path accordingly (years may change too!). 
# takes a few seconds to read in... this contains ALL of the survey data for this bank
oceanvision <- read.csv("Y:/Offshore/Assessment/Data/Survey_data/2021/Summer/GBa/Survey1984-2021.csv")

# simplify the dataframe to only what we need (tow numbers and coordinates)
ov2021 <- oceanvision %>%
  filter(year==2021) %>%
  select(tow, slon, slat, elon, elat)

# create a spatial sf object for the start points
ov2021_start <- ov2021 %>%
  select(tow, slon, slat) %>%
  st_as_sf(coords=c("slon", "slat"), crs=4326)
ov2021_start$pos <- "start"

# create a spatial sf object for the end points
ov2021_end <- ov2021 %>%
  select(tow, elon, elat) %>% 
  st_as_sf(coords=c("elon", "elat"), crs=4326)
ov2021_end$pos <- "end"

# combine the start and end points into one dataframe
ov2021 <- rbind(ov2021_start, ov2021_end)

# connect the points to create tracks by summarizing based on tow number. 
# Each track is a linestring, so the target object is a "multilinestring"
ov2021 <- ov2021 %>% 
  group_by(tow) %>% 
  summarize() %>% 
  st_cast("MULTILINESTRING")
    
# do the same thing for the olex data, to create a dataframe called olex2021.
# read in the data
# filter and select the relevant tow data and columns
# create 2 sf point objects: one for start points, one for end points
# rbind them
# connect the dots to get tracks


# plotting!
ggplot() + 
  # add the OV tows
  geom_sf(data=ov2021, lwd=2) + 
  # add the Olex tows (uncomment the line below)
  #geom_sf(data=olex2021, colour="red") +theme_bw() +
  # add some labels (should be the same for olex and OV)
  geom_sf_text(data=ov2021, aes(label=tow)) +
  # make it look nicer
  theme_bw()

# to make it interactive, put the above ggplot code inside a ggplotly function
require(plotly)
ggplotly(
  ggplot() + 
    geom_sf(data=ov2021, lwd=2) + 
    #geom_sf(data=olex2021, colour="red") +theme_bw() +
    geom_sf_text(data=ov2021, aes(label=tow)) +
    theme_bw()
)
