# Tips for Sian

# by Freya 2023-01-30

#####################################################################################
# kernel density stacked by year

# creating some fake data to play with
sh <- runif(50, 20, 100)
mw <- runif(50, 5, 30)
year <- 2015:2022
mwsh <- data.frame(year= rep(year, each = 50), sh=rep(sh,8), mw=rep(mw, 8))

# reading in packages
require(ggplot2)
#install.packages("ggridges") # only run this the first time!
require(ggridges)
#require(viridis) # may be necessary for the "inferno" colour ramp, not sure

ggplot() + 
  geom_density_ridges_gradient(data=mwsh, aes(x=mw/sh*100, y=as.factor(year), group=year, fill=..x..)#, 
                               #scale=0.9, # you may wish to adjust this
                               #size=0.3, # you may wish to adjust this
                               #bandwidth=4 # you may wish to adjust this
                               ) +
  scale_fill_gradientn(colors=viridis(n=5,option="inferno"), 
                       name= expression(paste("CF ",bgroup("(",frac(g,dm^3)   ,")")))) +
  theme_bw() +
  ylab("Year") + 
  xlab(expression(paste("Condition factor ",bgroup("(",frac(g,dm^3)   ,")"))))


##################################################################################
# Mapping!

# get map of land
require(rnaturalearth)
require(rnaturalearthhires)

coast <- ne_coastline(returnclass = "sf") # gets you world coastline in sf format
ggplot() + geom_sf(data=coast) # notice that it's just the LINES (land isn't filled in). Probably fine if you want to plot the entire planet, but not what we want.

# try this instead
countries <- ne_countries(continent = "North America", returnclass="sf")
ggplot() + geom_sf(data=countries) # land is filled in this time. Better.

# zoom in on the area of interest
ggplot() + geom_sf(data=countries) +
  # geom_sf(data=...) + if you want to add an sf object on top
  # geom_point(data=) + if you want to add plain points on top
  xlim(-70, -52) + 
  ylim(40,50)
# ah... that's no good! Need better resolution base map!

# Try again, but with scale as "large"
countries <- ne_countries(continent = "North America", returnclass="sf", scale = "large")
ggplot() + geom_sf(data=countries) +
  # geom_sf(data=...) + if you want to add an sf object on top
  # geom_point(data=) + if you want to add plain points on top
  xlim(-70, -52) + 
  ylim(40,50)
# that's more like it!

# add important map characteristics and make it look better
require(ggspatial)
ggplot() + geom_sf(data=countries) +
  # geom_sf(data=...) + if you want to add an sf object on top
  # geom_point(data=) + if you want to add plain points on top
  xlim(-70, -52) + 
  ylim(40,50) +
  theme_minimal()+
  annotation_north_arrow(which_north = "true", 
                         location="br", # bottom right corner
                         pad_y=unit(1, "cm")) + # nudge it up a bit to accommodate scale bar
  annotation_scale(location="br") # bottom right corner

#save this plot so we can just add more stuff to it without writing it all out again
p <- ggplot() + geom_sf(data=countries) +
  # geom_sf(data=...) + if you want to add an sf object on top
  # geom_point(data=) + if you want to add plain points on top
  xlim(-70, -52) + 
  ylim(40,50) +
  theme_minimal()+
  annotation_north_arrow(which_north = "true", 
                         location="br", # bottom right corner
                         pad_y=unit(1, "cm")) + # nudge it up a bit to accommodate scale bar
  annotation_scale(location="br") # bottom right corner

# other ideas...
# if you want bathymetry, try marmap package
require(marmap)
b = getNOAA.bathy(lon1 = -75, lon2 = -45, lat1 = 35, lat2 = 50, 
                  resolution = 1)
# b isn't formatted nicely for ggplot. Use fortify() to fix it
b <- fortify(b) 

# figure out contours
range(b$z) #tells you the min and max
# so let's do 250 m contours
contours <- seq(0, min(b$z), -100)

p + geom_contour(data = b, 
                 aes(x=x, y=y, z=z),
                 breaks=contours, # play with this above
                 linewidth=c(0.3),
                 colour="grey")

# that's a bit too chaotic... let's just show contours for -100 to -1000 m
contours <- seq(0, -1000, -100)
p + geom_contour(data = b, 
                 aes(x=x, y=y, z=z),
                 breaks=contours, # play with this above
                 linewidth=c(0.3),
                 colour="grey")
# not bad. You can see Georges Bank nicely. Feel free to play around with this more until you get it just right. 

# notice how it's cut off along the edges? have to add this one last annoying step...
p + geom_contour(data = b, 
                 aes(x=x, y=y, z=z),
                 breaks=contours, # play with this above
                 linewidth=c(0.3),
                 colour="grey") +
  coord_sf(expand=F) # forces the plot limits to exactly match the xlim and ylim we told it earlier. 

# note, the warning message is just telling you that the xlim and ylim settings that we requested have resulted in some data being cut off
# I downloaded more bathymetry data (for a larger area) than we needed just to make sure it went all the way to the edge, so it makes sense that some was cut off!
# this IS something to watch out for in other plots though because you might accidentally cut off data that you want to show!


## if you want to add points
# creating a random point for the purposes of this example
random_x <- runif(n = 1, min = -70, max=-55)
random_y <- runif(n = 1, min = 40, max=45)
randompoint <- data.frame(x=random_x, y=random_y)

# plotting it on the map
p + geom_point(data=randompoint, aes(x, y), size=2, colour="red")

# you could also add a box to show GB using geom_rect(data=...) or annotate(geom="rect",...)
# you could add labels using annotate(geom="text",...)


