mandelbaum <- st_as_sf(data.frame(X = c(0,0.25,0.5,.75,1.1),Y=c(0,0.25,0.5,0.75,1.1),ID=1:5),coords = c("X","Y"))

molly <- st_as_sf(data.frame(X = c(0,0.5,0.5,0,0),Y=c(0,0,0.5,0.5,0),ID=1),coords = c("X","Y"))
polyanna <- st_sf(geometry = st_cast(st_combine(molly),"POLYGON"),h=5)
al <- st_as_sf(data.frame(X = c(0.5,1,1,0.5,0.5),Y=c(0.5,0.5,1,1,0.5),ID=2),coords = c("X","Y"))
jasmine <- st_sf(geometry = st_cast(st_combine(al),"POLYGON"),h=6)
jon <- st_as_sf(data.frame(X =c(0,0.5,0.5,0,0),Y=c(0.5,0.5,1,1,0.5),ID=3),coords = c("X","Y"))
arbuckle <- st_sf(geometry = st_cast(st_combine(jon),"POLYGON"),h=7)

garfield <- rbind(polyanna,jasmine,arbuckle)

ggplot(dat=garfield) + geom_sf() + geom_sf(data = mandelbaum)


# which points fall on the border of any of the polygons?
harry <- st_touches(garfield,mandelbaum)
# And this gets us the touching points, but of course is a pain because it returns point 3, 3 times because it falls on 3 borders...
# But if you just want to know the touching points, here's a way to get those...
sally <- mandelbaum[unique(unlist(harry)),]

# if we flipped the script for harry...
harrier <- st_touches(mandelbaum,garfield)
# This tells use which of the polygons the points touch. You could do something similar to above to extract the polygon-point combos from this

# which points fall inside the polygon using st_join, here we see that points on the lines are being returned and points that are touching multiple polygons get returned multiple times.
# The geometry returned is for the polygon that the point falls into.
jimmy <- st_join(garfield,mandelbaum)
jimmy
# Now the opposite will give you the points and the feature from the associated polygon (the h).  But this also includes points that touch, they aren't being returned as NA's for me
swift <- st_join(mandelbaum,garfield)
swift

ggplot(jimmy) + geom_sf()
# This gives the identical result as st_join, which I think makes sense as it should be what st_join is doing...
arlene <- st_intersection(garfield,mandelbaum)

# What if we made the polygon a multi-polygon...
odie <- st_sf(geometry = st_combine(garfield), h=7)
# That is a bit cleaner as it just returns the 2 points touching...
nermal <-  st_touches(odie,mandelbaum)
nermal
# Now the join on that...it returns all 4 points
blinky <- st_join(odie,mandelbaum)
blinky
# Same behaviour is the POLYGON, only crap that is outside the polygon region is getting an NA label.
crusty <- st_join(mandelbaum,odie)
crusty
