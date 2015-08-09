#######Spatial Data in R################
#Luke Hedge
#Mitchell Lyons




##read in airport data from OpenFlights

library(RCurl)
URL <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
x <- getURL(URL)
airports <- read.csv(textConnection(x), header=F)
write.csv(airports, 'Data/airports.csv', row.names=F)


##maps data example
library('maps')
library('maptools')
oz <- map('world', 'Australia', plot=FALSE)
oz_crs <- CRS('+proj=longlat +ellps=WGS84')
oz <- map2SpatialLines(oz, proj4string=oz_crs)
str(oz, max.level=2)





###Mapping Journeys to Work in the UK

library(plyr)
library(ggplot2)
library(maptools)

input<-read.table("Data/wu03ew_v1.csv", sep=",", header=T)

input<- input[,1:3]
names(input)<- c("origin", "destination","total")

##data cleaning and manipulation
centroids<- read.csv("Data/msoa_popweightedcentroids.csv")
or.xy<- merge(input, centroids, by.x="origin", by.y="Code")
names(or.xy)<- c("origin", "destination", "trips", "o_name", "oX", "oY")
dest.xy<- merge(or.xy, centroids, by.x="destination", by.y="Code")
names(dest.xy)<- c("origin", "destination", "trips", "o_name", "oX", "oY","d_name", "dX", "dY")

##plotting in ggplot2
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

ggplot(dest.xy[which(dest.xy$trips>10),], aes(oX, oY))+
geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=trips), col="white")+
scale_alpha_continuous(range = c(0.03, 0.3))+
theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()




###SpatialPolygons class

library('maps')
library('maptools')
auc_crs <- CRS('+proj=longlat +ellps=WGS84')
auc_shore <- MapGen2SL('Data/auckland_mapgen.dat', auc_crs)
summary(auc_shore)

nz_lines <- slot(auc_shore, 'lines')
table(sapply(nz_lines, function(x) length(slot(x, 'Lines'))))

auc_islands <- sapply(nz_lines, function(x){
	crds <- slot(slot(x, 'Lines')[[1]], 'coords')
	identical(crds[1,], crds[nrow(crds),])
})

table(auc_islands)

auc_shore <- auc_shore[auc_islands]
list_of_lines <- slot(auc_shore, 'lines')


islands_sp <- SpatialPolygons(lapply(list_of_lines, function(x) {
    Polygons(list(Polygon(slot(slot(x, "Lines")[[1]], "coords"))),
      ID=slot(x, "ID"))
  }), proj4string=CRS("+proj=longlat +ellps=WGS84"))

summary(islands_sp)
getClass('SpatialPolygons')
slot(islands_sp, "plotOrder")



####Visualising Data

##basic intro
data(meuse)
str(meuse)
coordinates(meuse) <- c('x', 'y') 
str(meuse)
plot(meuse)

data(meuse.riv)
plot(meuse.riv)

meuse_lst <-list(Polygons(list(Polygon(meuse.riv)), "meuse.riv"))
meuse_sp <- SpatialPolygons(meuse_lst)


plot(meuse_sp, axes=T)
plot(meuse, add=T, pch=16, cex=0.5)


##par arguements
oldpar = par(no.readonly = TRUE)
layout(matrix(c(1,2),1,2))
plot(meuse, axes = TRUE, cex = 0.6)
title("Sample locations")

par(mar=c(0,0,0,0)+.1)
plot(meuse, axes = FALSE, cex = 0.6)
box()
par(oldpar)


##north arrow and scale bar
dev.off()
par(mar = c(1,1,1,1))
plot(meuse, cex = 0.7)
plot(meuse, cex = 0.6, add = TRUE)
SpatialPolygonsRescale(layout.scale.bar(), offset = c(180200,329600),
    scale = 1000, fill=c("transparent","black"), plot.grid = FALSE)
text(x = c(180200,181200), y = rep(329750, 2), c("0", "1 km"))
SpatialPolygonsRescale(layout.north.arrow(), offset = c(178750,332500),
    scale = 400, plot.grid = FALSE)
box()






###Reeflife survey data
rls_df <- read.csv('Data/Reef_Life_Survey_nsw.csv')
head(rls_df)
table(rls_df$SurveyID)

syd_df <- rls_df[grep('SYD',rls_df$SiteCode),]
summary(syd_df)
syd_sites <- droplevels(syd_df)
t <- data.frame(table(syd_sites$Site))

library(dplyr)

s <- syd_sites %>%
	group_by(Site, SurveyID) %>%
	summarise(lat=mean(SiteLat),
			  lon=mean(SiteLong),
			  richness=length(unique(Taxon)),
			  totalfish=sum(Total))
write.csv(s,'Data/rls_fishrichness.csv', row.names=F)
s <- read.csv('Data/rls_fishrichness.csv')

library(sp)
library(rgdal)
rls_spdf <- SpatialPointsDataFrame(coords=data.frame(s$lon, s$lat),
	data=s, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
rls_spdf <- spTransform(rls_spdf, CRS('+proj=utm +zone=56 +south +ellps=WGS84 +units=m +no_defs'))
writeOGR(rls_spdf, driver='ESRI Shapefile', dsn='Data', layer='rls_sites_sydney')


rls <- readOGR(dsn='Data', layer='rls_sites_sydney')
summary(rls)
syd <- readOGR(dsn='Data', layer='SH_est_poly_utm')
syd <- spTransform(syd, CRS('+proj=utm +zone=56 +south +ellps=WGS84 +units=m +no_defs'))

plot(syd, axes=F)
plot(rls, pch=16, col='violetred',add=T)
title('Reef Life Survey Sites - Sydney Area', line=0.5)
library(GISTools)
map.scale(xc= 325000, yc= 6250000, len=2000, ndivs=2, units='kms')
north.arrow(xb= 325000, yb= 6258000, len=100)
box()
summary(rls_spdf)
summary(syd)

###Point in Polygon

# combine is.na() with over() to do the containment test; note that we
# need to "demote" parks to a SpatialPolygons object first
over(rls, as(syd, "SpatialPolygons")) #which sites are 'over' the polygon. Creates 
!is.na(over(rls, as(syd, "SpatialPolygons")))
inside_harb <- is.na(over(rls, as(syd, "SpatialPolygons")))

rls_harb <- rls[!inside_harb, ]
summary(rls_harb)
plot(syd)
plot(rls_harb, col='violetred', add=T, pch=18)
title('Reef Life Survey Sites - Sydney Harbour only', line=0.5)
map.scale(xc= 325000, yc= 6250000, len=2000, ndivs=2, units='kms')
north.arrow(xb= 325000, yb= 6258000, len=100)


####Polygon dissolve

london_sport <- readOGR(dsn="Data", layer="london_sport")
proj4string(london_sport) <- CRS("+init=epsg:27700")
london_sport$london <- rep(1, length(london_sport))
london <- unionSpatialPolygons(london_sport, IDs=london_sport$london)
plot(london)



####GGPLOT plotting of london sports
summary(london_sport)
library(ggplot2)
p <- ggplot(london_sport@data, aes(Partic_Per, Pop_2001))
p+geom_point()

##To get the shapefiles into a 
##format that can be plotted we have to use the fortify() function. 
##he “polygons” slot contains the geometry of the polygons in the form 
##of the XY coordinates used to draw the polygon outline. While ggplot is good, 
##there is still no way for it to work out what is going on there. i.e. we need to convert
##this geometry into a `normal` data.frame.

sport.f <- fortify(london_sport, region = "ons_label")
head(sport.f)

##need to merge back in the attribute data
sport.f <- merge(sport.f, london_sport@data, by.x = "id", by.y = "ons_label")
head(sport.f)


Map <- ggplot(sport.f, aes(long, lat, group = group, fill = Partic_Per)) + 
	geom_polygon() + 
    coord_equal() + 
    labs(x = "Easting (m)", y = "Northing (m)", fill = "% Sport Partic.") + 
    ggtitle("London Sports Participation")
Map

Map + scale_fill_gradient(low = "white", high = "black")


###faceting maps - courtesy of James Cheshire and spatial.ly
library(reshape2) 
london.data <- read.csv("Data/census-historic-population-borough.csv")
head(london.data)

london.data.melt <- melt(london.data, id = c("Area.Code", "Area.Name"))
london.data.melt[1:50,]

##remember what sport.f id was! How cool, now we can merge these data.
head(sport.f)
plot.data <- merge(sport.f, london.data.melt, by.x = "id", by.y = "Area.Code")

##order so plots are sensible i.e. in time order
plot.data <- plot.data[order(plot.data$order), ]

##now simply facet them in the usual ggplot way!
ggplot(data = plot.data, aes(x = long, y = lat, fill = value, group = group)) + 
    geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() + 
    facet_wrap(~variable)

###work example london

 library(plyr)
library(ggplot2)
library(maptools)

##read in the data
input<-read.table("Data/wu03ew_v1.csv", sep=",", header=T)

##select only the origin, destination and total kms from the data
input<- input[,1:3]
names(input)<- c("origin", "destination","total")

##download and input the population centroids from 
centroids <- read.csv("Data/msoa_popweightedcentroids.csv")
head(centroids)
head(input)
or.xy <- merge(input, centroids, by.x="origin", by.y="Code")

names(or.xy) <- c("origin", "destination", "trips", "o_name", "oX", "oY")
head(or.xy)
dest.xy <- merge(or.xy, centroids, by.x="destination", by.y="Code")
names(dest.xy) <- c("origin", "destination", "trips", "o_name", "oX", "oY","d_name", "dX", "dY")

##plotting in ggplot2
xquiet <- scale_x_continuous("", breaks=NULL)
yquiet <-scale_y_continuous("", breaks=NULL)
quiet <-list(xquiet, yquiet)

ggplot(dest.xy[which(dest.xy$trips>10),], aes(oX, oY))+
geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=trips), col="white")+
scale_alpha_continuous(range = c(0.03, 0.3))+
theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()



##Excercises One
airports <- read.csv('Data/airports.csv')
head(airports)
can <- airports[airports$Country=='Canada',]
coordinates(can) <- cbind(can$Longitude,can$Latitude)
proj4string(can) <- CRS('+proj=longlat +ellps=WGS84')

can_border <- readOGR(dsn='Data', layer='Canada')
can_border <- spTransform(can_border, CRS('+proj=longlat +ellps=WGS84'))

plot(can_border, col='wheat', axes=T)
plot(can, add=T, cex=0.8, col='red', pch=18)
title('Canadian airports')
box()



