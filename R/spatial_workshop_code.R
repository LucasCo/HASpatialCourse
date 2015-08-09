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
table(rls_df$SiteCode)

syd_df <- rls_df[grep('SYD',rls_df$SiteCode),]
summary(syd_df)
rls_sites <- 










