#Spatial Data in R
##Sydney Institute of Marine Science 10th August 2015


##Initial Steps

###Set up a project directory


![](Publication/project_dir_diagram.jpg)

###A brief primer on R classes

Remember that:


```r
x <- sum(c(2, 5, 7, 1))
x
```

```
## [1] 15
```

`x` is an object in R. It has a class, which represents the way the data in x is stored.


```r
class(x)
```

```
## [1] "numeric"
```
we can see that the class of `x` is `numeric`. 

Classes determine what actions functions are going to have on the object. These functions, for example, include `print, summary, plot` etc. If no method is avialable for a specific class then it will try and default to known generic methods.

When R was first being created, there were no real functions. They were introduced in 1992 in a form called 'S3' classes, or 'old-style classes'.

Think back to the classic data set shipped with R, `mtcars'.


```r
data(mtcars)
class(mtcars)
```

```
## [1] "data.frame"
```

```r
typeof(mtcars)
```

```
## [1] "list"
```

```r
names(mtcars)
```

```
##  [1] "mpg"  "cyl"  "disp" "hp"   "drat"
##  [6] "wt"   "qsec" "vs"   "am"   "gear"
## [11] "carb"
```


mtcars is a `data.frame` that is stored as a `list` (this is why things like `lapply` will work on `data.frames`).

Old style classes are generally `lists` with attributes and not defined formally. In 1998 new style `S4` style classes were introduced. The main definition between old and new style classes that that the new S4 classes have clear, formal, definitions that can specify the type and storage structure for each of the components, called *slots*.

An understanding of these classes is important, as most spatial data in R uses new style, *S4* classes.


###R and spatial 

Different spatial classes in R *inherit* their attributes from other classes.

#### 1. `Spatial class`
![](Publication/spatial_class_box.jpg)


```r
library(sp)
```


```r
getClass("Spatial")
```

```
## Error in eval(expr, envir, enclos): could not find function "getClass"
```


```r
m <- matrix(c(0, 0, 1, 1), ncol = 2, dimnames = list(NULL, 
    c("min", "max")))
print(m)
```

```
##      min max
## [1,]   0   1
## [2,]   0   1
```

```r
crs <- CRS(projargs = as.character(NA))
print(crs)
```

```
## CRS arguments: NA
```

```r
S <- Spatial(bbox = m, proj4string = crs)
S
```

```
## An object of class "Spatial"
## Slot "bbox":
##      min max
## [1,]   0   1
## [2,]   0   1
## 
## Slot "proj4string":
## CRS arguments: NA
```

* We first creat a bounding box by making a 2 x 3 matrix.
* Create a CRS object (a specific object defined in the package `sp`). Give it no arguments.
* Create our first `Spatial` object, calling it `S`. We can see it has a `bbox` and a CRS (called a proj4string).


####2. `Spatial Points`

![](Publication/spatial_points_class_box.jpg)

Let's start using some real data!

The data below is from the OpenFlights project. OpenFlights is a tool that lets you map your flights around the world, search and filter them in all sorts of interesting ways, calculate statistics automatically, and share your flights and trips with friends and the entire world (if you wish). It's also the name of the open-source project to build the tool.

They have put their data (and code) on a public GitHub repo. Download and load into R directly.


```r
airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", 
    header = FALSE)
```


```r
colnames(airports) <- c("ID", "name", "city", 
    "country", "IATA_FAA", "ICAO", "lat", 
    "lon", "altitude", "timezone", "DST", 
    "TimeZone")
head(airports)
```

```
##   ID                       name
## 1  1                     Goroka
## 2  2                     Madang
## 3  3                Mount Hagen
## 4  4                     Nadzab
## 5  5 Port Moresby Jacksons Intl
## 6  6                 Wewak Intl
##           city          country
## 1       Goroka Papua New Guinea
## 2       Madang Papua New Guinea
## 3  Mount Hagen Papua New Guinea
## 4       Nadzab Papua New Guinea
## 5 Port Moresby Papua New Guinea
## 6        Wewak Papua New Guinea
##   IATA_FAA ICAO       lat      lon
## 1      GKA AYGA -6.081689 145.3919
## 2      MAG AYMD -5.207083 145.7887
## 3      HGU AYMH -5.826789 144.2959
## 4      LAE AYNZ -6.569828 146.7262
## 5      POM AYPY -9.443383 147.2200
## 6      WWK AYWK -3.583828 143.6692
##   altitude timezone DST
## 1     5282       10   U
## 2       20       10   U
## 3     5388       10   U
## 4      239       10   U
## 5      146       10   U
## 6       19       10   U
##               TimeZone
## 1 Pacific/Port_Moresby
## 2 Pacific/Port_Moresby
## 3 Pacific/Port_Moresby
## 4 Pacific/Port_Moresby
## 5 Pacific/Port_Moresby
## 6 Pacific/Port_Moresby
```

The only difference between the `Spatial` and `SpatialPoints` class is the addition of the `coords` *slot*.


```r
air_coords <- cbind(airports$lon, airports$lat)
air_CRS <- CRS("+proj=longlat +ellps=WHS84")

airports_sp <- SpatialPoints(air_coords, air_CRS)
summary(airports_sp)
```

```
## Object of class SpatialPoints
## Coordinates:
##                min       max
## coords.x1 -179.877 179.95100
## coords.x2  -90.000  82.51778
## Is projected: FALSE 
## proj4string : [+proj=longlat +ellps=WHS84]
## Number of points: 8107
```


####Methods

#####Bounding Box
Note that we did not specify a Bounding Box even though it is an integral component of the `Spatial` class. A bounding box is automatically assigned by taking the minimum and maximum latitude and longitude of the data.



```r
bbox(airports_sp)
```

```
##                min       max
## coords.x1 -179.877 179.95100
## coords.x2  -90.000  82.51778
```


#####Projection and datum

All spatial objects have a generic method `proj4string`. This sets the coordinate reference of the associated spatial data. Proj4 strings are a compact, but lightly quirky looking, way of specifying location references. Using the Proj4 syntax, the complete parameters to specify a CRS can be given. For example the PROJ4 string we usually use in Sydney is *+proj=utm +zone=56 +south +ellps=GRS80 +units=m +no_defs*. Here we essentially state that the CRS we are using is UTM (a projected CRS) in Zone 56. Most commonly we can start with an unprojected CRS (simply the longitute and latitude) using the CRS in the code above *+proj=longlat +ellps=WHS84*.

Proj4 strings can be assigned and re-assigned. NOTE that re-assigning a CRS does does NOT transform it. Simply saying the proj=utm will not convert the coordinates from long/lat into UTM.


```r
proj4string(airports_sp) <- CRS(as.character(NA))
summary(airports_sp)
```

```
## Object of class SpatialPoints
## Coordinates:
##                min       max
## coords.x1 -179.877 179.95100
## coords.x2  -90.000  82.51778
## Is projected: NA 
## proj4string : [NA]
## Number of points: 8107
```

```r
proj4string(airports_sp) <- air_CRS
```


#####Subsetting 
Subsetting SpatialPoints can be accomplished in the same manner as most other subsetting in R - using logical vectors.


```r
airports_oz <- which(airports$country=='Australia')
head(airports_oz)
```

```
## [1] 1928 2047 2101 2194 2214 2313
```

```r
oz_coords <- coordinates(airports_sp)[airports_oz,]
head(oz_coords)
```

```
##      coords.x1 coords.x2
## [1,]   151.488  -32.7033
## [2,]   149.611  -32.5625
## [3,]   148.755  -20.2760
## [4,]   150.832  -32.0372
## [5,]   151.342  -32.7875
## [6,]   128.307  -17.5453
```

You can also use the subsetting methods directly on the SpatialPoints.


```r
summary(airports_sp[airports_oz,])
```

```
## Object of class SpatialPoints
## Coordinates:
##                  min      max
## coords.x1 -153.01667 159.0770
## coords.x2  -42.83611  28.3835
## Is projected: FALSE 
## proj4string : [+proj=longlat +ellps=WHS84]
## Number of points: 263
```

####3. `SpatialPointsDataFrame`

Often we need to associate attribute data to simple point locations. The `SpatialPointsDataFrame` is the container for this sort of data.

So far we downloaded a `data.frame`, where 2 columns were latitude and longitude, and constructed a simple SpatialPoints object from it. 

A `SpatialPointsDataFrame` simply connects the SpatialPoints with the rest of the attribute data. 

A key consideration here is that the `row.names` of both the matrix of coordinates and the data need to match. 


```r
airports_spdf <- SpatialPointsDataFrame(coords=airports_sp, data=airports, proj4string=airCRS)
summary(airports_spdf)
```

```
## Object of class SpatialPointsDataFrame
## Coordinates:
##                min       max
## coords.x1 -179.877 179.95100
## coords.x2  -90.000  82.51778
## Is projected: FALSE 
## proj4string : [+proj=longlat +ellps=WHS84]
## Number of points: 8107
## Data attributes:
##        ID                    name     
##  Min.   :   1   North Sea      :  16  
##  1st Qu.:2092   All Airports   :  10  
##  Median :4257   Railway Station:   9  
##  Mean   :4766   Train Station  :   8  
##  3rd Qu.:7508   Vilamendhoo    :   6  
##  Max.   :9541   Bus            :   5  
##                 (Other)        :8053  
##         city               country    
##  London   :  21   United States:1697  
##  New York :  13   Canada       : 435  
##  Hong Kong:  12   Germany      : 321  
##  Berlin   :  10   Australia    : 263  
##  Paris    :  10   Russia       : 249  
##  Chicago  :   9   France       : 233  
##  (Other)  :8032   (Other)      :4909  
##     IATA_FAA        ICAO      
##         :2227   \\N    :1258  
##  BFT    :   2          :  63  
##  ZYA    :   2   EDDB   :   2  
##  %u0    :   1   OIIE   :   2  
##  04G    :   1   RPVM   :   2  
##  06A    :   1   UATE   :   2  
##  (Other):5873   (Other):6778  
##       lat               lon          
##  Min.   :-90.000   Min.   :-179.877  
##  1st Qu.:  8.825   1st Qu.: -79.022  
##  Median : 34.988   Median :   5.292  
##  Mean   : 26.818   Mean   :  -3.922  
##  3rd Qu.: 47.958   3rd Qu.:  49.786  
##  Max.   : 82.518   Max.   : 179.951  
##                                      
##     altitude          timezone       
##  Min.   :-1266.0   Min.   :-12.0000  
##  1st Qu.:   38.0   1st Qu.: -5.0000  
##  Median :  272.0   Median :  1.0000  
##  Mean   :  933.4   Mean   :  0.1692  
##  3rd Qu.: 1020.0   3rd Qu.:  4.0000  
##  Max.   :14472.0   Max.   : 13.0000  
##                                      
##  DST                     TimeZone   
##  A:2037   America/New_York   : 628  
##  E:1852   America/Chicago    : 373  
##  N:1364   Europe/Berlin      : 319  
##  O: 214   America/Anchorage  : 258  
##  S: 391   Europe/Paris       : 232  
##  U:2195   America/Los_Angeles: 226  
##  Z:  54   (Other)            :6071
```
















