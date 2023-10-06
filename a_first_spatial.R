## Clear the workspace
rm(list=ls())
## Install packages, only if needed

install.packages("maps")
install.packages("maptools")
install.packages("sp")
install.packages("sf")
install.packages("spdep")
install.packages("gstat")
install.packages("splancs")
install.packages("spatstat")
install.packages("lattice")
install.packages("pgirmess")
install.packages("RColorBrewer")
install.packages("classInt")
install.packages("spgwr")

## Load spatial packages, among others
library(maps)         ## Projections
library(maptools)     ## Data management
library(sp)           ## Data management
library(spdep)        ## Spatial autocorrelation
library(gstat)        ## Geostatistics
library(splancs)      ## Kernel Density
library(spatstat)     ## Geostatistics
library(pgirmess)     ## Spatial autocorrelation
library(RColorBrewer) ## Visualization
library(classInt)     ## Class intervals
library(spgwr)        ## GWR

## Set working directory

setwd("C:\\Users\\Jonathan\\Desktop\\SistemasExpertos\\Geoesta_MMOP") #in my case, this path
load("Datasets2.RData")
####################################################
##########         SECTION 1              ##########

#########################################
#### Point Data: Baltimore Crime     ####
#########################################

## Explore data
head(crime)
dim(crime)
data <- crime

## Create matrix of coordinates
sp_point <- cbind(data$LONG, data$LAT)
colnames(sp_point) <- c("LONG","LAT")
head(sp_point)

## Projection: UTM Zone 17
proj <- CRS("+proj=utm +zone=17 +datum=WGS84")

## Create spatial object
data.sp <- SpatialPointsDataFrame(coords=sp_point,data,proj4string=proj)

## Bounding box of data points
bbox(data.sp)

## Plot crime locations
par(mar=c(2,2,0.2,0.2))
plot(data.sp,pch=16, cex=.5, axes=T)
dev.off()


#########################################
#### Polygon Data: 2004 Election     ####
#########################################
## Lambert Conformal Conic Projection

## proj4string(data) <- CRS("+proj=lcc+lon_0=90w +lat_1=20n +lat_2=60n")
## Plot counties + Baltimore crime locations

par(mar=rep(0.5,4))
#Don't run the following:
plot(election,col="beige")
plot(data.sp,pch=1, cex=.5,add=T, col="blue")
dev.off()

plot(election["WHITE"]) #visulaizar de manera individual las columnas
######
## Plotting Attributes
######
## Look at some of the options
par(mar=c(0,3,0,0),cex=.6)
display.brewer.all(n=5)
dev.off()

## Create blue-state red-state palette

br.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
br.palette(5)
## Let's plot the % of vote for Bush
data <- election
var <- data$Bush_pct

cols <- ifelse(election$Bush > election$Kerry,"red","blue")
plot(st_geometry(election),col=cols,border=NA)

br.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
colos<-ifelse(data$Bush_pct>50,br.palette(2)[1],br.palette(2)[2])
plot(st_geometry(data),  col=colos, main="Percent of County Vote for Bush (2004)")
dev.off()

plot(st_geometry(data))
plot(st_geometry(data),  col=br.palette(10), main="Percent of County Vote for Bush (2004)")
dev.off()

plot(st_geometry(st_centroid(data)), pch = 3, col = 'red', add = TRUE)

# red republican  <- George Bush
# blue democrat 
data$Bush_pct2 = cut(data$Bush_pct, 10)
plot(data["Bush_pct2"], axes = TRUE, key.pos = 4, pal = sf.colors(10), key.width = lcm(5))

#What if I set the breaks
plot(data["Bush_pct"], breaks = c(0,52.72,61.17,69.36,100))

#Now using a fancier library: ggplot2 and ggspatial
library(ggplot2)
library(ggspatial)
ggplot(data = election) +
  #annotation_map_tile("stamenwatercolor") +
  geom_sf(aes(fill = Bush_pct), alpha = 0.8) +
  #annotation_scale() +
  scale_fill_viridis_c(option="B",direction = -1) + #Play with the "option" in this funtion
  ggtitle(label = "% de votos por Bush", subtitle = "Eleccion 2004")


## Harder but more flexible option:
## Define number of colors in a palette
pal <- br.palette(n=5)
## Fixed intervals
classes_fx <- classIntervals(var, n=5, style="fixed", fixedBreaks=c(0, 10, 25, 50, 75, 100), rtimes = 1)
classes_sd <- classIntervals(var, n=5, style = "sd", rtimes = 1)
classes_fi <- classIntervals(var, n=5, style = "fisher", rtimes = 3)
classes_eq <- classIntervals(var, n=5, style = "equal", rtimes = 1)
classes_km <- classIntervals(var, n=5, style = "kmeans", rtimes = 1)
classes_qt <- classIntervals(var, n=5, style = "quantile", rtimes = 1)


## Compare classes
par(mar=c(2,2,2,1)+0.1, mfrow=c(2,3))
plot(classes_fx, pal=pal, main="Fixed Intervals", xlab="", ylab="")
plot(classes_sd, pal=pal, main="Standard Deviation", xlab="", ylab="")
plot(classes_fi, pal=pal, main="Fisher-Jenks", xlab="", ylab="")
plot(classes_km, pal=pal, main="K Means", xlab="", ylab="")
plot(classes_eq, pal=pal, main="Equal Interval", xlab="", ylab="")
plot(classes_qt, pal=pal, main="Quantile", xlab="", ylab="")
dev.off()

#########################################
#### Grid Data: Maunga Whau Volcano  ####
#########################################

head(volcano)[,1:6]          ## Take a look at the data structure
dim(volcano)            ## 87 x 61 grid of elevation points

z <- volcano            ## Height Variable
x <- 10*(1:nrow(z))     ## 10 Meter Spacing (S-N)
y <- 10*(1:ncol(z))     ## 10 Meter Spacing (E-W)

## Contour Plot
par(mar=rep(0.5,4))               
contour(x, y, z, levels=seq(from=min(z), to=max(z), by=10),axes=F)
dev.off()

## Gradient
par(mar=rep(0.5,4))               
image(x, y, z, col=terrain.colors(100), axes=F)
dev.off()

## Gradient + Countour
par(mar=rep(0.5,4))               
image(x, y, z, col=terrain.colors(100), axes=F)
contour(x, y, z, levels=seq(from=min(z), to=max(z), by=10),axes=F, add=T)
dev.off()

z <- 2 * volcano # Exaggerate the relief
x <- 10 * (1:nrow(z)) # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z)) # 10 meter spacing (E to W)
## Don't draw the grid lines : border = NA
par(bg = "white")
persp(x, y, z, theta = 135, phi = 30, col="green", scale = FALSE,
      ltheta = -120, shade = 0.75, border = NA, box = FALSE)

## 3-D Plot
par(mar=rep(0,4))               
persp(x,y,z,theta=120,phi=15,scale=F,axes=F)
dev.off()

## 3-D Elevation Plot w/ color
z <- 2 * volcano        ## Exaggerate the relief
x <- 10 * (1:nrow(z))   ## 10 meter spacing (S to N) 
y <- 10 * (1:ncol(z))   ## 10 meter spacing (E to W) 

## Create new grid
z0 <- min(z) - 20 
z <- rbind(z0, cbind(z0, z, z0), z0) 
x <- c(min(x) - 1e-10, x, max(x) + 1e-10) 
y <- c(min(y) - 1e-10, y, max(y) + 1e-10) 

## Create matrix of base colors
fcol <- matrix("green3", nr = nrow(z)-1, nc = ncol(z)-1) 
fcol[ , i2 <- c(1,ncol(fcol))] <- "gray" 
fcol[i1 <- c(1,nrow(fcol)) , ] <- "gray" 


## Take average of four neighboring values for palette
zi <- (volcano[ -1,-1] + volcano[ -1,-61] + volcano[-87,-1] + volcano[-87,-61])/4
pal <- terrain.colors(20)[cut(zi, quantile(zi, seq(0,1, len = 21)), include.lowest = TRUE)]
fcol[-i1,-i2] <- pal

## Plot it
par(mar=rep(0,4))
persp(x, y, z, theta=120, phi=15, col = fcol, scale = FALSE, shade = 0.4, border = NA) 
dev.off()

#########################################
#### Data Management                 ####
#########################################
## What if you have some date you'd like to merge with a map?

#Also, see the .rmd file where the CONEVAL example was developed

## Let's open a world map Shapefile
map<-read_sf("P:/Geoesta/Estadistica-Espacial-main/CodeData/world.shp")

#The following one is obsolet
#map <- readShapePoly("P:/Geoesta/Estadistica-Espacial-main/world",IDvar="MAP_CCODE",proj4string=CRS("+proj=eqc +lon_0=90w"))          ## Equidistant Cylindrical
summary(map)
st_geometry(data) #This function is pretty useful

## Plot the study region
par(mar=rep(0,4))
plot(st_geometry(map))
dev.off()

## Open POLITY IV dataset
polity <- read.csv("P:/Geoesta/Estadistica-Espacial-main/CodeData/polity.csv")
names(polity)

#Now merging information from a non-spatial file to a spacial one
m_ccode <- as.data.frame(map)       
map2 <- merge(x=map, y=polity, by.x="CCODE", by.y="ccode", all.x=T, all.y=F)

## Recode Polity variable
map2$polity <- ifelse(map2$polity==-66,NA,map2$polity)
map2$polity <- ifelse(map2$polity==-77,NA,map2$polity)
map2$polity <- ifelse(map2$polity==-88,NA,map2$polity)

## Plot POLITY scores
map2$polity2 = cut(map2$polity, 10)
plot(map2["polity2"], axes = TRUE, key.pos = 4, pal = sf.colors(10), key.width = lcm(5))
dev.off()       
