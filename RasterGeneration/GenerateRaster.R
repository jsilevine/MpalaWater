library(ggplot2)
library(rgdal)
library(rnaturalearth)
library(sf)
library(raster)
library(ggplot2)
library(stars)
library(maptools)
library(data.table)
library(birk)

##Import Data
riverdata <- read.csv("RiverPoints_21097.csv") #points on the river
roaddata <- read.csv("MpalaRoadPoints.csv") #points on the road (maybe we should do these as lines?)
damdata <- read.csv("MpalaWaterPoints.csv") #dam points (should we include the river access points here?)
damdata <- damdata[complete.cases(damdata), ] #removing lass row


proj4string(dam_grid) <- CRS("+init=epsg:21097 +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
##define coordinates for other cases
rivercoords <- riverdata
coordinates(rivercoords) <- c(X = "xcoord", Y="ycoord")
proj4string(rivercoords) <- CRS("+init=epsg:4326")

roadcoords <- roaddata #need to convert 4326 to 21097
coordinates(roadcoords) <- c(X = "xcoord", Y="ycoord")
proj4string(roadcoords) <- CRS("+init=epsg:4326")

watercoords <- damdata
coordinates(watercoords) <- c(X = "xcoord", Y="ycoord")
proj4string(watercoords) <- CRS("+init=epsg:21097")

buffercoords <- buffer #already in a shp crs format

##Transformations
roadobs <- st_transform(st_as_sf(roadcoords), 32637) ## convert to sf object
riverobs <- st_transform(st_as_sf(rivercoords), 32637) ## convert to sf object
waterobs <- st_transform(st_as_sf(watercoords), 32637) ## convert to sf object
bufferobs <- st_transform(st_as_sf(buffercoords), 32637)

## first we need to create a grid object like the one above, go with a larger cell size for testing purposes
raster_grid <- st_make_grid(sample_grid, cellsize = 20, what = "centers") ## this is going to be really big, Q is probably better
## at processing large files, but its annoying me so...
#use the create grid output from above to make raster. it calculates points, then distance from points to object, then turn to raster
#make raster dataset
build_raster <- function(rastergrid = raster_grid, obsdata, coorddata) {
  combinedobs <- st_combine(obsdata)  ## this step removes individual dam data. this step is important, otherwise it will calculate distance between each grid center and each obs (single geomoetry object. all water should be treated )
  distancevals <- st_distance(combinedobs, rastergrid) ## this is going to take a while. dsitance between 2 arguments (combined geo and raster grid)-
  #length(distance_vals) == length(raster_grid) ## check to make sure length is correct
  #head(distance_vals) ## seems like at least reasonable numbers
  coorddata <- st_coordinates(rastergrid) ## attach distance and spatial data to a data.frame. In all honesty I don't know why
  ## I have to do it this way, but its the only way I can get it to work so I'm not gonna question
  distancedata <- data.frame(distance = as.vector(distancevals), coorddata)
  distsf <- st_as_sf(distancedata, coords = c("X", "Y"), crs = "+init=epsg:21097")
  r <- raster(resolution = 20, extent(sample_grid), crs = "+init=epsg:21097") ## create an empty raster layer with same spatial extent as the grid
  #res(r) ## check resolution, should be 3m x 3m
  return(rasterize(distsf, r, "distance", fun = mean)) ## convert to raster object
}
final.river.raster <- build_raster(raster_grid, riverobs, rivercoords)
final.water.raster <- build_raster(raster_grid, waterobs, watercoords)
final.road.raster <- build_raster(raster_grid, roadobs, roadcoords)

##save the rasters
rf <- writeRaster(final.road.raster, "roadraster.grd", datatype='INT4S', overwrite=TRUE)
if (require(rgdal)) {
  rf <- writeRaster(final.road.raster, "roadraster.tif", format="GTiff", overwrite=TRUE)
  bf <- writeRaster(final.road.raster, "roadraster", 
                    options="INTERLEAVE=BAND", overwrite=TRUE)
}

## seems to work, you will want to do this once you have created a grid based on the tracks to ensure full coverage.
## alternatively, if you have a layer that is the mpala boundary, create the initial grid based on that.

#-> convert to a dataframe, but the output is different than what we have before. 

#average value of the grid for the raster (maybe extract by mass?) and use the sf stuff. 
#and then do the same join thing but instaed of just grid id being attached, 
#you'll have grid id, distance to road and to water

#extract mean of raster values from given raster file according to observation coordinates (this should be from grid coordinates not, observation coordinates)

obsroaddistance <- extract(final.road.raster, joined.data, method='simple', fun=mean)
#turn joined data into dataframe instead of spatial object
nonsfjoined <- st_set_geometry(joined.data, NULL)

#add column to dataframe with water distance
nonsfjoined$road.distance=obsroaddistance
head(nonsfjoined) #check work
