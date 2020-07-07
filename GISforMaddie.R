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
## read data
data <- read.csv("sightings.csv")
data <- data[data$Y > 0,] ## get rid of funky 0s, not sure what thats about
buffer <- st_read("BufferedMpalaPolygon.shp") #mpala border with added buffer (.2 degrees)

## load in a basemap of Kenya
countries <- ne_countries(scale = 110)
Kenya <- countries[countries$name == "Kenya",]

## define coordinates
buffercoords <- buffer #already in a shp crs format

##Transformations
bufferobs <- st_as_sf(buffercoords)
bufferobs <- st_transform(bufferobs, 32637)

coords <- data
coordinates(coords) <- c(X ="X", Y="Y") ## convert to spatialPoints object
proj4string(coords) <- CRS("+init=epsg:21097") ## define the projection: which is UTM 37N (the corresponding epsg is 21097)
## transform coords to WGS84 to plot with basemap
coords <- spTransform(coords, proj4string(Kenya))
## plot basemap and coordinates
plot(Kenya)
plot(bufferobs, add = TRUE)
plot(bufferobs)
## --------- Processing ---------- ##


## the package sf has some useful functions so lets convert points to sf object
obs <- st_as_sf(coords) ## convert to sf object
obs <- st_transform(obs, 32637) ## transorm to UTM so we can work in meters instead of DDs
st_bbox(obs)


## define function to create a regular grid
create_grid <- function(buffer, grid.size = 100, random = FALSE, offset.mean = NA, offset.sd = NA) { 

  ## error checking
  if (random == TRUE & any(is.na(c(offset.mean, offset.sd)))) {
    print("Error: must supply mean and sd for random offset")
    return()
  }

  ## if random offset desired, create offset
  if (random == TRUE) {

    lower.left <- st_bbox(buffer)[c("xmin", "ymin")] ## get lower left extent of obs
    offset <- lower.left - abs(rnorm(2, offset.mean, offset.sd))

  }

  else offset <- st_bbox(buffer)[c("xmin", "ymin")]

  ## create grid object
  grid <- st_make_grid(x = buffer,
                       cellsize = grid.size,
                       offset = offset,
                       what = "polygons", ## want output to be spatial object
                       square = TRUE ## could do hexagons ?
                       )

  grid <- st_as_sf(grid)

  return(grid)

}


create_dam_grid <- function(buffer, grid.size = 100, random = FALSE, offset.mean = NA, offset.sd = NA) { 

  ## error checking
  if (random == TRUE & any(is.na(c(offset.mean, offset.sd)))) {
    print("Error: must supply mean and sd for random offset")
    return()
  }

  ## if random offset desired, create offset
  if (random == TRUE) {

    lower.left <- st_bbox(buffer)[c("xmin", "ymin")] ## get lower left extent of obs
    offset <- lower.left - abs(rnorm(2, offset.mean, offset.sd))

  }

  else offset <- st_bbox(buffer)[c("xmin", "ymin")]

  ## create grid object
  grid <- st_make_grid(x = buffer,
                       cellsize = grid.size,
                       offset = offset,
                       what = "centers", ## want output to be spatial object
                       square = TRUE ## could do hexagons ?
                       )

  grid <- st_as_sf(grid)

  return(grid)

}

sample_grid <- create_grid(bufferobs, grid.size = 400, random = TRUE, offset.mean = 10, offset.sd = 5)

plot(obs, add = TRUE) ## seems to work!

## Okay now to extract the observations based on the data
## we want one row per observation so we should do a left join of the grid to the observations.

## first add a grid ID datum
sample_grid$gridID = seq(1, nrow(sample_grid), by = 1)
grid_data <- as.data.frame(sample_grid)                                                       
joined.data <- st_join(obs, sample_grid, join = st_within, left = TRUE)

## okay so next step is probably to write this all into a function that just takes the data locations
## as an argument and spits out a data frame like this, perhaps trimmed down to the relevant points
## Also need to add in the rest of the data!


## calculate raster of distances: example
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


##NEXT STEP
#function that takes in obseration data and runs against 3 rasters, creates new distances csv file. 

observation.data <- read.csv("file_name")
riverraster <- raster("riverraster.tif")
roadraster <- raster("roadraster.tif")
waterraster <- raster("waterraster.tif")
sample_grid <- create_grid(bufferobs, grid.size = 400, random = TRUE, offset.mean = 10, offset.sd = 5)
## compared to the (264088.2 33723.97) to dam 1, the distance is 753.8003 using the grid method and it's 638.8 using qgis
##with the simple distance measuring tool is 638.96 to dam 1. zebra is in grid 533
#grid 533 = 263891 ymin: 33409.79 xmax: 264291 ymax: 33809.79 so four points = 
## corners of polygon: 8676403, 8676408, 8677268, 999.0729
#(264250.3 34342)

#Overall function once distance is fixed. 
find_distances <- function(observationdata, riverraster, roadraster, waterraster, sample_grid) {
observationdata <- observationdata[observationdata$Y > 0,] ## get rid of funky 0s, not sure what thats about
 
  ## define coordinates
  coords <- observationdata
  coordinates(coords) <- c(X ="X", Y="Y") ## convert to spatialPoints object
  proj4string(coords) <- CRS("+init=epsg:21097")
  
  obs <- st_as_sf(coords) ## convert to sf object
  obs <- st_transform(obs, 32637) ## transorm to UTM so we can work in meters instead of DDs
  
  sample_grid$gridID = seq(1, nrow(sample_grid), by = 1)
  grid_data <- as.data.frame(sample_grid)                                                       
  joined.data <- st_join(obs, sample_grid, join = st_within, left = TRUE)
  joined.data <- as.data.frame(joined.data)
  merged.data <- merge(joined.data, grid_data, by="gridID")
  point_names <- subset(merged.data, select=geometry)
  merged.data$geometry = NULL
  colnames(merged.data)[which(names(merged.data) == "x")] <- "geometry"
  observation_polygons <- st_as_sf(merged.data)
  observation_polygons$obs.points = point_names

  obsriverdistance <- extract(riverraster, observation_polygons, method='simple', fun=mean)
  obsroaddistance <- extract(roadraster, observation_polygons, method='simple', fun=mean)
  obswaterdistance <- extract(waterraster, observation_polygons, method='simple', fun=mean)

    
  observation_polygons_df <- as.data.frame(observation_polygons)
  #add column to dataframe with water distance
  observation_polygons_df$river_distance=obsriverdistance
  observation_polygons_df$road_distance=obsroaddistance
  observation_polygons_df$water_distance=obswaterdistance
  
  return(observation_polygons_df)
}
river_road_water_distances <- find_distances(data, riverraster, roadraster, waterraster, sample_grid)
write.csv(river_road_water_distances, "river_road_water.csv")
head(river_road_water_distances)
#Adding Dam IDs
test2 <- read.csv("river_road_water.csv")
#st_join(sample_grid_object, dam points, join=st_nearest_feature, left=TRUE)
#gives you nearest dam to every grid id. gives you all grid ids and feature of dam data set
#merge to bigger dataset with grid based on grid id
#we might have to create a separate grid object with centers as points (what = centers) and make sure grid id's are equal
dam_grid <- create_dam_grid(bufferobs, grid.size = 400, random = TRUE, offset.mean = 10, offset.sd = 5)
dam_grid$gridID = seq(1, nrow(dam_grid), by = 1)
dam_grid <- st_transform_proj(dam_grid, "+init=epsg:21097 +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
water_sf <- st_as_sf(watercoords)
nearest_dam <- st_join(dam_grid, water_sf, join=st_nearest_feature, left=TRUE)
river_road_waterID <- merge(river_road_water_distances, nearest_dam, by="gridID", all.x=TRUE)
#remove all geometries
newdf <- river_road_waterID[-c(23,24,44)]
write.csv(newdf, "river_road_water_dataID.csv")


##Adding date info
damlevelsID <- read.csv("DamLevels_ID.csv")
sampledate <- c("21-Feb-20", "22-Feb-20")
as.Date(sampledate, format= "%d-%h-%y")

newdf$Patrol.End.Date <- as.Date(newdf$Patrol.End.Date, format= "%d-%h-%y")
newdf$Patrol.Start.Date <- as.Date(newdf$Patrol.Start.Date, format= "%d-%h-%y")

names(damlevelsID)[1] <- "DamID"
names(damlevelsID)[3] <- "2020-01-01"
names(damlevelsID)[4] <- "2020-02-01"
names(damlevelsID)[5] <- "2020-03-01"
colnames(newdf)[which(names(newdf) == "Number")] <- "DamID"

river_road_waterID_levels <- merge(newdf, damlevelsID, by="DamID", all.x=TRUE)

datevector <- as.Date(names(damlevelsID)[3:5])

findclosestDate <- function(row) {
  # row = [... '2020-01-01', ... 2]
  # index_of_patrol_start_date = river_road_waterID_levels.getColumnIndex('Patrol Start Date')
  # patrol_start_date = row[index_of_patrol_start_date]
  
  # row {... 'Period Start Date': '2020-01-01', ... '2020-01-01': 2}
  index <- which.closest(datevector, row)
  date <- datevector[index]
  
  column_name <- format(date, "%Y-%m-%d")
  return(column_name)
}

findclosestValue <- function(row2) {
  indexes <- match(river_road_waterID_levels$closestDateValue, names(river_road_waterID_levels))
  
}

river_road_waterID_levels$closestDate <- sapply(river_road_waterID_levels$Patrol.Start.Date, findclosestDate)

sapply(row.names(river_road_waterID_levels), function(x) river_road_waterID_levels[[x, x]])

indexes <- match(river_road_waterID_levels$closestDateValue, names(river_road_waterID_levels))

value <- function(river_road_waterID_levels) {
  z <- c()
  for (i in 1:length(indexes))
  z <- c(z,river_road_waterID_levels[i,(indexes)[i]])
  return(z)
}

river_road_waterID_levels <- cbind(river_road_waterID_levels, value(river_road_waterID_levels))
names(river_road_waterID_levels)[47] <- "ClosestValue"


match(river_road_waterID_levels$closestDateValue, names(river_road_waterID_levels))


river_road_waterID_levels$closestValue <- 

newdf$closestDate <- sapply(river_road_waterID_levels, findclosestDate(river_road_waterID_levels))
##Graphing Distances for Dan
## date = closestDate(patrolStartDate, three_dates)
## return row[date]

elliedata <- subset(nonsfjoined, Species==" Elephant",
                    +                   select=Patrol.ID:road.distance)
ggplot(nonsfjoined, aes(river.distance, water.distance, colour = Species)) + geom_point()

p <- ggplot(river_road_water, aes(Water.Depth..m., water_distance, colour = Species)) + geom_point()
p + facet_wrap(vars(Species))
q <- ggplot(nonsfjoined, aes(road.distance, water.distance, colour = Species)) + geom_point()
q + facet_wrap(vars(Species))
r <- ggplot(nonsfjoined, aes(river.distance, water.distance, colour = Species)) + geom_point()
r + facet_wrap(vars(Species))

##Testing Distances

test <- st_distance(obs, waterobs, by_element = FALSE) # should get difference between each observation and each dam
testdf <- as.data.frame(test)
minimums <- apply(testdf, 1, min)
minimums <- as.data.frame(minimums)
minimums$index <- apply(testdf, 1, which.min)

grid533 <- subset(sample_grid, gridID == 533)
grid200 <- subset(sample_grid, gridID == 200)
grid100 <- subset(sample_grid, gridID == 100)
three_grids <- subset(sample_grid, gridID == 533 | gridID == 200| gridID == 100)
three_grids <- as(st_geometry(three_grids), "Spatial")
grid533 <- as(st_geometry(grid533), "Spatial")
grid100 <- as(st_geometry(grid100), "Spatial")
grid200 <- as(st_geometry(grid200), "Spatial")
grid533 <- as(grid533, "SpatialPolygonsDataFrame")
grid100 <- as(grid100, "SpatialPolygonsDataFrame")
grid200 <- as(grid200, "SpatialPolygonsDataFrame")
three_grids <- as(three_grids, "SpatialPolygonsDataFrame")

writeOGR(obj=grid533, dsn="tempdir", layer="grid533", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=grid100, dsn="tempdir", layer="grid100", driver="ESRI Shapefile") # this is in geographical projection
writeOGR(obj=grid200, dsn="tempdir", layer="grid200", driver="ESRI Shapefile") # this is in geographical projection

gridd533distance <- extract(waterraster, grid533, method='simple', fun=mean)
#min = 4239
#max = 4702
#range is around 462 meters. The distances match up with qgis woohoo!
gridd533distance <- extract(waterraster, grid533, method='simple')
ggridd100distance <- extract(waterraster, grid100, method='simple', fun=mean)
gridd200distance <- extract(waterraster, grid200, method='simple', fun=mean)
threegrid_distance <- extract(waterraster, three_grids, method='simple', fun=mean)
threegrid_distance


##Water dam 