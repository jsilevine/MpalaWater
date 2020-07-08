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

##INSTRUCTIONS:
#Move to line 218 to Start, first part

## read data
# data <- read.csv("sightings.csv")
# data <- data[data$Y > 0,] ## get rid of funky 0s, not sure what thats about
buffer <- st_read("BufferedMpalaPolygon.shp") #mpala border with added buffer (.2 degrees)

## load in a basemap of Kenya
countries <- ne_countries(scale = 110)
Kenya <- countries[countries$name == "Kenya",]

## define coordinates
buffercoords <- buffer #already in a shp crs format

##Transformations
bufferobs <- st_as_sf(buffercoords)
bufferobs <- st_transform(bufferobs, 32637)
# coords <- data
# coordinates(coords) <- c(X ="X", Y="Y") ## convert to spatialPoints object
# proj4string(coords) <- CRS("+init=epsg:21097") ## define the projection: which is UTM 37N (the corresponding epsg is 21097)

# ## transform coords to WGS84 to plot with basemap
# coords <- spTransform(coords, proj4string(Kenya))
# obs <- st_as_sf(coords) ## convert to sf object
# obs <- st_transform(obs, 32637) ## transorm to UTM so we can work in meters instead of DDs
# st_bbox(obs)
## plot basemap and coordinates
plot(Kenya)
plot(bufferobs, add = TRUE)

## define function to create a regular grid
create_grid <- function(buffer, grid.size = 100, random = FALSE, offset.mean = NA, offset.sd = NA, what="polygons") { 
  
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
                       what = what, ## want output to be spatial object
                       square = TRUE ## could do hexagons ?
  )
  
  grid <- st_as_sf(grid)
  
  return(grid)
  
}

## --------- Processing ---------- ##

## Okay now to extract the observations based on the data
## we want one row per observation so we should do a left join of the grid to the observations.
## okay so next step is probably to write this all into a function that just takes the data locations
## as an argument and spits out a data frame like this, perhaps trimmed down to the relevant points
## Also need to add in the rest of the data!

##NEXT STEP
#function that takes in obseration data and runs against 3 rasters, creates new distances csv file. 

observation.data <- read.csv("file_name.csv") #input full observation data here (not nececssary above?)
riverraster <- raster("riverraster.tif")
roadraster <- raster("roadraster.tif")
waterraster <- raster("waterraster.tif")
sample_grid <- create_grid(bufferobs, grid.size = 400, random = TRUE, offset.mean = 10, offset.sd = 5, what="polygons")
## compared to the (264088.2 33723.97) to dam 1, the distance is 753.8003 using the grid method and it's 638.8 using qgis
##with the simple distance measuring tool is 638.96 to dam 1. zebra is in grid 533
#grid 533 = 263891 ymin: 33409.79 xmax: 264291 ymax: 33809.79 so four points = 
## corners of polygon: 8676403, 8676408, 8677268, 999.0729
#(264250.3 34342)

#Overall function to find distances between river, road, and water on grid
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

river_road_water_distances <- find_distances(data, riverraster, roadraster, waterraster, sample_grid) #returns as dataframe
write.csv(river_road_water_distances, "river_road_water.csv")
head(river_road_water_distances)
#Adding Dam IDs


#st_join(sample_grid_object, dam points, join=st_nearest_feature, left=TRUE)
#gives you nearest dam to every grid id. gives you all grid ids and feature of dam data set
#merge to bigger dataset with grid based on grid id
#Create a separate grid for dam IDs that allows us to get feautres of dam data set
dam_grid <- create_dam_grid(bufferobs, grid.size = 400, random = TRUE, offset.mean = 10, offset.sd = 5, what = "centers")
dam_grid$gridID = seq(1, nrow(dam_grid), by = 1)
dam_grid <- st_transform_proj(dam_grid, "+init=epsg:21097 +proj=utm +zone=37 +ellps=clrk80 +towgs84=-160,-6,-302,0,0,0,0 +units=m +no_defs")
water_sf <- st_as_sf(watercoords)
nearest_dam <- st_join(dam_grid, water_sf, join=st_nearest_feature, left=TRUE)
#Join distances added to dataframe above with the nearest dam data by gridID
river_road_waterID <- merge(river_road_water_distances, nearest_dam, by="gridID", all.x=TRUE)
#remove all geometries so we can transform into csv
river_road_waterID <- river_road_waterID[-c(23,24,44)]

##Adding water level info
damlevelsID <- read.csv("DamLevels_ID.csv")
river_road_waterID <- read.csv("river_road_water_dataID.csv")

#convert patrol dates to actual dates in R
river_road_waterID$Patrol.End.Date <- as.Date(river_road_waterID$Patrol.End.Date, format= "%d-%h-%y")
river_road_waterID$Patrol.Start.Date <- as.Date(river_road_waterID$Patrol.Start.Date, format= "%d-%h-%y")
colnames(river_road_waterID)[which(names(river_road_waterID) == "Number")] <- "DamID"

#Cleanup / Rename damlevelsID dataframe 
names(damlevelsID)[1] <- "DamID"
names(damlevelsID)[3] <- "2020-01-01"
names(damlevelsID)[4] <- "2020-02-01"
names(damlevelsID)[5] <- "2020-03-01"

#Add all water level information to dataframe
river_road_waterID_levels <- merge(river_road_waterID, damlevelsID, by="DamID", all.x=TRUE)

#create dates as a separate vector to compare
datevector <- as.Date(names(damlevelsID)[3:5])

#function to find water date closest to sighting date
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
#Added closeest Date to the dataframe
river_road_waterID_levels$closestDate <- sapply(river_road_waterID_levels$Patrol.Start.Date, findclosestDate)
#Determine the index of the date
indexes <- match(river_road_waterID_levels$closestDate, names(river_road_waterID_levels))

#Extract the value from that index
value <- function(river_road_waterID_levels) {
  z <- c()
  for (i in 1:length(indexes))
  z <- c(z,river_road_waterID_levels[i,(indexes)[i]])
  return(z)
}
#Add values to data frame
river_road_waterID_levels <- cbind(river_road_waterID_levels, value(river_road_waterID_levels))
#rename column of water levels
colnames(river_road_waterID_levels)[which(names(river_road_waterID_levels) == "value(river_road_waterID_levels)")] <- "closestValue"
write.csv(river_road_waterID_levels, "CompleteDataFrame.csv")

##Graphing Distances for Dan
## date = closestDate(patrolStartDate, three_dates)
## return row[date]

elliedata <- subset(nonsfjoined, Species==" Elephant",
                    +                   select=Patrol.ID:road.distance)
ggplot(nonsfjoined, aes(river.distance, water.distance, colour = Species)) + geom_point()

p <- ggplot(river_road_waterID_levels, aes(closestValue, water_distance, colour = Species)) + geom_point()
p + facet_wrap(vars(Species))




##Testing Distances
# 
# test <- st_distance(obs, waterobs, by_element = FALSE) # should get difference between each observation and each dam
# testdf <- as.data.frame(test)
# minimums <- apply(testdf, 1, min)
# minimums <- as.data.frame(minimums)
# minimums$index <- apply(testdf, 1, which.min)
# 
# grid533 <- subset(sample_grid, gridID == 533)
# grid200 <- subset(sample_grid, gridID == 200)
# grid100 <- subset(sample_grid, gridID == 100)
# three_grids <- subset(sample_grid, gridID == 533 | gridID == 200| gridID == 100)
# three_grids <- as(st_geometry(three_grids), "Spatial")
# grid533 <- as(st_geometry(grid533), "Spatial")
# grid100 <- as(st_geometry(grid100), "Spatial")
# grid200 <- as(st_geometry(grid200), "Spatial")
# grid533 <- as(grid533, "SpatialPolygonsDataFrame")
# grid100 <- as(grid100, "SpatialPolygonsDataFrame")
# grid200 <- as(grid200, "SpatialPolygonsDataFrame")
# three_grids <- as(three_grids, "SpatialPolygonsDataFrame")
# 
# writeOGR(obj=grid533, dsn="tempdir", layer="grid533", driver="ESRI Shapefile") # this is in geographical projection
# writeOGR(obj=grid100, dsn="tempdir", layer="grid100", driver="ESRI Shapefile") # this is in geographical projection
# writeOGR(obj=grid200, dsn="tempdir", layer="grid200", driver="ESRI Shapefile") # this is in geographical projection
# 
# gridd533distance <- extract(waterraster, grid533, method='simple', fun=mean)
# #min = 4239
# #max = 4702
# #range is around 462 meters. The distances match up with qgis woohoo!
# gridd533distance <- extract(waterraster, grid533, method='simple')
# ggridd100distance <- extract(waterraster, grid100, method='simple', fun=mean)
# gridd200distance <- extract(waterraster, grid200, method='simple', fun=mean)
# threegrid_distance <- extract(waterraster, three_grids, method='simple', fun=mean)
# threegrid_distance
# 
# 
# ##Water dam 