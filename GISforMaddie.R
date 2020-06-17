
library(ggplot2)
library(rgdal)
library(rnaturalearth)
library(sf)


## read data
data <- read.csv("GIS.csv")
data <- data[data$Y > 0,] ## get rid of funky 0s, not sure what thats about


## load in a basemap of Kenya
countries <- ne_countries(scale = 110)
Kenya <- countries[countries$name == "Kenya",]


## define coordinates
coords <- data
coordinates(coords) <- c(X ="X", Y="Y") ## convert to spatialPoints object
proj4string(coords) <- CRS("+init=epsg:21097") ## define the projection: which is UTM 37N (the corresponding epsg is 21097)


## transform coords to WGS84 to plot with basemap
coords <- spTransform(coords, proj4string(Kenya))


## plot basemap and coordinates
plot(Kenya)
plot(coords, add = TRUE)


## --------- Processing ---------- ##


## the package sf has some useful functions so lets convert points to sf object
obs <- st_as_sf(coords) ## convert to sf object
obs <- st_transform(obs, 32637) ## transorm to UTM so we can work in meters instead of DDs
st_bbox(obs)


## define function to create a regular grid
create_grid <- function(obs, grid.size = 100, random = FALSE, offset.mean = NA, offset.sd = NA) {

  ## error checking
  if (random == TRUE & any(is.na(c(offset.mean, offset.sd)))) {
    print("Error: must supply mean and sd for random offset")
    return()
  }

  ## if random offset desired, create offset
  if (random == TRUE) {

    lower.left <- st_bbox(obs)[c("xmin", "ymin")] ## get lower left extent of obs
    offset <- lower.left - abs(rnorm(2, offset.mean, offset.sd))

  }

  else offset <- st_bbox(obs)[c("xmin", "ymin")]

  ## create grid object
  grid <- st_make_grid(x = obs,
                       cellsize = grid.size,
                       offset = offset,
                       what = "polygons", ## want output to be spatial object
                       square = TRUE ## could do hexagons ?
                       )

  grid <- st_as_sf(grid)

  return(grid)

}


sample_grid <- create_grid(obs, grid.size = 400, random = TRUE, offset.mean = 10, offset.sd = 5)
plot(sample_grid)
plot(obs, add = TRUE) ## seems to work!

## Okay now to extract the observations based on the data
## we want one row per observation so we should do a left join of the grid to the observations.

## first add a grid ID datum
sample_grid$gridID = seq(1, nrow(sample_grid), by = 1)

joined.data <- st_join(obs, sample_grid, join = st_within, left = TRUE)
str(joined.data) ## looks like that worked too!

## okay so next step is probably to write this all into a function that just takes the data locations
## as an argument and spits out a data frame like this, perhaps trimmed down to the relevant points
## Also need to add in the rest of the data!


## calculate raster of distances: example

## okay so this is one way I found, but not sure if its the best or easiest

## first we need to create a grid object like the one above, go with a larger cell size for testing purposes
raster_grid <- st_make_grid(sample_grid, cellsize = 20, what = "centers") ## this is going to be really big, Q is probably better
                                                                                   ## at processing large files, but its annoying me so...

## now we calculate the distances
combined_obs <- st_combine(obs) ## this step is important, otherwise it will calculate distance between each grid center and each obs
distance_vals <- st_distance(combined_obs, raster_grid) ## this is going to take a while
length(distance_vals) == length(raster_grid) ## check to make sure length is correct
head(distance_vals) ## seems like at least reasonable numbers

## attach distance and spatial data to a data.frame. In all honesty I don't know why
## I have to do it this way, but its the only way I can get it to work so I'm not gonna question
coords <- st_coordinates(raster_grid)
distance_data <- data.frame(distance = as.vector(distance_vals), coords)

dist_sf <- st_as_sf(distance_data, coords = c("X", "Y"), crs = "+init=epsg:21097")

r <- raster(resolution = 20, extent(sample_grid), crs = "+init=epsg:21097") ## create an empty raster layer with same spatial extent as the grid
res(r) ## check resolution, should be 3m x 3m


final.raster <- rasterize(dist_sf, r, "distance", fun = mean) ## convert to raster object
plot(final.raster)

## seems to work, you will want to do this once you have created a grid based on the tracks to ensure full coverage.
## alternatively, if you have a layer that is the mpala boundary, create the initial grid based on that.


## now calculate distance to 
