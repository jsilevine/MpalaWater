
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


sample_grid <- create_grid(obs, grid.size = 400, random = FALSE, offset.mean = 10, offset.sd = 5)
plot(sample_grid)
plot(obs, add = TRUE) ## seems to work!

## Okay now to extract the observations based on the data
## we want one row per observation so we should do a left join of the grid to the observations.

## first add a grid ID datum
sample_grid$gridID = seq(1, nrow(sample_grid), by = 1)

joined.data <- st_join(obs, sample_grid, join = st_within, left = TRUE)
str(joined_data) ## looks like that worked too!

## okay so next step is probably to write this all into a function that just takes the data locations
## as an argument and spits out a data frame like this, perhaps trimmed down to the relevant points
## Also need to add in the rest of the data!
