library(ggplot2)
library(rgdal)
library(rnaturalearth)
library(sf)

## read data
data <- read.csv("sightings.csv")
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
obs <- st_as_sf(coords) ## convert to sf object (geospatial analysis package in r)
obs <- st_transform(obs, 32637) ## transorm to UTM so we can work in meters instead of DDs
st_bbox(obs)

## define function to create a regular grid
create_grid <- function(obs, grid.size = 100, random = FALSE, offset.mean = NA, offset.sd = NA) { #supply the observations and then placement of grid, etc. The function takes extent of observations and places grid on top of it. Later we might want to change it so that we are taking tracks as input instead of observation. 

  ## error checking
  if (random == TRUE & any(is.na(c(offset.mean, offset.sd)))) {
    print("Error: must supply mean and sd for random offset")
    return()
  }

  ## if random offset desired, create offset - rnadomize the location of the gri.d the sf function takes the lower left hand corner as a starting point. This code generates a point near a lower left hand corner to start the grid
  if (random == TRUE) {

    lower.left <- st_bbox(obs)[c("xmin", "ymin")] ## get lower left extent of obs (the full range)
    offset <- lower.left - abs(rnorm(2, offset.mean, offset.sd)) #usbtracts a random number in lower lh corner to start grid

  }
  else offset <- st_bbox(obs)[c("xmin", "ymin")] #if grid isn't random then just give offset as actual lower left hadn corner

  ## create grid object
  grid <- st_make_grid(x = obs, #from sf packaage and it creates a grid given grid size, offset, 
                       cellsize = grid.size,
                       offset = offset,
                       what = "polygons", ## want output to be spatial object
                       square = FALSE ## could do hexagons ?
                       )
  grid <- st_as_sf(grid) #converts to sf object again

  return(grid)

}


sample_grid <- create_grid(obs, grid.size = 400, random = FALSE, offset.mean = 10, offset.sd = 5)
plot(sample_grid)
plot(obs, add = TRUE) ## seems to work!

## Okay now to extract the observations based on the data. we want output table to be a list of observations and then to indicte what grid each obs occurred in (later we will want to now distance columns), 
## we want one row per observation so we should do a left join of the grid to the observations.

## first add a grid ID datum
sample_grid$gridID = seq(1, nrow(sample_grid), by = 1)

joined.data <- st_join(obs, sample_grid, join = st_within, left = TRUE)
str(joined.data) ## looks like that worked too!

## okay so next step is probably to write this all into a function that just takes the data locations
## as an argument and spits out a data frame like this, perhaps trimmed down to the relevant points
## Also need to add in the rest of the data!

#create raster versions of the nearest neighbors at pretty small resolutino (3mx3m) at each one calculate distance to road. Tool = proximity which prints out raster field of distance to specific objects. Then put it into R with an average value. So each grid will have a value for distance to road or water. Not involving our grid at all. You create your own raster of small respolution that you keep constant. Then you can recaluclate with grid size. 
# ideally we are taking everything in R
# doing it as a raster, yo uonly have to do it once, b/c "this point is in this grid and this raster is this distance away"
# try doing this with the other data e.g. dams. include something in make grid that calculates average values of those for each grid point on the new grid. and then attach to obs data
# package calld raster in r. grid defined as spatail object, you should be able to iteratively extrac the values of the raster to the grid
