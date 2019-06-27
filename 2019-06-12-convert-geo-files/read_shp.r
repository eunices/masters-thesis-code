# Libraries
library(raster)
library(maptools)


# Params
directory = 'data/geo'
map_crs = '+proj=longlat +datum=WGS84'

# Initialize
data(wrld_simpl)


################ Geospatial functions ################




# Read master grid
file <- paste0(directory, '/0_manual/anthropogenic_biomes/anthromes-v2-2000-global-geotif/a2000_global.tif')
x1 <- raster::raster(file)



# Read shp 1
file <- c('/0_manual/soil_map/DSMW/DSMW.shp')
x2 <- raster::shapefile(paste0(directory, file[1]))
raster::projection(x2) <- map_crs
plot(x2)


# Rasterize shp
file <- paste0(directory, '/0_manual/plant_wcmc/Centres_of_Plant_Diversity_2013/Centres_of_Plant_Diversity_2013/CPD_2013.shp')
x4 <- raster::shapefile(file)

rst <- raster(matrix(runif(20), 5, 4))
extent(rst) <- extent(-180, 180, -90, 90)
res(rst) <- 0.5 # 1 deg cell size

rasterized <- raster::rasterize(x4, rst)
# attribute_t <- levels(rasterized)[[1]]
head(levels(rasterized)[[1]]); dim(levels(rasterized)[[1]])
head(unique(levels(rasterized)[[1]])); unique(dim(levels(rasterized)[[1]]))
# rasterized <- raster::ratify(rasterized) # reset raster attribute table


# Resample to same extent/resolution/grid (for categorical)
x5 <- raster::resample(rasterized, x1, method='ngb')
par(mfrow=c(2,1)); plot(rasterized, col=rev(topo.colors(50))); plot(x3, col=rev(heat.colors(50)))

plot(x3)
plot(wrld_simpl, add=T)

raster::stack(x1, x5) # now it is able to stack 


# Resample to same extent/resolution/grid (for continuous)
# aggregate (fun=mean) -> resample
# TODO:




# Tutorial on vector
# https://cengel.github.io/rspatial/2_spDataTypes.nb.html

# sp package
library(sp)
ln <- sp::Line(matrix(runif(6), ncol=2))
str(ln)

lns <- sp::Lines(list(ln), ID = "a") # this contains just one Line!
str(lns)

sp_lns <- sp::SpatialLines(list(lns))
str(sp_lns)

dfr <- data.frame(id = "a", use = "road", cars_per_hour = 10) # note how we use the ID from above!
sp_lns_dfr <- sp::SpatialLinesDataFrame(sp_lns, dfr, match.ID = "id")
str(sp_lns_dfr)

# sf package
library(sf)
lnstr_sfg <- sf::st_linestring(matrix(runif(6), ncol=2)) 
class(lnstr_sfg)

(lnstr_sfc <- sf::st_sfc(lnstr_sfg)) # just one feature here
class(lnstr_sfc) 

(lnstr_sf <- st_sf(dfr, lnstr_sfc))
class(lnstr_sf)



# Test
folders <- list.files(directory)
folder_dir <- paste0(directory, "/", folders[1])
list.files(folder_dir)