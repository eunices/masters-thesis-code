# Libraries
library(raster)
library(maptools)


# Params
directory = 'data/geo'
map_crs = '+proj=longlat +datum=WGS84'

# Initialize
data(wrld_simpl)


################ Reading different file types ################

# Read geotif
file <- paste0(directory, '/0_manual/anthropogenic_biomes/anthromes-v2-2000-global-geotif/a2000_global.tif')
x1 <- raster::raster(file)

file <- paste0(directory, '/0_manual/spam/dataverse_files/spam2010v1r0_global_harv_area.geotiff/spam2010v1r0_global_harvested-area_acof_a.tif')
x5 <- raster::raster(file)
stack(x1, x5)


# Read dat
file <- paste0(directory, '/0_manual/soil_param/ISRIC_W_GRID_546/ISRIC_W_GRID_546/data/wise_awc.dat')
x3 <- raster::raster(file)
raster::projection(x3) <- map_crs
x3 <- raster::resample(x3, x1, method='ngb')
stack(x1,x3)
plot(x3)


# Read asc
file_dir <- paste0(directory, '/0_manual/soil_param2/ISLSCP_SOILS_1DEG_1004/ISLSCP_SOILS_1DEG_1004/data')
files <- list.files(file_dir, pattern="asc", full.names=TRUE)
x4 <- raster::raster(files[1])
raster::projection(x4) <- map_crs
x4 <- raster::resample(x4, x1, method='ngb')
stack(x1, x4)
plot(x4)






# Reproject raster
# raster::reprojectRaster
