# Convert raster to polygons

# https://gis.stackexchange.com/questions/192771/how-to-speed-up-raster-to-polygon-conversion-in-r

library(rgdal)

raster = paste0(folder_rast, "Figure1_svy21.tif")

r = raster(raster)