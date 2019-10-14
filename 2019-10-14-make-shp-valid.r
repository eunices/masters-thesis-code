library(lwgeom)
library(sf)

filepath_shp <- 'data/geo/0_manual/Ecoregions2017/fixing/invalid_geom_fix3.shp'

to_fix <-  read_sf(filepath_shp)
fixed <- st_make_valid(to_fix)
st_is_valid(fixed)

filename_out <- paste0(gsub(".shp", "", filepath_shp), "_fixed.shp")
st_write(fixed, filename_out, driver="ESRI Shapefile")


