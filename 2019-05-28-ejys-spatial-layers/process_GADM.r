# setwd('C:/Dev/msc-thesis-code')
# setwd('mnt/c/Dev/msc-thesis-code')

library(tidyverse)
library(sf)

# Merge IDN to country level as it is missing
region = read_sf("data/geo/gadm/shp_sec/gadm36_IDN_1.shp")
region %>%
  st_set_geometry(NULL) %>%
  glimpse()

region$area = st_area(region)

ind =
  region %>%
  summarise(NAME_0="Indonesia", GID_0="IDN")

st_write(ind, "data/geo/gadm/shp_pri/gadm36_IDN_0.shp", driver="ESRI Shapefile")