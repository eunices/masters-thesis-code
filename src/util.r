library(tidyverse)
library(sf)

dissolvePolygon = function(filepath) {
    # Merge IDN to country level as it is missing
    region = read_sf(filepath)
    region %>%
    st_set_geometry(NULL) %>%
    glimpse()

    dissolve =
    region %>%
    summarise()

    filename_out = paste0(gsub(".shp", "", filepath), "_dissolved.shp")

    st_write(dissolve, filename_out, driver="ESRI Shapefile")

}
