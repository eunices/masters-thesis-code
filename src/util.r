library(tidyverse)
library(sf)

dissolvePolygon = function(filepath, by="None") {
    region = read_sf(filepath)

    region %>%
        st_set_geometry(NULL) %>%
        glimpse()

    if (by=="None") {
        dissolve =
            region %>%
            summarise()
    } else {
        dissolve =
            region %>% 
            group_by(eval(by)) %>%
            summarise()
    }

    filename_out = paste0(gsub(".shp", "", filepath), "_dissolved.shp")

    st_write(dissolve, filename_out, driver="ESRI Shapefile")

}
