library(tidyverse)
library(sf); library(rgeos)

dissolvePolygon = function(filepath, by="None") {
    region = read_sf(filepath)
    # region = st_buffer(region, dist = 0) # to resolve TopologyException: input geom invalid
    
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


install_libs <- function() {
    library_files <- list.files('.', pattern="libraries.r", recursive=T)
    lib_li <- c()
    for (i in 1:length(library_files)) {
        libs <- gsub("suppressMessages\\(library\\(|\\)\\)", "", readLines(library_files[1])[-1])
        lib_li <- c(lib_li, libs)
    }
    all_packages <- unique(lib_li)
    packages_to_install <- all_packages[!(all_packages %in% installed.packages()[,"Package"])]
    if(length(packages_to_install)) install.packages(packages_to_install)
}
