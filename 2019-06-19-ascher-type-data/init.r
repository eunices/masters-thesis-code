# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Initializing
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- INITIALIZING"))
source('C:/Dev/msc-thesis-code/2019-06-19-ascher-type-data/var.r')
setwd(wd)

# Read shapefile
# shp2 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo.shp')
# shp2$GID_DIV_FINAL <- ifelse(is.na(shp2$GID_2), as.character(shp2$GID_1), as.character(shp2$GID_2))
# shp2$GID_NAME_FINAL <- ifelse(is.na(shp2$NAME_2), as.character(shp2$NAME_1), as.character(shp2$NAME_2))
# shp2 <- sf::st_write(shp2, 'data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp', driver="ESRI Shapefile", delete_layer=T)
if(!exists("shp")) {
    shp <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp', quiet=T)
}
if(!exists("shp2")) {
    shp2 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp', quiet=T)
}
if(!exists("shp3")){
    shp3 <- shp2
    st_geometry(shp3) <- NULL
    # write.csv(shp3, 'data/lookup/2019-07-12-gadm-countries-biogeo.csv')
}
if(!exists("shp4")){
    shp4 <- sf::st_read('data/geo_processed/gadm/gadm36_0_utf8_continents.shp', quiet=T)
}
if(!exists("shp5")){
    shp5 <- shp4
    st_geometry(shp5) <- NULL
    # write.csv(shp5, 'data/lookup/2019-07-15-gadm-countries-continent.csv')
}

# shp2 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt.shp')
# shp2$GID_DIV_FINAL <- ifelse(is.na(shp2$GID_2), as.character(shp2$GID_1), as.character(shp2$GID_2))
# shp2$GID_NAME_FINAL <- ifelse(is.na(shp2$NAME_2), as.character(shp2$NAME_1), as.character(shp2$NAME_2))
# sf::st_write(shp2, 'data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt2.shp', driver="ESRI Shapefile", delete_layer=T)

if(!exists("shp6")) {
    shp6 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt2.shp', quiet=T)
}
if(!exists("shp7")){
    shp7 <- shp6
    st_geometry(shp7) <- NULL
    # write.csv(shp7, 'data/lookup/2019-07-15-gadm-countries-biogeo-holt.csv')
}
if(!exists("shp8")) {
    shp8 <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_1.shp', quiet=T)
}
# Read lookup tables
lookup.cty <- read.csv('data/lookup/2019-05-29-statoid-country-codes.csv', encoding="UTF-8",
                       stringsAsFactors=F, na=c(""))
names(lookup.cty)[1] <- "Country"

lookup.pri_div <- read.csv('data/lookup/2019-06-27-gadm-pri-div.csv', encoding="UTF-8",
                           stringsAsFactors=F, na=c(""))
lookup.pri_div$CTY.STATE.CODE <- paste0(lookup.pri_div$GEC, ".", lookup.pri_div$STATE_CODE)

lookup.loc <- read.csv('data/lookup/2019-09-26-location-codes.csv', encoding="UTF-8",
                           stringsAsFactors=F, na=c(""))

