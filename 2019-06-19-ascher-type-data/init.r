# Libraries
library(data.table)
library(ggplot2)
library(sf)
library(anytime)
library(ggmap)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Initializing
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Initializing setup"))

# Setup
source('keys.R')


# Initialize google api for geocoding
register_google(key = geocode_api)


# Read shapefile
# shp2 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo.shp')
# shp2$GID_DIV_FINAL <- ifelse(is.na(shp2$GID_2), as.character(shp2$GID_1), as.character(shp2$GID_2))
# shp2$GID_NAME_FINAL <- ifelse(is.na(shp2$NAME_2), as.character(shp2$NAME_1), as.character(shp2$NAME_2))
# shp2 <- sf::st_write(shp2, 'data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp', driver="ESRI Shapefile", delete_layer=T)
shp <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp')
shp2 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp')
shp3 <- shp2
st_geometry(shp3) <- NULL
write.csv(shp3, 'data/lookup/2019-07-12-gadm-countries-biogeo.csv')

shp4 <- sf::st_read('data/geo_processed/gadm/gadm36_0_utf8_continents.shp')
shp5 <- shp4
st_geometry(shp5) <- NULL
write.csv(shp5, 'data/lookup/2019-07-15-gadm-countries-continent.csv')

# shp2 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt.shp')
# shp2$GID_DIV_FINAL <- ifelse(is.na(shp2$GID_2), as.character(shp2$GID_1), as.character(shp2$GID_2))
# shp2$GID_NAME_FINAL <- ifelse(is.na(shp2$NAME_2), as.character(shp2$NAME_1), as.character(shp2$NAME_2))
# sf::st_write(shp2, 'data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt2.shp', driver="ESRI Shapefile", delete_layer=T)
shp6 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt2.shp')
shp7 <- shp6
st_geometry(shp7) <- NULL
write.csv(shp7, 'data/lookup/2019-07-15-gadm-countries-biogeo-holt.csv')

# Read lookup tables
lookup.cty <- read.csv('data/lookup/2019-05-29-statoid-country-codes.csv', encoding="UTF-8",
                       stringsAsFactors=F)
names(lookup.cty)[1] <- "Country"
lookup.pri_div <- read.csv('data/lookup/2019-06-27-gadm-pri-div.csv', encoding="UTF-8",
                           stringsAsFactors=F)
lookup.pri_div$CTY.STATE.CODE <- paste0(lookup.pri_div$GEC, ".", lookup.pri_div$STATE_CODE)

# Read dataset
dir <- 'data/01-in/ascher-bee-data/'
files <- list.files(dir, full.names=T, pattern='csv')
files
