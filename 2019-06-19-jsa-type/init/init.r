# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Initializing
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# print(paste0(Sys.time(), " --- initializing with init/init.R"))
source('2019-06-19-jsa-type/init/var.R')
setwd(working_dir)

# Set environment variables
if (Sys.getenv("JAVA_HOME") == "") {
    Sys.setenv(JAVA_HOME=java_home)
}

# Prior modification to shp files
# shp2 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo.shp')
# shp2$GID_DIV_FINAL <- ifelse(is.na(shp2$GID_2), as.character(shp2$GID_1), as.character(shp2$GID_2))
# shp2$GID_NAME_FINAL <- ifelse(is.na(shp2$NAME_2), as.character(shp2$NAME_1), as.character(shp2$NAME_2))
# shp2 <- sf::st_write(shp2, 'data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp', driver="ESRI Shapefile", delete_layer=T)


# Read shapefile
# note: Most of the shp files are not read at the moment:
if(!exists("shp")) {
    # Country shp file
    shp <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp', quiet=T)
}
if(!exists("shp2")) {
    # Country pri div w/ biogeograhic regions (WWF) shp file
    # shp2 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp', quiet=T)
}
if(!exists("shp4")){
    # Country w/ continents shp file
    # shp4 <- sf::st_read('data/geo_processed/gadm/gadm36_0_utf8_continents.shp', quiet=T)
    # shp5 <- shp4;  st_geometry(shp5) <- NULL
}
if(!exists("shp6")) {
    # Country pri div w/ biogeograhic regions (Holt) shp file
    # shp6 <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt2.shp',    
    #                     quiet=T)
    # shp7 <- shp6; st_geometry(shp7) <- NULL
}
if(!exists("shp8")) {
    # Country pri div shp file
    shp8 <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_1.shp', quiet=T)
}


# Read lookup tables
lookup.cty <- read.csv('data/lookup/2019-05-29-statoid-country-codes.csv', 
                       encoding="UTF-8",
                       stringsAsFactors=F, na=c(""))
names(lookup.cty)[1] <- "Country"

lookup.pri_div <- read.csv('data/lookup/2019-06-27-gadm-pri-div.csv', 
                           encoding="UTF-8",
                           stringsAsFactors=F, na=c(""))
lookup.pri_div$CTY.STATE.CODE <- paste0(lookup.pri_div$GEC, ".", lookup.pri_div$STATE_CODE)

lookup.loc <- read.csv('data/lookup/2019-09-26-location-codes.csv', encoding="UTF-8",
                           stringsAsFactors=F, na=c(""))

