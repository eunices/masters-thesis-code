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
shp <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp')


# Read lookup tables
lookup.cty <- read.csv('data/lookup/2019-05-29-statoid-country-codes.csv', encoding="UTF-8",
                       stringsAsFactors=F)
lookup.pri_div <- read.csv('data/lookup/2019-06-27-gadm-pri-div.csv', encoding="UTF-8",
                           stringsAsFactors=F)
lookup.pri_div$CTY.STATE.CODE <- paste0(lookup.pri_div$GEC, ".", lookup.pri_div$STATE_CODE)

# Read dataset
dir <- 'data/01-in/ascher-bee-data/'
files <- list.files(dir, full.names=T, pattern='csv')
files
