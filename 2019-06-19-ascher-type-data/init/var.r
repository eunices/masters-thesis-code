# Variables
dir_data <- 'data/2019-05-23-ascher-bee-data/'
dir_script <- '2019-06-19-ascher-type-data/'
dir_shiny <- '2019-06-19-ascher-type-data-shiny/'
basefile <- '2019-05-23-Apoidea world consensus file Sorted by name 2019'
source('keys.R')
source(paste0(dir_script, 'init/helper.r'))
source(paste0(dir_script, 'init/libraries.r'))

# Initialize google api for geocoding
register_google(key = geocode_api)