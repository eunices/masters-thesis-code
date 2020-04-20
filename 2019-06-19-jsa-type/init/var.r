# Variables
dir_data <- 'data/2019-05-23-ascher-bee-data/'

dir_base <- '2019-06-19-jsa-type'
dir_script <- paste0(dir_base, '/')
dir_shiny <- paste0(dir_base, '-shiny/')
dir_ch1 <- paste0(dir_base, '-ch1/')
dir_ch2 <- paste0(dir_base, '-ch2/')
dir_ch3a <- paste0(dir_base, '-ch3-coauth/')
dir_ch3b <- paste0(dir_base, '-ch3-flow/')
dir_ch3c <- paste0(dir_base, '-ch3-gender/')

basefile <- '2019-05-23-Apoidea world consensus file Sorted by name 2019'
source('keys.R')
source(paste0(dir_script, 'init/helper.r'))
source(paste0(dir_script, 'init/libraries.r'))

# Initialize google api for geocoding
register_google(key = geocode_api)