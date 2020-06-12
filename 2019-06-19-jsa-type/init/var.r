# Variables
dir_data <- 'data/2019-05-23-ascher-bee-data/'

version = "v2" # data version
# v1 on 2019-05-23

# Output data
dir_data_analysis <- paste0(dir_data, "analysis/", version, "/")
dir_data_ch1 <- paste0(dir_data_analysis, 'ch1/')
dir_data_ch2 <- paste0(dir_data_analysis, 'ch2/')
dir_data_ch3_coauth <- paste0(dir_data_analysis, 'ch3-coauth/')
dir_data_ch3_flow <- paste0(dir_data_analysis, 'ch3-flow/')
dir_data_ch3_gender <- paste0(dir_data_analysis, 'ch3-gender/')

# Raw data
dir_data_raw <- paste0(dir_data, version, "/")
dir_data_raw_clean <- paste0(dir_data_raw, "clean/")

# Script folder
dir_base <- '2019-06-19-jsa-type'
dir_script <- paste0(dir_base, '/')
dir_shiny <- paste0(dir_base, '-shiny/')
dir_ch1 <- paste0(dir_base, '-ch1/')
dir_ch2 <- paste0(dir_base, '-ch2/')
dir_ch3a <- paste0(dir_base, '-ch3-coauth/')
dir_ch3b <- paste0(dir_base, '-ch3-flow/')
dir_ch3c <- paste0(dir_base, '-ch3-gender/')

# Base file folder
basefile <- '2019-05-23-Apoidea world consensus file Sorted by name 2019'

# Other 
source('keys.R')
source(paste0(dir_script, 'init/util.r'))
source(paste0(dir_script, 'init/libraries.r'))

# Initialize google api for geocoding
register_google(key = geocode_api)

# If data dir does not exist, create it
data_dirs = c(dir_data, dir_data_analysis, dir_data_ch1,
              dir_data_ch2, dir_data_ch3_coauth, dir_data_ch3_flow, dir_data_ch3_gender,
              dir_data_raw, dir_data_raw_clean)
lapply(data_dirs, function(folder) {
  if(!dir.exists(folder)) dir.create(folder)
})