# Variables
dir_data <- 'data/2019-05-23-ascher-bee-data/'

updated_version = "v2" # data version

# Output data
v2_dir_data_analysis <- paste0(dir_data, "analysis/", updated_version, "/")
v2_dir_data_ch1 <- paste0(dir_data_analysis, 'ch1/')
v2_dir_data_ch2 <- paste0(dir_data_analysis, 'ch2/')
v2_dir_data_ch3_coauth <- paste0(dir_data_analysis, 'ch3-coauth/')
v2_dir_data_ch3_flow <- paste0(dir_data_analysis, 'ch3-flow/')
v2_dir_data_ch3_gender <- paste0(dir_data_analysis, 'ch3-gender/')

# Raw data
v2_dir_data_raw <- paste0(dir_data, updated_version, "/")
v2_dir_data_raw_clean <- paste0(dir_data_raw, "clean/")

# Script folder
v2_dir_ref <- '2019-06-19-jsa-type/'
v2_dir_base <- paste0('2020-08-15-jsa-type-', updated_version)
v2_dir_script <- paste0(dir_base, '/')
v2_dir_shiny <- paste0(dir_base, '-shiny/')
v2_dir_ch1 <- paste0(dir_base, '-ch1/')
v2_dir_ch2 <- paste0(dir_base, '-ch2/')
v2_dir_ch3a <- paste0(dir_base, '-ch3-coauth/')
v2_dir_ch3b <- paste0(dir_base, '-ch3-flow/')
v2_dir_ch3c <- paste0(dir_base, '-ch3-gender/')

# Base file folder
v2_basefile <- '2020-08-15-Apoidea world consensus file Sorted by name 2020'

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