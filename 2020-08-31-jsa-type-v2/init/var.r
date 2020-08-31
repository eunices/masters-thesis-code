# Variables
v2_dir_data <- 'data/2019-05-23-ascher-bee-data/'

updated_version <- "v2" # data version

# Output data
v2_dir_data_analysis <- paste0(v2_dir_data, "analysis/", updated_version, "/")
v2_dir_data_ch1 <- paste0(v2_dir_data_analysis, 'ch1/')
v2_dir_data_ch2 <- paste0(v2_dir_data_analysis, 'ch2/')
v2_dir_data_ch3_coauth <- paste0(v2_dir_data_analysis, 'ch3-coauth/')
v2_dir_data_ch3_flow <- paste0(v2_dir_data_analysis, 'ch3-flow/')
v2_dir_data_ch3_gender <- paste0(v2_dir_data_analysis, 'ch3-gender/')

# Raw data
v2_dir_data_raw <- paste0(v2_dir_data, updated_version, "/")
v2_dir_data_raw_raw <- paste0(v2_dir_data_raw, "raw/")
v2_dir_data_raw_clean <- paste0(v2_dir_data_raw, "clean/")

# Script folder
v2_dir_ref <- '2019-06-19-jsa-type'
v2_dir_base <- paste0('2020-08-31-jsa-type-', updated_version)
v2_dir_script <- paste0(v2_dir_ref, '/')
v2_dir_shiny <- paste0(v2_dir_ref, '-shiny/')
v2_dir_ch1 <- paste0(v2_dir_ref, '-ch1/')
v2_dir_ch2 <- paste0(v2_dir_ref, '-ch2/')
v2_dir_ch3a <- paste0(v2_dir_ref, '-ch3-coauth/')
v2_dir_ch3b <- paste0(v2_dir_ref, '-ch3-flow/')
v2_dir_ch3c <- paste0(v2_dir_ref, '-ch3-gender/')

# Base file folder
v2_basefile <- '2020-08-31-Apoidea'

# Other 
source('keys.R')
source(paste0(v2_dir_script, 'init/util.r'))
source(paste0(v2_dir_script, 'init/libraries.r'))

# Initialize google api for geocoding
register_google(key = geocode_api)

# If data dir does not exist, create it
data_dirs = c(v2_dir_data, v2_dir_data_analysis, v2_dir_data_ch1,
              v2_dir_data_ch2, v2_dir_data_ch3_coauth, v2_dir_data_ch3_flow, 
              v2_dir_data_ch3_gender,
              v2_dir_data_raw, v2_dir_data_raw_raw, v2_dir_data_raw_clean)
lapply(data_dirs, function(folder) {
  if(!dir.exists(folder)) dir.create(folder)
})