
dir_data_subf1 = paste0(v2_dir_data_ch3_gender, "time-series-spp/")
dir_data_subf2 = paste0(v2_dir_data_ch3_gender, "time-series-tax/")

if(!dir.exists(v2_dir_data_ch3_gender)) dir.create(v2_dir_data_ch3_gender)
if(!dir.exists(dir_data_subf1)) dir.create(dir_data_subf1)
if(!dir.exists(dir_data_subf2)) dir.create(dir_data_subf2)

# Read/initialize all data/variables

CURRENT_YEAR <- 2019
theme <- theme_minimal()
