# To use this script: 
# setwd("C:/_dev/msc/thesis/")
# source('2020-08-31-jsa-type-v2/main.r')

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO CLEANING SCRIPTS FOR BEE TYPE DATA V2")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# Set up
dir_script_td <- "2020-08-31-jsa-type-v2/"

# Cleaning/ data wrangling scripts
start <- Sys.time()
source(paste0(dir_script_td, '00-init/main.r'))    # initialise vars/ libs
source(paste0(dir_script_td, '01-prep/main.r'))    # map variables/ format file
source(paste0(dir_script_td, '03-clean/main.r'))   # cleaning
source(paste0(dir_script_td, '04-df/main.r'))      # derived datasets
end <- Sys.time()

print(paste0(
    Sys.time(), " --- Cleaning complete. Cleaning took ", 
    round( (end-start) / 60, 2) , " mins"
))

rm(list=ls())
