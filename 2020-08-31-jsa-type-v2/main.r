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
source(paste0(dir_script_td, 'init/init.R'))    # initialise vars/ libs
source(paste0(dir_script_td, 'prep/prep.r'))    # map variables/ format file
source(paste0(dir_script_td, 'clean/clean.r'))  # cleaning
source(paste0(dir_script_td, 'df/df.r'))        # derived datasets
end <- Sys.time()
print(paste0(Sys.time(), " --- Cleaning complete. Cleaning took ", 
             round( (end-start) / 60, 2) , " mins"))

rm(list=ls())
