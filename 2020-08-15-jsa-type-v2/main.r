# Information about code:
# This code corresponds to the main script for data cleaning and wrangling for my MSc thesis.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# To use this script: 
# setwd("C:/_dev/msc/thesis/")
# source('2020-08-15-jsa-type-v2/main.r')

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO CLEANING SCRIPTS FOR BEE TYPE DATA V2")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# Set up
start <- proc.time()
dir_script_td <- "2020-08-15-jsa-type-v2/"

# Cleaning/ data wrangling scripts
source(paste0(dir_script_td, 'init/init.R'))    # initialise libraries, util, variables
source(paste0(dir_script_td, 'prep/map.r'))     # map variables
source(paste0(dir_script_td, 'clean/clean.r'))  # cleaning
source(paste0(dir_script_td, 'df/df.r'))        # derived datasets
rm(list=ls())

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("BEE TYPE DATA CLEANING COMPLETE (V2).")
print(paste0("Cleaning took ", proc.time()-start))
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
