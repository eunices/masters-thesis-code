# Information about code:
# This code corresponds to the main script for data cleaning and wrangling for my MSc thesis.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# To use this script: 
# setwd("C:/_dev/msc/thesis/")
# source('2019-06-19-jsa-type/main.r')

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO CLEANING SCRIPTS FOR BEE TYPE DATA")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# Set up
start <- proc.time()
dir_script_td <- "2019-06-19-jsa-type/"

# Cleaning/ data wrangling scripts
source(paste0(dir_script_td, 'init/init.R'))
source(paste0(dir_script_td, 'clean/clean.r'))
source(paste0(dir_script_td, 'clean/df1.r'), encoding="utf-8")
source(paste0(dir_script_td, 'clean/df2.r'))
source(paste0(dir_script_td, 'clean/df3.r'))
source(paste0(dir_script_td, 'clean/df4.r'))
print(proc.time()-start)
rm(list=ls())

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("BEE TYPE DATA CLEANING COMPLETE.")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
