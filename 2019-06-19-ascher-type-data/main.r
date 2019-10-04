print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO CLEANING SCRIPTS FOR BEE TYPE DATA")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# setwd("C:/Dev/msc-thesis-code/")
dir_script_td <- "2019-06-19-ascher-type-data/"
# source('2019-06-19-ascher-type-data/main.r')

# Scripts
#############

# Clean
start <- proc.time()
source(paste0(dir_script_td, 'init.r'))
source(paste0(dir_script_td, 'clean.r'))
source(paste0(dir_script_td, 'df1.r'), encoding="utf-8")
source(paste0(dir_script_td, 'df2.r'))
source(paste0(dir_script_td, 'df3.r'))
source(paste0(dir_script_td, 'df4.r'))
source(paste0(dir_script_td, 'subset.r'))
print(proc.time()-start)


rm(list=ls())

print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("BEE TYPE DATA CLEANING COMPLETE.")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
