print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO ANALYSES SCRIPTS FOR BEE TYPE DATA")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")


# setwd("C:/Dev/msc-thesis-code/")
# source(paste0(dir_script_ed, "analysis.r"))
dir_script_ed <- "2019-07-15-edie-et-al/"

# Scripts
#############
# Analysis
source(paste0(dir_script_ed, 'analysis0.r')) # data prep
source(paste0(dir_script_ed, 'analysis1.r')) # data prep
source(paste0(dir_script_ed, 'analysis2.r')) # model fitting
source(paste0(dir_script_ed, 'analysis3.r')) # post
source(paste0(dir_script_ed, 'analysis4.r')) # forecast
source(paste0(dir_script_ed, 'analysis5.r')) # plot

# TODO: figure out a way to store params for models
