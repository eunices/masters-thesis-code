# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating distribution datasets by country and other groups.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting df4.r"))
print("######################################################")
print("######################################################")
print("######################################################")

source('2019-06-19-ascher-type-data/init/init.R')

# Libraries
#############

# NONE

# Parameters
#############

# NONE

# Scripts
#############
source('2019-06-19-ascher-type-data/clean/df4.1.r', local=T)
source('2019-06-19-ascher-type-data/clean/df4.2.r', local=T)
