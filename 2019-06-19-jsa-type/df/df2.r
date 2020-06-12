# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating distribution datasets by country and other groups.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


print("######################################################")
print(paste0(Sys.time(), " --- starting df2.r"))
print("######################################################")

source('2019-06-19-jsa-type/init/init.R')
source('2019-06-19-jsa-type/df/functions.R')

# Libraries
#############

# NONE

# Parameters
#############

# NONE

# Scripts
#############
source('2019-06-19-jsa-type/df/df2.1.r', local=T)
source('2019-06-19-jsa-type/df/df2.2.r', local=T)
