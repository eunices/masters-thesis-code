# Information about code:
# This code corresponds to data wrangling for my MSc thesis.
# They format the original datasets into other datasets for analyses.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting df3.r"))
print("######################################################")
print("######################################################")
print("######################################################")

source('2019-06-19-jsa-type/init/init.R')
source('2019-06-19-jsa-type/clean/functions.R')


# Libraries
#############

# NONE

# Parameters
#############

# loop_3 <- 'Y'
loop_3 <- 'N'

# Scripts
#############
source('2019-06-19-jsa-type/clean/df3.1.r', local=T)
source('2019-06-19-jsa-type/clean/df3.2.r', local=T)
source('2019-06-19-jsa-type/clean/df3.3.r', local=T)
source('2019-06-19-jsa-type/clean/df3.4.r', local=T)
source('2019-06-19-jsa-type/clean/df3.5.r', local=T)
source('2019-06-19-jsa-type/clean/df3.6.r', local=T)
