# Information about code:
# This code corresponds to data wrangling for my MSc thesis.
# They format the original datasets into other datasets for analyses.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

print("")
print(paste0(Sys.time(), " --- starting df1.r"))
print("######################################################")

source('2019-06-19-jsa-type/init/init.R')
source('2019-06-19-jsa-type/df/functions.R')


# Libraries
#############

# NONE

# Parameters
#############

# loop_3 <- 'Y'
loop_3 <- 'N'

# Scripts
#############
source('2019-06-19-jsa-type/df/df1.1.r', local=T)
source('2019-06-19-jsa-type/df/df1.2.r', local=T)
source('2019-06-19-jsa-type/df/df1.3.r', local=T)
source('2019-06-19-jsa-type/df/df1.4.r', local=T)
source('2019-06-19-jsa-type/df/df1.5.r', local=T)
source('2019-06-19-jsa-type/df/df1.6.r', local=T)
