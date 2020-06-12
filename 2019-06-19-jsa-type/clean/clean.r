# Information about code:
# This code corresponds to cleaning code for my MSc thesis.
# A series of other codes are named as clean1|2|3|4|5|6.r
# The scope of these scripts include making the data consistent &
# denormalising the tables (e.g. creating describer, collector, 
# non-valid species, publications tables)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

print("")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print(paste0(Sys.time(), " --- starting clean.r"))
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("")

source('2019-06-19-jsa-type/init/init.R')
source('2019-06-19-jsa-type/clean/functions.R')

# Scripts
#############
source('2019-06-19-jsa-type/clean/clean1.r', local=T)
source('2019-06-19-jsa-type/clean/clean2.r', local=T)
source('2019-06-19-jsa-type/clean/clean3.r', local=T)
source('2019-06-19-jsa-type/clean/clean4.r', local=T)
source('2019-06-19-jsa-type/clean/clean5.r', local=T, encoding="utf-8")
source('2019-06-19-jsa-type/clean/clean6.r', local=T)
