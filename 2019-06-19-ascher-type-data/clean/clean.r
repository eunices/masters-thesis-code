# Information about code:
# This code corresponds to cleaning code for my MSc thesis.
# A series of other codes are named as clean1|2|3|4.r
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting clean.r"))
print("######################################################")
print("######################################################")
print("######################################################")

source('2019-06-19-ascher-type-data/init/init.R')

# Scripts
#############
source('2019-06-19-ascher-type-data/clean/clean1.r', local=T)
source('2019-06-19-ascher-type-data/clean/clean2.r', local=T)
source('2019-06-19-ascher-type-data/clean/clean3.r', local=T)
source('2019-06-19-ascher-type-data/clean/clean4.r', local=T)
