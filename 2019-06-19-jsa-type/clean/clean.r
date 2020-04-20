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

source('2019-06-19-jsa-type/init/init.R')

# Scripts
#############
source('2019-06-19-jsa-type/clean/clean1.r', local=T)
source('2019-06-19-jsa-type/clean/clean2.r', local=T)
source('2019-06-19-jsa-type/clean/clean3.r', local=T)
source('2019-06-19-jsa-type/clean/clean4.r', local=T)
