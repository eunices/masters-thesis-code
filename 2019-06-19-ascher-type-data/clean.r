print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting clean.r"))
print("######################################################")
print("######################################################")
print("######################################################")

source('2019-06-19-ascher-type-data/init.R')

# Scripts
#############
source('2019-06-19-ascher-type-data/clean1.r', local=T)
source('2019-06-19-ascher-type-data/clean2.r', local=T)
source('2019-06-19-ascher-type-data/clean3.r', local=T)
source('2019-06-19-ascher-type-data/clean4.r', local=T)
