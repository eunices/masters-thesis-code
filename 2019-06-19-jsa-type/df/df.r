# Information about code:
# This code corresponds to cleaning code for my MSc thesis.
# A series of other codes named as df1|df2.r
# These code converts the denormalised data into derived datasets.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

print("")
print("n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print(paste0(Sys.time(), " --- starting derived datasets df.r"))
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("")

source('2019-06-19-jsa-type/init/init.R')
source('2019-06-19-jsa-type/df/functions.R')

# Scripts
#############
source('2019-06-19-jsa-type/df/df1.r', local=T)
source('2019-06-19-jsa-type/df/df2.r', local=T)