print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting clean.r"))
print("######################################################")
print("######################################################")
print("######################################################")

source('2019-06-19-ascher-type-data/init.R')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - initial formatting
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- initial formatting"))

# read dataset


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - georeferencing
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- georeferencing"))

# read dataset


# clean country


# clean state


# check for out of range/ odd placements of lat lon


# incorporate manually cleaned lat and lon


# geocode blank lat lon from locality


# add country/state to those with lat lon if missing


# check country/state and lat/lon


# add flags

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - cleaning repository
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- cleaning repository"))

# clean repository


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - cleaning other fields
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- cleaning other fields"))




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - splitting dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- splitting dataset"))





# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean fields specific to valid species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean fields specific to valid species"))



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean fields specific to valid species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean fields specific to not-valid species"))
