# An exercise on species disribution modelling with Megachile sculpturalis
# R packages to use: SDMTools, dismo, biomod, hSDM


# Setup
setwd('C:/Dev/msc-thesis-code')
source('keys.R')

# Load libraries
library(data.table)
library(ggmap)


# Parameters
data_in = 'data/2019-05-27-gbif-data/0018967-190415153152247.csv'
data_geo = 'data/2019-05-27-gbif-data/0018967-190415153152247-geocoded.csv'
data_out = 'data/2019-05-27-gbif-data/0018967-190415153152247-clean.csv'
if(file.exists(data_geo)){
    should_geocode = 'no'
} else {
    should_geocode = 'yes'
}

# Initialize google api for geocoding
register_google(key = geocode_api)


# Read dataset
df = fread(data_in) # obtained from GBIF
df_cols = names(df)


# Georeference those without coordinates
if(should_geocode == "yes"){
georef_cols = c(
    'locality',
    'verbatimLocality'
)
# followed for setting up setting api keys https://stackoverflow.com/questions/52565472/
filter = is.na(df$decimalLatitude) | is.na(df$decimalLongitude)
table(filter)

# Separate tables
df1 = df[!filter]   # no need geocoding
df2 = df[filter]    # to be geocoded
# Remove those with no locality
df2 = df2[!(verbatimLocality == "" & locality == ""),]; print(dim(df2)[1]) 
# Create columns with most information for geocoding
df2$localityCombined = df2$locality
df2[locality=="",]$localityCombined = df2[locality=="", verbatimLocality]
df2$localityMoreDetails = paste0(gsub("\\|", ", ", df2$higherGeography), ", ", df2$localityCombined)
df2$localityMoreDetails = gsub("^, ", "", df2$localityMoreDetails)
# Checks
df2[locality=="", localityMoreDetails]
df2[verbatimLocality=="", localityMoreDetails]
dim(df2[verbatimLocality=="" & locality=="",])
# Geocoding
geocoded = try(geocode(df2$localityMoreDetails)) 
geocoded = as.data.frame(geocoded)
df2$decimalLatitude = geocoded$lat
df2$decimalLongitude = geocoded$lon
# Cleanup 
filter = is.na(df2$decimalLatitude) | is.na(df2$decimalLongitude)
table(filter)
df2 = df2[!filter]
df2$hasGeospatialIssues = TRUE
df2$issue = "GEOREFERENCED W/ GOOGLE API"
df2$locality = paste0(df2$localityMoreDetails)
df2$localityCombined = NULLs
df2$localityMoreDetails = NULL
# Recombine both dataframes
df = rbind(df1, df2)
# Last checks
df[hasCoordinate==FALSE, decimalLatitude]
df[hasCoordinate==FALSE, decimalLongitude]
write.csv(df, data_geo)
}


# Read geocoded data
df = fread(data_geo)


# Quick cleans
df$institutionCodeShort = gsub(".*\\((.*)\\).*", "\\1", df[,institutionCode])


# Remove duplicates
# TODO:
# 1: may be based on exact time/date/locality
# 2: may have same catalogNumber


# Write data out
write.csv(df, data_out)