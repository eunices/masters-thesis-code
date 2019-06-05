# An exercise on species disribution modelling with Megachile sculpturalis
# R packages to use: SDMTools, dismo, biomod, hSDM

# 01


# Setup
source('keys.R')
setwd(working_dir)


# Load libraries
library(data.table)
library(ggmap)


# Parameters
data_in = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247.csv'
data_geo = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-geocoded.csv'
data_out = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-clean.csv'
if(file.exists(data_geo)){
    should_geocode = 'no'
} else {
    should_geocode = 'yes'
}

# Initialize google api for geocoding
register_google(key = geocode_api)


# Read dataset
df = fread(data_in, integer64="character") # obtained from GBIF
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

    # Recover as many localities as possible by recovering higher geography
    condition1 = (df2$verbatimLocality == "" & df2$locality == "") # no localities
    condition2 = (df2$county != "" & df2$higherGeography == "")    # no county information & no higher geog
    to_paste = df2[condition1 & condition2, ]
    higher_geog = paste0(to_paste$stateProvince, ", ", 
                         to_paste$county, ", ", 
                         to_paste$municipality)
    df2[condition1 & condition2]$higherGeography =  higher_geog
    df2[condition1 & condition2]$issue = "GEOREFERENCED_GOOGLE_API_WITH_HIGHER_GEOG"
    print(table(condition1 & condition2))
    condition3 = df2$higherGeography %in% 
        c("", "?", "Japan", "0|0|0", "China", "Korea?|Koryo", "Korea?|Koryo.")
    df2[condition3]$higherGeography = ""

    # Remove those with no locality or higher geography
    condition4 = ((df2$verbatimLocality == "" & df2$locality == "") & df2$higherGeography == "")
    table(condition4)
    df2 = df2[!condition4, ]

    # Create columns with most information for geocoding
    df2$localityCombined = df2$locality
    df2[locality=="",]$localityCombined = df2[locality=="", verbatimLocality]
    df2[localityCombined=="" & higherGeography != ""]$issue = "GEOREFERENCED_GOOGLE_API_WITH_HIGHER_GEOG"
    df2$localityMoreDetails = 
        paste0(gsub("\\|", ", ", df2$higherGeography), ", ", df2$localityCombined)
    df2$localityMoreDetails = gsub("^, ", "", df2$localityMoreDetails)
    df2[issue=="",]$issue = "GEOREFERENCED_GOOGLE_API"

    # Geocoding
    geocoded = try(geocode(df2$localityMoreDetails))
    geocoded = as.data.frame(geocoded)
    df2$decimalLatitude = geocoded$lat
    df2$decimalLongitude = geocoded$lon

    # Cleanup 
    filter = is.na(df2$decimalLatitude) | is.na(df2$decimalLongitude)
    table(filter)
    df2 = df2[!filter]
    df2$locality = paste0(df2$localityMoreDetails)
    df2$localityCombined = NULL
    df2$localityMoreDetails = NULL

    # Recombine both dataframes
    df = rbind(df1, df2)

    # Last checks
    df[hasCoordinate==FALSE, decimalLatitude]
    df[hasCoordinate==FALSE, decimalLongitude]


    # Manually correcting georeferencing errors
    df[id=="1934303234"]$locality = "Taiwan, Nantou, Puli, "
    df[id=="1934303234"]$countryCode = "TW"
    df[id=="1934303282"]$locality = "Taiwan, Hualien, "
    df[id=="1934303282"]$countryCode = "TW"
    df[id=="1934303331"]$locality = "Taiwan, Nantou, Niaotakung,  "
    df[id=="1934303331"]$countryCode = "TW"
    df[id=="1934303300"]$locality = "Taiwan, Nantou, Wushe, "
    df[id=="1934303300"]$countryCode = "TW"
    df[id=="1934303297"]$decimalLatitude = 45.243670
    df[id=="1934303297"]$decimalLongitude = 127.927272

    # Write to csv
    write.csv(df, data_geo)
}


# Read geocoded data
df = fread(data_geo, integer64="character")


# Remove duplicates
# TODO:
# 1: may be based on exact time/date/locality
# 2: may have same catalogNumber


# Subset useful columns with criterion:
# 1) with data
# 2) retaining identifiers

cols = c(
    # specimen identifiers
    'id', 
    'identifier',
    'scientificName',

    # rank
    'taxonRank',

    # date/time
    'year',
    'month',
    'day', 
    'verbatimEventDate',

    # life history
    'sex',
    'associatedTaxa',
    'occurrenceRemarks',
    
    # lat long
    'decimalLatitude',
    'decimalLongitude',
    'coordinateUncertaintyInMeters',

    # issues with data
    'issue',
    'hasCoordinate',
    'hasGeospatialIssues',

    # specific locality
    'locality',
    'verbatimLocality',
    'verbatimElevation',

    # identification information
    'identifiedBy',
    'dateIdentified',
    'recordedBy',

    # higher locality
    'higherGeography',
    'countryCode',
    'stateProvince',
    'county',
    'municipality',

    # higher identifiers
    'basisOfRecord',
    'institutionCode',
    'collectionID',
    'collectionCode',
    'catalogNumber',
    'datasetName',
    'datasetKey',
    'occurrenceID',

    # ancillary info
    'associatedSequences',
    'references',
    'type',
    'license'
)
df = df[,..cols]


# Add records
species_name = "Megachile sculpturalis Smith, 1853"
taxon_rank = "SPECIES"

add = data.frame("", 
                 "AMNH_BEE00232195",
                 species_name,
                 taxon_rank,
                 "1938",
                 "7",
                 "23",
                 "",
                 "FEMALE",
                 "",
                 "",
                 30.37100,
                 102.81200,
                 NA,
                 "",
                 "TRUE",
                 "FALSE",
                 "Moupin (Baoxing)",
                 "", "",
                 "J. S. Ascher",
                 "2012-01-01T00:00:00Z",
                 "",
                 "China | Sichuan | Moupin (Baoxing)", 
                 "CN",
                 "", "", "", "PRESERVED_SPECIMEN", "AMNH"

)
names(add) = names(df)[1:length(add)]
df = rbind(df, add, fill=T)

add = NULL
add = data.frame("",
                 "I_JSA5727",
                 species_name,
                 taxon_rank,
                 "2017",
                 "7",
                 "18",
                 "2017:07:18 14:31:35",
                 "FEMALE",
                 "",
                 "",
                 34.560047, 
                 112.468264,
                 NA,
                 "",
                 "TRUE",
                 "FALSE",
                 "Longmen Grottoes",
                 "", "",
                 "J. S. Ascher",
                 "2015-01-01T00:00:00Z",
                 "",
                 "China | Henan | Longmen Grottoes", 
                 "CN", "", "", "", "HUMAN_OBSERVATION", "Discover Life")
names(add) = names(df)[1:length(add)]
df = rbind(df, add, fill=T)

add = NULL
add = data.frame("",
                 "AMNH_BEE00232199",
                 species_name,
                 taxon_rank,
                 "2017",
                 "7",
                 "18",
                 "2017:07:18 14:31:35",
                 "FEMALE",
                 "",
                 "",
                 32.06000,
                 118.79000,
                 NA,
                 "",
                 "TRUE",
                 "FALSE",
                 "Nanking (Nanjing)",
                 "", "",
                 "J. S. Ascher",
                 "2012-01-01T00:00:00Z",
                 "",
                 "China | Jiangsu | Longmen Grottoes", 
                 "CN", "", "", "", "PRESERVED_SPECIMEN", "AMNH")
names(add) = names(df)[1:length(add)]
df = rbind(df, add, fill=T)

add = NULL
add = data.frame("",
                 "AMNH_BEE00232196",
                 species_name,
                 taxon_rank,
                 "1963",
                 "12",
                 "15",
                 "2017:07:18 14:31:35",
                 "FEMALE",
                 "",
                 "",
                 23.96700,
                 120.96600,
                 NA,
                 "",
                 "TRUE",
                 "FALSE",
                 "Ren-ai Township, Puli Village",
                 "497", "",
                 "J. S. Ascher",
                 "2012-01-01T00:00:00Z",
                 "",
                 "Taiwan | Nantou | Ren-ai Township, Puli Village", 
                 "TW", "", "", "", "PRESERVED_SPECIMEN", "AMNH")
names(add) = names(df)[1:length(add)]
df = rbind(df, add, fill=T)



# Quick cleans
df$institutionCodeShort = gsub(".*\\((.*)\\).*", "\\1", df[,institutionCode])
df[institutionCode == "", "institutionCodeShort"] = df[institutionCode == "", "collectionCode"] 




# Write data out
write.csv(df, data_out)