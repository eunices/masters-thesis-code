# Initialize
source('2019-08-05-bee-distribution-data/init_amnh.r')
source('2019-08-05-bee-distribution-data/init_gbif.r')

names(df_amnh)
names(df_gbif)


# Column metadata
useful_names_amnh = list(
    identifier="PBIUSI",
    genus.species = "binomial.nomenclature", # derived, not from original dataset, Genus + species
    family = "Family",
    subfamily = "Subfamily",
    tribe = "Tribe",
    date.collection = "Start_Date",
    latitude = "Lat",
    longitude = "Lon",
    datum = NA,
    lat.lon.uncertainty = "Lat_Lon_Accuracy",
    lat.lon.method = "Lat_Lon_Method",
    record.type = NA,
    storage.repository = "Inst_Code",
    collector = "Collector",
    locality.verbatim = "Locality",
    locality.country = "Country",
    locality.primary_div = "State_Prov",
    persistent.link = NA, 
    floral.resource = c("Host_family", "Host_Genus", "Host_species"),
    parasite = NA
)

useful_names_gbif = list(
    identifier="gbifID",
    genus.species = "species",
    family = "family",
    subfamily = NA,
    tribe = NA,
    date.collection = c("eventDate", "day", "month", "year"),
    latitude = "decimalLatitude",
    longitude = "decimalLongitude",
    datum = NA,
    lat.lon.uncertainty = "coordinateUncertaintyInMeters",
    lat.lon.method = NA,
    record.type = "basisOfRecord",
    storage.repository = "institutionCode",
    collector = "recordedBy",
    locality.verbatim = "locality",
    locality.country = "countryCode",
    locality.primary_div = NA,
    persistent.link = "occurrenceID", 
    floral.resource = NA,
    parasite = NA
)

# http://rs.gbif.org/core/dwc_occurrence.xml
# http://rs.gbif.org/extension/gbif/1.0/distribution.xml


# Parameters
dataset_identifier <- "amnh"
dataset_identifier <- "gbif"

# Workflow
dataset <- eval(parse(text = paste0("df_", dataset_identifier)))
useful_names <- eval(parse(text = paste0("useful_names_", dataset_identifier)))
rm(list=paste0("df_", dataset_identifier))

# Analysis for metrics

## Latitude and longitude
print(paste0("Latitude, Longitude (LL), and Locality, Method and Uncertainty"))
print(paste0("Both LL present"))
check1 <- !(is.na(dataset[[useful_names$latitude]]) | is.na(dataset[[useful_names$longitude]]))
check2 <- useful_names$locality
table(check1)

hist(df_gbif[df_gbif$coordinateUncertaintyInMeters<4000]$coordinateUncertaintyInMeters, breaks=20)
summary(df_gbif$coordinateUncertaintyInMeters)
