library(data.table)
library(tidyr)
library(gridExtra)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Read datasets"))

dir_data <- 'data/2019-05-23-ascher-bee-data/'
dir_script <- '2019-06-19-ascher-type-data/'
source(paste0(dir_script, "subset.r"))

print(paste0("Read df"))
df <- get_df1(write=F)
df <- df[date.n<2019]
df$date.decade <- paste0(substr(df$date.n, 1, 3), "0s")

print(paste0("Read df_country"))
df_country <- get_dis(write=F)
table(df_country$duplicated.row)
table(is.na(df_country$A.3))
dim(df_country); df_country <- df_country[!is.na(A.3)]; dim(df_country)
# df_country$duplicated.row <- NULL
df_country <- merge(df_country, df[, c("idx", "date.n")], all.x=T, all.y=F)[date.n<2019]

print(paste0("Read df_publications"))
df_publications <- get_pub(write=F)
dim(df_publications)
df_publications$date.decade <- paste0(substr(df_publications$date.n, 1, 3), "0s")

print(paste0("Read df_continent"))
df_continent <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty3-continent.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
table(df_continent$duplicated.row)
table(is.na(df_continent$countries))
df_continent$duplicated.row <- NULL
df_continent <- df_continent[date.n<2019]

print(paste0("Read df_biogeo"))
df_biogeo <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty4-biogeo.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
table(is.na(df_biogeo$REALM_E))
dim(df_biogeo); df_biogeo <- df_biogeo[!is.na(REALM_E)]; dim(df_biogeo)
df_biogeo <- df_biogeo[date.n<2019]


print(paste0("Read df_biogeo_holt"))
df_biogeo_holt <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty5-biogeo-holt.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
table(is.na(df_biogeo_holt$Realm))
dim(df_biogeo_holt); df_biogeo_holt <- df_biogeo_holt[!is.na(Realm)]; dim(df_biogeo_holt)
df_biogeo_holt <- df_biogeo_holt[date.n<2019]


print(paste0("Read df_trop_type1"))
df_trop_type1 <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty6-trop-type1.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
table(is.na(df_trop_type1$Latitude_type))
df_trop_type1 <- df_trop_type1[date.n<2019]


print(paste0("Read df_trop_type2"))
df_trop_type2 <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty7-trop-type2.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
table(is.na(df_trop_type2$Latitude_type))
df_trop_type2 <- df_trop_type2[date.n<2019]


print(paste0("Read taxonomic_effort / taxonomic_effort_long"))
taxonomic_effort <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_6.0-active-by-year.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
taxonomic_effort_long <- data.table(taxonomic_effort %>% gather(type, N, N_describers:N_synonyms))


print(paste0("Read df_describers"))
df_describers <- get_des(write=F)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read shp files
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Read shpfiles"))

if(!exists("shp_gadm_0")) {
    print(paste0("Read shp_gadm_0"))
    shp_gadm_0 <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp')
}
if(!exists("shp_biogeo")) {
    print(paste0("Read shp_biogeo"))
    shp_biogeo <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp')
}
if(!exists("shp_biogeo_holt")) {
    print(paste0("Read shp_biogeo_holt"))
    shp_biogeo_holt <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt2.shp')
}
if(!exists("shp_continents")) {
    print(paste0("Read shp_continents"))
    shp_continents <- sf::st_read('data/geo_processed/gadm/gadm36_0_utf8_continents.shp')
}
