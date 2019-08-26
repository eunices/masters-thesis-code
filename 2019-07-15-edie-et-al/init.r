library(data.table)
library(ggplot2)
library(gridExtra)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Read datasets"))


dir <- 'data/01-in/ascher-bee-data/'


df <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.1-useful-col.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(paste0("Read df"))
df <- df[date.n<2019]


df_country <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty2-cty.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(paste0("Read df_country"))
table(df_country$duplicated.row)
table(is.na(df_country$A.3))
dim(df_country); df_country <- df_country[!is.na(A.3)]; dim(df_country)
df_country$duplicated.row <- NULL
df_country <- df_country[date.n<2019]


df_continent <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty3-continent.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(paste0("Read df_continent"))
table(df_continent$duplicated.row)
table(is.na(df_continent$countries))
df_continent$duplicated.row <- NULL
df_continent <- df_continent[date.n<2019]


df_biogeo <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty4-biogeo.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(paste0("Read df_biogeo"))
table(is.na(df_biogeo$REALM_E))
dim(df_biogeo); df_biogeo <- df_biogeo[!is.na(REALM_E)]; dim(df_biogeo)
df_biogeo <- df_biogeo[date.n<2019]


df_biogeo_holt <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty5-biogeo-holt.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(paste0("Read df_biogeo_holt"))
table(is.na(df_biogeo_holt$Realm))
dim(df_biogeo_holt); df_biogeo_holt <- df_biogeo_holt[!is.na(Realm)]; dim(df_biogeo_holt)
df_biogeo_holt <- df_biogeo_holt[date.n<2019]



df_trop_type1 <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty6-trop-type1.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(paste0("Read df_trop_type1"))
table(is.na(df_trop_type1$Latitude_type))
df_trop_type1 <- df_trop_type1[date.n<2019]



df_trop_type2 <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty7-trop-type2.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(paste0("Read df_trop_type1"))
table(is.na(df_trop_type2$Latitude_type))
df_trop_type2 <- df_trop_type2[date.n<2019]

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read shp files
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Read shpfiles"))

shp_gadm_0 <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp')
print(paste0("Read shp_gadm_0"))
shp_biogeo <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp')
print(paste0("Read shp_biogeo"))
shp_biogeo_holt <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt2.shp')
print(paste0("Read shp_biogeo_holt"))
shp_continents <- sf::st_read('data/geo_processed/gadm/gadm36_0_utf8_continents.shp')
print(paste0("Read shp_continents"))
