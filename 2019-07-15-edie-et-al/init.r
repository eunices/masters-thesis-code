library(data.table)
library(ggplot2)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Read datasets"))

dir <- 'data/01-in/ascher-bee-data/'


df_country <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty2-cty.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(names(df_country))
table(df_country$duplicated.row)
table(is.na(df_country$A.3))
dim(df_country); df_country <- df_country[!is.na(A.3)]; dim(df_country)
df_country$duplicated.row <- NULL

df_continent <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty3-continent.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(names(df_continent))
table(df_continent$duplicated.row)
table(is.na(df_continent$countries))
df_continent$duplicated.row <- NULL


df_biogeo <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty4-biogeo.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(names(df_biogeo))
table(is.na(df_biogeo$REALM_E))
dim(df_biogeo); df_biogeo <- df_biogeo[!is.na(REALM_E)]; dim(df_biogeo)


df_biogeo_holt <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty5-biogeo-holt.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

print(names(df_biogeo_holt))
table(is.na(df_biogeo_holt$Realm))
dim(df_biogeo_holt); df_biogeo_holt <- df_biogeo_holt[!is.na(Realm)]; dim(df_biogeo_holt)