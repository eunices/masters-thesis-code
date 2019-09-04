# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by country
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by country"))

df_mapper2 <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty1.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

df_mapper2$Country.final <- ifelse(is.na(df_mapper2$Country1), df_mapper2$Country2, df_mapper2$Country1)

missing <- unique(df_mapper2[is.na(df_mapper2$Country.final)]$idx)
not_missing <- unique(df_mapper2[!is.na(df_mapper2$Country.final)]$idx)

# no distribution data
no_data <- df[idx %in% as.character(missing) & !(idx %in% as.character(not_missing)), c("idx", "global.mapper")]
write.csv(no_data[order(global.mapper)], 
          paste0(dir, "clean/countries.csv"), row.names=F)

df_mapper3 <- df_mapper2[(!is.na(Country.final) & Country.final!=" "), c("idx", "Country.final")]
df_mapper3 <- unique(df_mapper3)
df_mapper3[Country.final == "C<f4>te d'Ivoire", ]$Country.final <- "Côte d'Ivoire"
df_mapper3[Country.final == "Cura<e7>ao",]$Country.final <- "Curaçao"
df_mapper3[Country.final == "Saint Barth<e9>lemy",]$Country.final <- "Saint Barthélemy"

df_mapper3 <- merge(df_mapper3, lookup.cty, by.x="Country.final", by.y="Country", all.x=T, all.y=F)

df_mapper9 <- df_mapper3[,c("idx", "A.3", "Latitude_type", "Latitude_type2")]
df_mapper9[] <- lapply(df_mapper9, as.character)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by tropical/not
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by tropical/not [using countries]"))


# By type1 of trop/sub tropical
df_mapper10 <- copy(df_mapper9)
df_mapper10 <- df_mapper10[, no_cty_in_trop:=length(unique(A.3)), by=c("idx", "Latitude_type")]
df_mapper10 <- df_mapper10[, cty_in_trop:=paste(unique(A.3), collapse=', '), by=c("idx", "Latitude_type")]
df_mapper10 <- df_mapper10[,c("idx", "Latitude_type", "no_cty_in_trop", "cty_in_trop")]
df_mapper10 <- unique(df_mapper10)

df_merge <- df[,c("idx", "duplicated.row", "date.n", "full.name.of.describer")]
df_merge[] <- lapply(df_merge, as.character)
df_mapper10a <- merge(df_mapper10, df_merge, by.x="idx", by.y="idx", all.x=T, all.y=F)

write.csv(df_mapper10a[date.n <2019][duplicated.row=="FALSE"][order(as.numeric(idx))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty6-trop-type1.csv"), na='', row.names=F, fileEncoding="UTF-8")

# By type2 of trop/sub tropical
df_mapper11 <- copy(df_mapper9)
df_mapper11 <- df_mapper11[, no_cty_in_trop:=length(unique(A.3)), by=c("idx", "Latitude_type2")]
df_mapper11 <- df_mapper11[, cty_in_trop:=paste(unique(A.3), collapse=', '), by=c("idx", "Latitude_type2")]
df_mapper11 <- df_mapper11[,c("idx", "Latitude_type2", "no_cty_in_trop", "cty_in_trop")]
df_mapper11 <- unique(df_mapper11)

df_merge <- df[,c("idx", "duplicated.row", "date.n", "full.name.of.describer", "family")]
df_merge[] <- lapply(df_merge, as.character)
df_mapper11a <- merge(df_mapper11, df_merge, by.x="idx", by.y="idx", all.x=T, all.y=F)

write.csv(df_mapper11a[date.n <2019][duplicated.row=="FALSE"][order(as.numeric(idx))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty7-trop-type2.csv"), na='', row.names=F, fileEncoding="UTF-8")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - appending WWF's ecoregions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- appending WWF's ecoregions [using countries]"))

#### Using WWF's ####
countries_biogeo <- as.data.table(shp3)
countries_biogeo <- countries_biogeo[, no_states_in_realm:=.N, by=c("GID_0", "REALM_E")]
countries_biogeo <- countries_biogeo[,c("GID_0", "no_states_in_realm", "REALM_E")][order(GID_0, -no_states_in_realm)]
countries_biogeo <- countries_biogeo[!duplicated(countries_biogeo$GID_0),]

df_mapper3 <- merge(df_mapper3, countries_biogeo, by.x="A.3", by.y="GID_0", all.x=T, all.y=F)

df_mapper4 <- df_mapper3[,c("idx", "A.3", "REALM_E")]
df_mapper4[] <- lapply(df_mapper4, as.character)
df_mapper4[A.3=="BLM",]$REALM_E = "NT" # Saint Barthélemy
df_mapper4[A.3=="NIU",]$REALM_E = "OC" # Niue
df_mapper4[A.3=="MDV",]$REALM_E = "IM" # Maldives
df_mapper4[A.3=="AIA",]$REALM_E = "NT" # Anguilla
df_mapper4[A.3=="CXR",]$REALM_E = "IM" # Christmas Island
df_mapper4[A.3=="MCO",]$REALM_E = "PA" # Monaco
df_mapper4[A.3=="CUW",]$REALM_E = "NT" # Curaçao

df_mapper4[A.3=="MHL",]$REALM_E = "OC" # Marshall Islands
df_mapper4[A.3=="ABW",]$REALM_E = "NT" # Aruba
df_mapper4[A.3=="ATA",]$REALM_E = "AN" # Antarctica
df_mapper4[A.3=="GIB",]$REALM_E = "PA" # Gibraltar
df_mapper4[A.3=="KIR",]$REALM_E = "OC" # Kiribati
df_mapper4[A.3=="MAF",]$REALM_E = "NT" # Saint Martin (French part)
df_mapper4[A.3=="PCN",]$REALM_E = "OC" # Pitcairn
df_mapper4[A.3=="SXM",]$REALM_E = "OC" # Sint Maarten
# TODO: check islands why missing in 2019-06-24-spatial-join.R

no_data <- fread(paste0(dir, "clean/countries_edit.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
no_data <- merge(no_data, lookup.cty[c("A.2", "A.3")], all.x=T, all.y=F, by.x="corrected.global.mapper", by.y="A.2")
no_data[] <- lapply(no_data, as.character)
df_mapper4 <- rbind(df_mapper4, no_data[,c("idx", "A.3", "REALM_E")])


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - appending Holt's ecoregions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- appending Holt's ecoregions [using countries]"))

#### Using Holt's ####
countries_biogeo <- as.data.table(shp7)
countries_biogeo <- countries_biogeo[, no_states_in_realm_holt:=.N, by=c("GID_0", "Realm")]
countries_biogeo <- countries_biogeo[,c("GID_0", "no_states_in_realm_holt", "Realm")][order(GID_0, -no_states_in_realm_holt)]
countries_biogeo <- countries_biogeo[!duplicated(countries_biogeo$GID_0),]
df_mapper6 <- merge(df_mapper4, countries_biogeo, by.x="A.3", by.y="GID_0", all.x=T, all.y=F)
df_mapper6$no_states_in_realm_holt <- NULL

df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'VUT',]$Realm <- 'Australian'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'GNQ',]$Realm <- 'Afrotropical'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'MUS',]$Realm <- 'Afrotropical'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'MAC',]$Realm <- 'Oriental'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'ABW',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'AIA',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'ATA',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'BLM',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'BMU',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'CUW',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'CXR',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'GIB',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'GRL',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'KIR',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'MAF',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'MCO',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'MDV',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'MHL',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'NIU',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'PCN',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'SXM',]$Realm <- 'Nearctic'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'GRD',]$Realm <- 'Neotropical'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'FSM',]$Realm <- 'Oceanina'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'MNP',]$Realm <- 'Oceanina'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'PYF',]$Realm <- 'Oceanina'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'TON',]$Realm <- 'Oceanina'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'TUV',]$Realm <- 'Oceanina'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'UMI',]$Realm <- 'Oceanina'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$A.3 == 'GGY',]$Realm <- 'Oceanina'

df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$REALM_E == 'AT',]$Realm <- 'Afrotropical'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$REALM_E == 'IM',]$Realm <- 'Oriental'
df_mapper6[is.na(df_mapper6$Realm) & df_mapper6$REALM_E == 'PA',]$Realm <- 'Palearctic'

df_merge <- df[,c("idx", "duplicated.row", "date.n", "full.name.of.describer")]
df_merge[] <- lapply(df_merge, as.character)
df_mapper6a <- merge(df_mapper6, df_merge, by.x="idx", by.y="idx", all.x=T, all.y=F)

write.csv(df_mapper6a[date.n <2019][duplicated.row=="FALSE"][order(as.numeric(idx))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty2-cty.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by continent
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by continent"))

df_mapper4b <- df_mapper6
df_mapper4b <- merge(df_mapper4b, shp5[,c("GID_0", "CONTINENT")], by.x="A.3", by.y="GID_0")
df_mapper4b[,countries:=paste(unique(A.3), collapse=', '), by=c("idx", "CONTINENT")]
df_mapper4b[,countries_n:=length(unique(A.3)), by=c("idx", "CONTINENT")]
df_mapper4b <- unique(df_mapper4b[,c("idx", "countries", "CONTINENT", "countries_n")])
df_mapper4b[,no_continents:=length(unique(CONTINENT)), by=c("idx")]
df_mapper4b <- merge(df_mapper4b, df_merge, by.x="idx", by.y="idx", all.x=T, all.y=F)

df_mapper4b[countries=="BLM",]$CONTINENT <- "North America"
df_mapper4b[countries=="SXM",]$CONTINENT <- "North America"
df_mapper4b[countries=="MAF",]$CONTINENT <- "North America"

write.csv(df_mapper4b[duplicated.row=="FALSE"][order(as.numeric(idx))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty3-continent.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by biogeographic region WWF
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by biogeographic region (WWF)"))

df_mapper4[,countries:=paste(unique(A.3), collapse=', '), by=c("idx", "REALM_E")]
df_mapper4[,countries_n:=length(unique(A.3)), by=c("idx", "REALM_E")]
df_mapper4 <- unique(df_mapper4[,c("idx", "countries", "REALM_E", "countries_n")])
df_mapper4[,no_realms:=length(unique(REALM_E)), by=c("idx")]

df_mapper5 <- merge(df_mapper4, df_merge,
                    by.x="idx", by.y="idx", all.x=T, all.y=F)

df_mapper5 <- df_mapper5[duplicated.row == FALSE]

AT_PA <- c("MLI", "NER", "TCD", "SDN", "YEM", "SAU")
IM_PA <- c("OMN", "PAK", "CHN")
AA_IM <- c("IND")
NA_NT <- c("MEX")
df_mapper5$BIOGEO_OVERLAP_CTY <- "FALSE"
df_mapper5[grepl(paste(c(AT_PA), collapse='|'), countries),]$BIOGEO_OVERLAP_CTY <- "AT_PA"
df_mapper5[grepl(paste(c(IM_PA), collapse='|'), countries),]$BIOGEO_OVERLAP_CTY <- "IM_PA"
df_mapper5[grepl(paste(c(AA_IM), collapse='|'), countries),]$BIOGEO_OVERLAP_CTY <- "AA_IM"
df_mapper5[grepl(paste(c(NA_NT), collapse='|'), countries),]$BIOGEO_OVERLAP_CTY <- "NA_NT"

df_mapper5 <- merge(df_mapper5, unique(df_mapper5[BIOGEO_OVERLAP_CTY != "FALSE", c("idx", "BIOGEO_OVERLAP_CTY")]), all.x=T, all.y=F, by.x="idx", by.y="idx", suffix=c("_row", "_idx"))

df_mapper5 <- df_mapper5[duplicated.row == FALSE]
cols <- c("idx", "REALM_E", "date.n", "full.name.of.describer", "countries", "countries_n", "BIOGEO_OVERLAP_CTY_row", "BIOGEO_OVERLAP_CTY_idx")
write.csv(df_mapper5[order(as.numeric(idx))][,..cols],
          paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty4-biogeo.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by biogeographic region Holt
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by biogeographic region (Holt)"))

df_mapper6[,countries:=paste(unique(A.3), collapse=', '), by=c("idx", "Realm")]
df_mapper6[,countries_n:=length(unique(A.3)), by=c("idx", "Realm")]
df_mapper6 <- unique(df_mapper6[,c("idx", "countries", "Realm", "countries_n")])
df_mapper6[,no_realms:=length(unique(Realm)), by=c("idx")]

df_mapper8 <- merge(df_mapper6, df_merge,
                    by.x="idx", by.y="idx", all.x=T, all.y=F)

df_mapper8 <- df_mapper8[duplicated.row == FALSE]
cols <- c("idx", "Realm", "date.n", "full.name.of.describer", "countries", "countries_n")
write.csv(df_mapper8[order(as.numeric(idx))][,..cols],
          paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4-species-cty5-biogeo-holt.csv"), na='', row.names=F, fileEncoding="UTF-8")

