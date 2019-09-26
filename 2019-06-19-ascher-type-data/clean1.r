# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - initial formatting
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- initial formatting"))

# read dataset
filepath <- paste0(dir_data, '2019-05-23-Apoidea world consensus file Sorted by name 2019-idx.csv')
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes
# csv double quotes are escaped by \\"\\", fread reads them as "" instead of "
df[] <- lapply(df, gsub, pattern='[\r\n]', replacement=' ') # remove line carriages (otherwise funky things will happen with write.csv)

# replace unknown values with NA
replace_na <- c('other,_unknown,_or none', 'other,_unknown,_or_other', 'other,_unknown,_or_none')
for (i in 1:length(replace_na)){
    df[, names(df) := lapply(.SD, function(x) gsub(replace_na[i], NA, x))]
}

# rename column names
names(df) <- gsub("\\.\\.", "\\.", gsub(" ", ".", gsub("[[:punct:]]", "", tolower(names(df)))))
names(df) <- iconv(names(df), from = 'UTF-8', to = 'ASCII//TRANSLIT')
if (any(grepl("full.name.a.e", names(df)))) {
    names(df)[which(grepl("full.name.a.e", names(df)))] <- 'full.name' # renaming this long name
}

# initialize flag column
df$flag <- '' 

# exclude those with no idx [those "nomen nudem etc"]
df <- df[!is.na(idx)]
odim <- dim(df)
print(paste0("Dataset read is ", odim[1], " rows and ", odim[2], " cols."))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - georeferencing
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- georeferencing"))

# check for odd placements of lat lon [make manual changes below based on idx]
df$lat_li <- df$lat; df$lon_li <- df$lon
df$lat <- as.numeric(df$lat) # convert lat lon to numeric
df$lon <- as.numeric(df$lon)
df[is.na(lat) & lat_li != "", c("idx", "lat", "lat_li")]
df[is.na(lon) & lon_li != "", c("idx", "lon", "lon_li")]
df$lat_li <- NULL; df$lon_li <- NULL

# check for out of range lat lon [make manual changes below based on idx]
df[lat < -90, c("idx", "lat")]
df[lat > 90, c("idx", "lat")]
df[lon < -180, c("idx", "lon")]
df[lon > 180, c("idx", "lon")]

# lat lon quick fixes
df[idx==34]$lat <- 46.6666
df[idx==34]$lon <- 11.6666
df[idx==31958]$lat <- 46.6666
df[idx==31958]$lon <- 11.6666
df[idx==18963]$lat <- NA
df[idx==20365]$lat <- 34.0751
df[idx==20571]$lat <- 45.146
df[idx %in% c(10061, 10076, 10133, 10143, 10146, 9762, 9825)]$lat <- -21.3558
df[idx==18963]$lon <- NA
df[idx==1530]$lon <- -70.617721

# clean country & state
# Quick fixes
df[idx==23111]$type.country = "IT"
df[idx==23361]$type.country = "PL"
df[idx==24784]$type.country = "FS"
df[idx==28152]$type.country = "IT"
df[idx==29142]$type.country = "PL"
df[idx==30057]$type.country = "IT"

df$type.country.n <- trimws(gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", df$type.country),                                        which="both")
df$type.country.n  <- gsub("([A-Za-z]+).*", "\\1", df$type.country.n) # get first word
df$type.country.n[grepl("\\?", df$type.country)] <- NA
df$type.state.n <- trimws(gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", df$type.state), which="both")
df$type.state.n[grepl("\\?", df$type.state.n)] <- NA

country_check1 <- merge(df[, c("idx", "type.country.n")], lookup.cty[, c("DL", "Country")],
                       all.x=T, all.y=F, by.x="type.country.n", by.y="DL")
country_check2 <- merge(df[, c("idx", "type.country.n")], 
                        lookup.loc[, c("DL", "NAME_0_owner")],
                        all.x=T, all.y=F, by.x="type.country.n", by.y="DL")
country_check <- merge(country_check1, country_check2, all.x=T, all.y=F, 
                       by=c("idx", "type.country.n"))

country_check$Country_long <- ifelse(is.na(country_check$Country),
                                           country_check$NAME_0_owner, country_check$Country)
country_check[is.na(Country_long) &  type.country.n != ""] # CHECK WITH JSA
table(country_check[is.na(Country_long) &  type.country.n != ""]$type.country.n) # CHECK WITH JSA

country_check <- merge(country_check, lookup.cty[, c("DL", "Country")], 
                       all.x=T, all.y=F, by.x="Country_long", by.y="Country")

df <- merge(df, country_check[, c("idx", "DL", "Country_long")], by="idx", all.x=T, all.y=F)
df$type.country.n <- df$DL; df$type.country.n.full <- df$Country_long
df$DL <- NULL; df$Country_long <- NULL

df$cty.state <- ifelse(
    is.na(df$type.state.n) | df$type.state.n == "" | is.na(df$type.country.n), NA, 
        paste0(df$type.country.n, ".", df$type.state.n))

df <- merge(df, lookup.pri_div[,c("CTY.STATE.CODE", "NAME_1")], 
            by.x="cty.state", by.y="CTY.STATE.CODE", all.x=T, all.y=F)
df[is.na(NAME_1)]$type.state.n <- NA
names(df)[which(names(df) == "Country.final")] <- "type.country.n.full"
names(df)[which(names(df) == "NAME_1")] <- "type.state.n.full"
df$cty.state <- NULL

# incorporate manually cleaned lat and lon, and flag
filepath <- paste0(dir_data, 'clean/lat-lon-edit.csv')
ll <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
ll <- ll[, c("idx", "lat", "lon", "type.country.n", "type.state.n", "flag")]
ll$idx <- as.character(ll$idx)
df <- merge(df, ll, all.x=T, all.y=F, by="idx", suffixes=c("", "_n"))
df[!(is.na(lat_n))]$lat <- df[!(is.na(lat_n))]$lat_n 
df[!(is.na(lat_n))]$lon <- df[!(is.na(lat_n))]$lon_n
df[!(is.na(lat_n))]$type.country.n <- df[!(is.na(lat_n))]$type.country.n_n
df[!(is.na(lat_n))]$flag <- df[!(is.na(lat_n))]$flag_n
df[, c("lat_n", "lon_n", "type.country.n_n", "type.state.n_n", "flag_n"):=NULL]

# add country/state to those with lat lon if missing
check0 <- is.na(df$lat) | is.na(df$lon)
check1 <- is.na(df$type.country.n) | df$type.country.n == ""
check2 <- (is.na(df$type.state.n) | df$type.state.n == "") & 
          (is.na(df$type.state.n.full) | df$type.state.n.full == "")
df$check1 <- check1
df$check2 <- check2

get_state <- df[!check0 & (check1|check2)]
ll <- sf::st_as_sf(get_state[,c("idx", "lat", "lon", "type.country.n", "type.state.n", 
                                 "type.country.n.full", "type.state.n.full", 
                                 "type.country", "type.state", "check1", "check2",
                                 "type.locality.verbatim", "type.locality.updated")], 
                   coords = c('lon', 'lat'),
                   crs = "+init=epsg:4326")
countries_shp <- sf::st_join(ll, shp8, join = st_intersects) # derive new columns
countries <- data.frame(countries_shp); countries$HASC_1 <- as.character(countries$HASC_1)
lookup <- lookup.pri_div[!duplicated(lookup.pri_div$HASC_1), ]
countries <- merge(countries, lookup[,c("HASC_1", "CTY.STATE.CODE")],
                   by="HASC_1", all.x=T, all.y=F) # check state
countries <- merge(countries, lookup.cty[, c("A.3", "DL")],
                   by.x="GID_0", by.y="A.3", all.x=T, all.y=F) # check state
setDT(countries)[, c("geo_cty", "geo_state") := tstrsplit(CTY.STATE.CODE, "\\.")]
cols <- c("idx", "type.country.n", "type.state.n", "type.country.n.full",
          "type.state.n.full", "type.country", "type.state", "geo_cty", "geo_state", "DL",
          "type.locality.verbatim", "type.locality.updated", "check1", "check2", "geometry")
write.csv(countries[, ..cols], paste0(dir_data, 'clean/df-state-check.csv'), 
          na='', row.names=F, fileEncoding="UTF-8")


filepath <- paste0(dir_data, "clean/df-state-check_edit.csv")
add <- fread(filepath, integer64='character', na.strings=c(''), encoding='UTF-8')
add[, names(add) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
new_cols <- c("idx", "type.country.n_N", "type.state.n_N", 
             "lat_N", "lon_N")
add <- add[!duplicated(idx)][, ..new_cols]
add$idx <- as.character(add$idx)
add <- merge(add, lookup.cty[,c("DL", "Country", "GEC")],
             by.x="type.country.n_N", by.y="DL", all.x.T=, all.y=T)
names(add)[which(names(add)=="Country")] <- "type.country.n.full_N"
add$cty.state <- paste0(add$GEC, ".", add$type.state.n_N)
lookup <- lookup.pri_div[!duplicated(lookup.pri_div$CTY.STATE.CODE), 
                         c("CTY.STATE.CODE", "NAME_1")]

add <- merge(add, lookup,
             by.x="cty.state", by.y="CTY.STATE.CODE", all.x=T, all.y=F)
names(add)[which(names(add)=="NAME_1")] <- "type.state.n.full_N"
add$cty.state <- NULL; add$GEC <- NULL

df <- merge(df, add, by='idx', all.x=T, all.y=F)
df[!is.na(type.country.n_N)]$type.country.n.full <-
    df[!is.na(type.country.n_N)]$type.country.n.full_N
df[!is.na(type.country.n_N)]$type.state.n.full <- 
    df[!is.na(type.country.n_N)]$type.state.n.full_N
df[!is.na(type.country.n_N)]$type.country.n <- 
    df[!is.na(type.country.n_N)]$type.country.n_N
df[!is.na(type.country.n_N)]$type.state.n <- 
    df[!is.na(type.country.n_N)]$type.state.n_N
df[!is.na(type.country.n_N)]$lat <- 
    as.numeric(df[!is.na(type.country.n_N)]$lat_N)
df[!is.na(type.country.n_N)]$lon <- 
    as.numeric(df[!is.na(type.country.n_N)]$lon_N)
df[!is.na(type.country.n_N) & !(is.na(lat)|is.na(lon))]$flag <- 
    "COORDINATES_DOUBLE_CHECKED_TO_STATE_COUNTRY_MANUAL"
df[type.country.n_N == "REMOVE"]$type.country.n <- ""
df[,c("type.country.n_N", "type.state.n_N", "lat_N", "lon_N", "type.country.n.full_N", "type.state.n.full_N"):=NULL]
# *TODO: "[TODO]" states that must be manually done

# geocode blank lat lon from locality
check1 <- is.na(df$type.country.n.full) | df$type.country.n.full == ""
check2 <- is.na(df$type.state.n.full) | df$type.state.n.full == ""
check3a <- is.na(df$type.locality.verbatim) | df$type.locality.verbatim == "NA" | df$type.locality.verbatim == "" | grepl("\\[unknown\\]", df$type.locality.verbatim)
check3b <- is.na(df$type.locality.updated) | df$type.locality.updated == "NA" | df$type.locality.updated == "" | grepl("\\[unknown\\]", df$type.locality.updated)

df$search_locality <- df$type.locality.updated
df[check3b]$search_locality <- NA
df[check3b & !check3a]$search_locality <- df[check3b & !check3a]$type.locality.verbatim

df[!check1 & !check2]$search_locality <- 
    paste0(df[!check1 & !check2]$search_locality, ", ", 
           df[!check1 & !check2]$type.state.n.full, ", ",
           df[!check1 & !check2]$type.country.n.full) 
df[!check1 & check2]$search_locality <- 
    paste0(df[!check1 & check2]$search_locality, ", ",
           df[!check1 & check2]$type.country.n.full) 
df$check1 <- check1
df$check2 <- check2

# check1 <- is.na(df$lat) | is.na(df$lon)
# to_geocode <- df[check1 & !is.na(search_locality), c("idx", "search_locality")]
# geocoded <- geocode_lat_long(to_geocode$search_locality)
# geocoded2 <- cbind(to_geocode, geocoded)
# write.csv(geocoded2, paste0(dir_data, 'clean/df-geocoded.csv'), row.names=F)
df[,c("search_locality", "check1", "check2"):=NULL]

filepath <- paste0(dir_data, "clean/df-geocoded_edit.csv")
add <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
add[, names(add) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
new_cols <- c("idx", "lat", "lon")
add <- add[!duplicated(idx)][, ..new_cols][!is.na(lat)]
add$idx <- as.character(add$idx)
df <- merge(df, add, by='idx', all.x=T, all.y=F, suffixes=c("", "_N"))
df[!is.na(lat_N)]$lat <- as.numeric(df[!is.na(lat_N)]$lat_N)
df[!is.na(lat_N)]$lon <- as.numeric(df[!is.na(lat_N)]$lon_N)
df[!is.na(lat_N)]$flag <- "GEOCODED_GOOGLE_MAPS_API"
df[,c("lat_N", "lon_N"):=NULL]

# check country/state and lat/lon
check0 <- is.na(df$lat) | is.na(df$lon)

ll <- sf::st_as_sf(df[!check0,c("idx", "lat", "lon", "type.country.n", "type.state.n", 
                           "type.country.n.full", "type.state.n.full", 
                           "type.country", "type.state", 
                           "type.locality.verbatim", "type.locality.updated")], 
                   coords = c('lon', 'lat'),
                   crs = "+init=epsg:4326")

# derive new columns
countries_shp <- sf::st_join(ll, shp8, join = st_intersects)
countries <- data.table(data.frame(countries_shp))

lookup <- lookup.pri_div[!duplicated(lookup.pri_div$HASC_1),c("HASC_1", "CTY.STATE.CODE")]

countries <- merge(countries, lookup, by="HASC_1", all.x=T, all.y=F)
setDT(countries)[, c("country", "state") := tstrsplit(CTY.STATE.CODE, "\\.")]
countries$CTY.STATE.CODE <- NULL
countries <- countries[!duplicated(idx)]

countries$check.country <- countries$NAME_0 == countries$type.country.n.full
countries$check.country2 <- countries$type.country.n == countries$country
countries$check.state <- countries$NAME_1 == countries$type.state.n.full

write.csv(countries[!(check.country | check.country2) | !check.state],
          paste0(dir_data, "clean/df-check-state2.csv"), row.names=F)

add[idx==6986]
add[idx==21964]

filepath <- paste0(dir_data, "clean/df-check-state2_edit.csv")
add <- fread(filepath, integer64='character', na.strings=c(''), encoding='UTF-8')
add[, names(add) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
new_cols <- c("idx", "type.country.n_N", "type.state.n_N", 
             "lat_N", "lon_N")
add <- add[!duplicated(idx)][, ..new_cols]
add$idx <- as.numeric(add$idx)

add <- merge(add, lookup.cty[,c("DL", "Country", "GEC")],
             by.x="type.country.n_N", by.y="DL", all.x.T=, all.y=F)

names(add)[which(names(add)=="Country")] <- "type.country.n.full_N"
add$cty.state <- paste0(add$GEC, ".", add$type.state.n_N)
lookup <- lookup.pri_div[!duplicated(lookup.pri_div$CTY.STATE.CODE), 
                         c("CTY.STATE.CODE", "NAME_1")]
add <- merge(add, lookup,
             by.x="cty.state", by.y="CTY.STATE.CODE", all.x=T, all.y=F)
names(add)[which(names(add)=="NAME_1")] <- "type.state.n.full_N"
add$cty.state <- NULL; add$GEC <- NULL
add$idx <- as.character(add$idx)
df <- merge(df, add, by='idx', all.x=T, all.y=F)

df[!is.na(type.country.n_N)]$type.country.n.full <- 
    df[!is.na(type.country.n_N)]$type.country.n.full_N
df[!is.na(type.country.n_N)]$type.state.n.full <- 
    df[!is.na(type.country.n_N)]$type.state.n.full_N
df[!is.na(type.country.n_N)]$type.country.n <- 
    df[!is.na(type.country.n_N)]$type.country.n_N
df[!is.na(type.country.n_N)]$type.state.n <- 
    df[!is.na(type.country.n_N)]$type.state.n_N
df[!is.na(type.country.n_N)]$lat <- 
    as.numeric(df[!is.na(type.country.n_N)]$lat_N)
df[!is.na(type.country.n_N)]$lon <- 
    as.numeric(df[!is.na(type.country.n_N)]$lon_N)
df[!is.na(type.country.n_N) & !(is.na(lat)|is.na(lon))]$flag <- 
    "COORDINATES_DOUBLE_CHECKED_TO_STATE_COUNTRY_MANUAL"
df[type.country.n_N == "REMOVE"]$type.country.n  <- ""

df[,c("type.country.n_N", "type.state.n_N", 
        "lat_N", "lon_N",
        "type.country.n.full_N", "type.state.n.full_N"):=NULL]


# add flags
df$source.of.latlon.n <- ""
check0 <- is.na(df$lat) | is.na(df$lon)
check1 <- is.na(df$type.country.n) | df$type.country.n == ""
check2 <- (is.na(df$type.state.n) | df$type.state.n == "") & 
          (is.na(df$type.state.n.full) | df$type.state.n.full == "")
check3 <- is.na(df$source.of.latlon)  | df$source.of.latlon == ""
check4 <- is.na(df$flag) | df$flag == ""
check5 <- grepl("GEOCODED_GOOGLE_MAPS_API", df$flag)

df[check0 & check1]$source.of.latlon.n <- "0_NO_LAT_LON_AND_COUNTRY"
df[check0 & !check1 & check2]$source.of.latlon.n <- "1_NO_LAT_LON_AND_COUNTRY_ONLY"
df[check0 & !check1 & !check2]$source.of.latlon.n <- "2_NO_LAT_LON_AND_COUNTRY_AND_STATE"
df[!check0]$source.of.latlon.n <- "3_WITH_LAT_LON"

df[!check0 & !check3]$source.of.latlon.n <- 
    paste0(df[!check0 & !check3]$source.of.latlon.n, " & JSASCHER_ADDED")
df[!check0 & check3 & !check4 & check5]$source.of.latlon.n <- 
    paste0(df[!check0 & check3 & !check4 & check5]$source.of.latlon.n, " & EJYSOH_ADDED_GMAPS_API")
df[!check0 & check3 & !check4 & !check5]$source.of.latlon.n <- 
    paste0(df[!check0 & check3 & !check4 & !check5]$source.of.latlon.n, " & EJYSOH_ADDED_MANUAL")

df <- df[!duplicated(idx)]

write.csv(df[order(as.numeric(idx))], 
          paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019-idx-1-geocoded.csv"), na='', row.names=F, fileEncoding="UTF-8")

