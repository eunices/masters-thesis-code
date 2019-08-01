source('2019-06-19-ascher-type-data/init.R')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - quick formatting
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- quick formatting"))

# read dataset with index
filepath <- paste0(dir, '2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_1-idx.csv')
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes
# csv double quotes are escaped by \\"\\", fread reads them as "" instead of "


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

print(paste0(Sys.time(), " --- Dataset read is ", dim(df)[1], " rows and ", dim(df)[2], " cols"))

# initialize flag column
df$flag <- '' 

# keep a list of original column names
df_original_cols <- names(df) # should be 94 cols

# remove line carriages (otherwise funky things will happen with write.csv)
df[] <- lapply(df, gsub, pattern='[\r\n]', replacement=' ')

# check number of NA
na_count <- data.frame(names=names(df), N=sapply(df, function(x) sum(length(which(is.na(x))))), row.names=NULL)
print(paste0(Sys.time(), " --- Number of NA")); print(na_count[order(-na_count$N), c("names", "N")][1:10,])

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - geocoding + quick lat lon checks
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- geocoding + lat lon checks"))

# + geocoding and manually adding the missing lat long
missing_lat_long_check <- is.na(df$lat) | is.na(df$lon)
table(is.na(df$lat)); table(is.na(df$lon)); table(missing_lat_long_check)

# write to view data
write.csv(df[missing_lat_long_check], paste0(dir, "clean/", "check1.csv"),
           fileEncoding="UTF-8")

# subset data to geocode
geocode <- df[missing_lat_long_check,]

# clean country field - extract words outside of square brackets
cty_raw <- gsub(" ", "", gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", geocode$type.country)) 
# check certainty of country
certain_country <- geocode$type.country == cty_raw
# create new field for cleaned countries
geocode$type.country.n <- geocode$type.country
geocode[!certain_country]$type.country.n <- cty_raw[!certain_country] 
table(!certain_country & geocode$type.country.n == "") # 337 which have uncertain country
 # add flag
geocode[!certain_country & geocode$type.country.n == ""]$flag <- "UNCERTAIN_COUNTRY"

# ignore those with no information on type.country and type.state, or type.locality.updated
missing_country <- 
    geocode$type.country == '[unknown]' | geocode$type.country.n == ""
missing_state <- 
    geocode$type.state == '[unknown]' | is.na(geocode$type.state) | geocode$type.state == ""
missing_locality <- 
    geocode$type.locality.updated == '[unknown]' | is.na(geocode$type.locality.updated) | geocode$type.locality.updated == ""
table((missing_state & missing_locality)); table((missing_country))
table((missing_country) | (missing_state & missing_locality)) # 1595 records with no location
no_geocode <- geocode[(missing_country | (missing_state & missing_locality)),] # subset

missing_country <- 
    no_geocode$type.country == '[unknown]' | no_geocode$type.country.n == ""
missing_state <- 
    no_geocode$type.state == '[unknown]' | is.na(no_geocode$type.state) | no_geocode$type.state == ""
missing_locality <- 
    no_geocode$type.locality.updated == '[unknown]' | is.na(no_geocode$type.locality.updated) | no_geocode$type.locality.updated == ""
no_geocode[(missing_state & missing_locality),]$flag <- 'COUNTRY_ONLY' # add flags
no_geocode[missing_country,]$flag <- 'NO_COUNTRY'
no_geocode <- no_geocode[,c('idx', 'flag')]


# actual geocoding
# # =================
# # DONE ONCE ONLY ##
# # =================
# # subset columns
# to_geocode <- geocode[!(missing_country | (missing_state & missing_locality)), 
#                       c('idx', 'type.locality.updated', 'type.country.n', 'type.state')]
# # merge country by GEC
# to_geocode <- merge(to_geocode, lookup.cty, 
#                     by.x="type.country.n", by.y="GEC", all.x=T, all.y=F, suffix=c(1, 2))
# # merge country by A.2
# to_geocode <- merge(to_geocode, lookup.cty, by.x="type.country.n", by.y="A.2", all.x=T, all.y=F, suffix=c(2, 3))
# # convert to character
# to_geocode[] <- lapply(to_geocode, as.character)
# # obtain best possible country name
# to_geocode$Country2 <- ifelse(is.na(to_geocode$Country2),
#                               to_geocode$Country3,
#                               to_geocode$Country2)
# # create new field search_locality
# to_geocode$search_locality <- to_geocode$type.locality.updated
# to_geocode[!is.na(to_geocode$type.state)]$search_locality  <- 
#     paste0(to_geocode[!is.na(to_geocode$type.state)]$type.state, ", ", 
#            to_geocode[!is.na(to_geocode$type.state)]$type.locality.updated)
# to_geocode[!is.na(to_geocode$Country)]$search_locality  <- 
#     paste0(to_geocode[!is.na(to_geocode$Country)]$Country, ", ", 
#            to_geocode[!is.na(to_geocode$Country)]$search_locality)
# # geocoding row by row
# geocoded <- data.frame(lat=as.numeric(), lon=as.numeric())
# for (i in 1:length(to_geocode$search_locality)) {
#     if (to_geocode$search_locality[i] == "" | is.na(to_geocode$search_locality[i])) {
#         geocoded <- rbind(geocoded, data.frame(lat=NA, lon=NA))
#         print(paste0("Not geocoded for row ", i))
#     } else {
#         geocoded_row <- try(geocode(to_geocode$search_locality[i]))
#         geocoded_row <- as.data.frame(geocoded_row)
#         geocoded <- rbind(geocoded, geocoded_row)
#         print(paste0("Geocoded for row ", i))
#     }
# }
# to_geocode$lat <- geocoded$lat; to_geocode$lon <- geocoded$lon
# to_geocode$flag <- "GEOCODED_GOOGLE_MAPS_API"
# # persist so that geocoding is only done once!
# write.csv(to_geocode, paste0(dir, "clean/", "geocoded.csv"), fileEncoding="UTF-8")

# merge from geocoding
geocoded_all <- read.csv(paste0(dir, "clean/", "geocoded.csv"), stringsAsFactors=F, encoding='UTF-8')
geocoded_na <- read.csv(paste0(dir, "clean/", "geocoded_na_edit.csv"), stringsAsFactors=F, encoding="UTF-8")
# files with "_edit" suffix means they have gone through manual editing
# geocoded_na were the na lat long with locality modified

names(geocoded_all) # fields needed: idx, lat, lon, flag
names(geocoded_na) # fields needed: idx, lat, long, flag, country?

table(geocoded_na$flag)
geocoded_na_check <- is.na(geocoded_na$lat) | is.na(geocoded_na$lon)
table(geocoded_na_check) # 193 were not geocoded

geocode_lat_long <- function(to_geocode) {
    # to_geocode is a list of localities
    # doing it one locality at a time saves the dataframe if the geocode api does not work
    geocoded <- data.frame(lat=as.numeric(), lon=as.numeric())
    for (i in 1:length(to_geocode)) {
        if (to_geocode[i] == "" | is.na(to_geocode[i])) {
            geocoded <- rbind(geocoded, data.frame(lat=NA, lon=NA))
            print(paste0("Not geocoded for row ", i))
        } else {
            geocoded_row <- try(geocode(to_geocode[i]))
            geocoded_row <- as.data.frame(geocoded_row)
            geocoded <- rbind(geocoded, geocoded_row)
            print(paste0("Geocoded for row ", i))
        }
    }
    geocoded
}

# 1. for NAs - look through NAs and apply flags and try to recover more lat long
flag <- "LOCALITY_MANUALLY_CHECKED_AMENDED_LOCALITY_GEOCODED_AGAIN" # for this flag, all should lack lat lon
# # =================
# # DONE ONCE ONLY ##
# # =================
# geocoded_na_to_geocode <- as.character(geocoded_na[geocoded_na$flag==flag,]$search_locality_updated)
# geocoded_na_geocoded <- geocode_lat_long(geocoded_na_to_geocode)
# geocoded_na[geocoded_na$flag==flag,]$lat <- geocoded_na_geocoded$lat
# geocoded_na[geocoded_na$flag==flag,]$lon <- geocoded_na_geocoded$lon
# write.csv(geocoded_na, paste0(dir, "clean/", "geocoded_na2.csv"),
#           fileEncoding="UTF-8" ) # manual edits required to check again for unsuccessful cases
geocoded_na <- read.csv(paste0(dir, "clean/", "geocoded_na2_edit.csv"), stringsAsFactors=F,
                        encoding="UTF-8")

# 2. for NAs - geocode those with COUNTRY AND PRI DIV ONLY
flag <- "COUNTRY_AND_PRI_DIV_ONLY"
# # =================
# # DONE ONCE ONLY ##
# # =================
# geocoded_na[grepl(flag, geocoded_na$flag),]$lat; geocoded_na[grepl(flag, geocoded_na$flag),]$lon
# geocoded_na_pri_div_to_geocode <- 
#     as.character(geocoded_na[grepl(flag, geocoded_na$flag),]$search_locality_updated)
# geocoded_na_pri_div_geocoded <- geocode_lat_long(geocoded_na_pri_div_to_geocode)
# table(is.na(geocoded_na_pri_div_geocoded$lat)); table(is.na(geocoded_na_pri_div_geocoded$lon))
# geocoded_na[grepl(flag, geocoded_na$flag),]$lat <- geocoded_na_pri_div_geocoded$lat
# geocoded_na[grepl(flag, geocoded_na$flag),]$lon <- geocoded_na_pri_div_geocoded$lon
# write.csv(geocoded_na, paste0(dir, "clean/", "geocoded_na3.csv"), fileEncoding="UTF-8")
geocoded_na <- read.csv(paste0(dir, "clean/", "geocoded_na3.csv"), stringsAsFactors=F, 
                        encoding="UTF-8")
                        

# ensure consistency in NA values
flag1 <- 'COUNTRY_ONLY'
flag2 <- 'COUNTRY_ONLY_UNCERTAIN_STATE'
flag3 <- 'UNKNOWN_LOCALITY'
check <- geocoded_na$flag == flag1 | geocoded_na$flag == flag2 | geocoded_na$flag == flag3
geocoded_na[check,]$lat <- NA
geocoded_na[check,]$lon <- NA
geocoded_na[] <- lapply(geocoded_na, as.character)
# write.csv(geocoded_na, 'tmp/test.csv', fileEncoding="UTF-8") # looks good

# combine na to geocoded (for 1st time success)
geocoded_final <- merge(geocoded_all, geocoded_na, by='idx', suffix=c('', '_new'), all.x=T, all.y=T)
geocoded_final[] <- lapply(geocoded_final, as.character)
geocoded_final_cols <- c('idx', 'type.locality.updated', 'search_locality_updated',
                         'lat', 'lon', 'lat_new', 'lon_new', 'flag', 'flag_new')
geocoded_final_used <- geocoded_final[, geocoded_final_cols]
geocoded_final_used[] <- lapply(geocoded_final_used, as.character)

# update type.locality.update if type locality is NA
check2 <- 
    is.na(geocoded_final_used$type.locality.updated) | 
    geocoded_final_used$type.locality.updated == 'NA' 

geocoded_final_used[check2,]$type.locality.updated <- geocoded_final_used[check2,]$search_locality_updated

# update type.locality.updated if not the same
check1 <- 
    (geocoded_final_used$type.locality.updated != geocoded_final_used$search_locality_updated) &
    !is.na(geocoded_final_used$search_locality_updated)

geocoded_final_used[check1,]$type.locality.updated <- geocoded_final_used[check1,]$search_locality_updated

# update flag
geocoded_final_used$flag <- 
    ifelse(is.na(geocoded_final_used$flag_new) | 
           geocoded_final_used$flag_new == 'NA',
           geocoded_final_used$flag, geocoded_final_used$flag_new)

# update lat
geocoded_final_used$lat <- 
    ifelse(is.na(geocoded_final_used$lat),
    geocoded_final_used$lat_new, geocoded_final_used$lat)

# update lon
geocoded_final_used$lon <- 
    ifelse(is.na(geocoded_final_used$lon),
           geocoded_final_used$lon_new, geocoded_final_used$lon)

# subset relevant columns
geocoded_final_final <- geocoded_final_used[,c('idx', 'type.locality.updated',
                                               'lat', 'lon', 'flag')]
geocoded_final_final[] <- lapply(geocoded_final_final, as.character)
# write.csv(geocoded_final_used, 'tmp/test.csv', fileEncoding="UTF-8") # looks good

# merge back geocoded information into main data frame
df$idx <- as.character(df$idx)
df_merged <- merge(df, geocoded_final_final, by='idx', suffix=c('', '_new'), all.x=T, all.y=T)
missing_lat_long_check2 <- !(is.na(df_merged$lat) | is.na(df_merged$lon)); table(missing_lat_long_check2)
df_merged[!missing_lat_long_check2,]$lat <- df_merged[!missing_lat_long_check2,]$lat_new
df_merged[!missing_lat_long_check2,]$lon <- df_merged[!missing_lat_long_check2,]$lon_new
df_merged[!missing_lat_long_check2,]$flag <- df_merged[!missing_lat_long_check2,]$flag_new
df_merged[!missing_lat_long_check2,]$type.locality.updated <- 
    df_merged[!missing_lat_long_check2,]$type.locality.updated_new
df <- df_merged[,..df_original_cols]; dim(df)

# merge back those not possible to geocode into main dataframe
no_geocode$idx <- as.character(no_geocode$idx)
df_merged <- merge(df, no_geocode, by='idx', suffix=c('', '_new'), all.x=T, all.y=T)
df_merged[!is.na(df_merged$flag_new),]$flag <- df_merged[!is.na(df_merged$flag_new),]$flag_new
df <- df_merged[, ..df_original_cols]; dim(df)


# + clean lat long

# convert to numeric
df$lat.n <- as.numeric(df$lat); df$lon.n <- as.numeric(df$lon)
# write.csv(df, 'tmp/test.csv', fileEncoding="UTF-8") # looks good

# NOTE: This section is manual - requires visual checks!
# checking why NA coercion was necessary
df[is.na(lat.n) & (is.na(flag) | flag ==""), "lat"]
df[is.na(lat.n) & (is.na(flag) | flag ==""), "lat.n"] <- c(NA, 34.0751, 45.146) # manual correction
df$lat <- df$lat.n; df[, lat.n:=NULL]
df[is.na(lon.n) & (is.na(flag) | flag ==""), "lon"]
df[is.na(lon.n) & (is.na(flag) | flag ==""), "lon.n"] <- c(NA) # manual correction
df$lon <- df$lon.n; df[, lon.n:=NULL]

# checking for out of range lat long
df[df$lon < -180, "lon"] <- -70.617721 # manual correction
df[df$lat < -90, "lat"] <- -21.3558    # manual correction

df <- df[order(as.numeric(idx))]
write.csv(df, 
          paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_2-geocoded.csv"),
          na='', row.names=F, fileEncoding="UTF-8") # write.csv escapes double quotes PROPERLY


df <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_2-geocoded.csv"),
            na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes
# csv double quotes are escaped by \\"\\", fread reads them as "" instead of "

# + check country and lat lon
# # =================
# # DONE ONCE ONLY ##
# # =================
# df_original <- df
# df <- df[!(is.na(lat) | is.na(lon))]
# # Create sf point object
# ll <- sf::st_as_sf(df, coords = c('lon', 'lat'),
#                    crs = "+init=epsg:4326")
# # derive new columns
# countries <- sf::st_join(ll, shp, join = st_intersects)
# countries <- merge(countries, lookup.cty, by.x="type.country", by.y="GEC")
# # check country
# cols <- c("idx", "GID_0", "A.3")
# check <- countries[,cols]
# check[,cols] <- lapply(check[,cols], as.character)
# check$checks <- check$GID_0 == check$A.3
# discrepancies <- check[!(check$checks | is.na(check$checks)),]; discrepancies
# table(discrepancies$checks)
# to_write <- countries[countries$idx %in% discrepancies$idx, 
#                       c('idx', 'NAME_0', 'Country', 'type.locality.verbatim', 'type.locality.updated',
#                         'lat', 'lon', 'flag')]
# write.csv(to_write, paste0(dir, "clean/", "check2.csv"), row.names=F, fileEncoding='UTF-8')

# Integrate discrepancies
corrected <- fread(paste0(dir, "clean/", "check2_edit.csv"), 
                   encoding='UTF-8', stringsAsFactors=F); dim(corrected)
corrected[, names(corrected) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
unique(corrected$flag.country)
unique(corrected$flag.amended)

# ignore "correct"
corrected <- corrected[flag.country!="correct",]; dim(corrected)
suffix <- '_MERGED'
df_merge <- merge(df, corrected, by='idx', suffix=c('', suffix), all.x=T, all.y=F)

# contains "geocoded wrongly" + "GEOCODE_ERRONEOUS_LAT_LONG_ADDED_MANUALLY"
# --> copy lat.corrected long.corrected flag.amended
check1 <- grepl("geocoded wrongly", df_merge$flag.country)
check2 <- df_merge$flag.amended == "GEOCODE_ERRONEOUS_LAT_LONG_ADDED_MANUALLY" 
unique(df_merge[check1 & check2]$flag.country)
df_merge[check1 & check2]$lat <- df_merge[check1 & check2]$lat.corrected
df_merge[check1 & check2]$lon <- df_merge[check1 & check2]$lon.corrected
df_merge[check1 & check2]$flag <- df_merge[check1 & check2]$flag.amended

# "geocoded wrongly" + "COUNTRY_ONLY"
# --> copy flag.amended
check1 <- df_merge$flag.country == "geocoded wrongly"
check2 <- df_merge$flag.amended == "COUNTRY_ONLY" 
unique(df_merge[check1 & check2]$flag.country); table(check1); table(check2)
df_merge[check1 & check2]$flag <- df_merge[check1 & check2]$flag.amended
df_merge[check1 & check2]$lat <- NA
df_merge[check1 & check2]$lon <- NA

# "geocoded wrongly" + contains "COUNTRY_AND_PRI_DIV_ONLY"
# --> copy lat.corrected long.corrected flag.amended
check1 <- df_merge$flag.country == "geocoded wrongly"
check2 <- grepl("COUNTRY_AND_PRI_DIV_ONLY", df_merge$flag.amended)
unique(df_merge[check1 & check2]$flag.amended)
df_merge[check1 & check2]$lat <- df_merge[check1 & check2]$lat.corrected
df_merge[check1 & check2]$lon <- df_merge[check1 & check2]$lon.corrected
df_merge[check1 & check2]$flag <- df_merge[check1 & check2]$flag.amended

# contains "lat long from original dataset incorrect"
# --> copy lat.corrected long.corrected flag.amended
check1 <- grepl("lat long from original dataset incorrect", df_merge$flag.country)
unique(df_merge[check1]$flag.amended)
df_merge[check1]$lat <- df_merge[check1]$lat.corrected
df_merge[check1]$lon <- df_merge[check1]$lon.corrected
df_merge[check1]$flag <- df_merge[check1]$flag.amended

# contains "type.country wrong" 
# --> copy country.corrected.code flag.amended AND 
#     country.corrected.pri.province if it exists
check1 <- grepl("type.country wrong", df_merge$flag.country)
check2 <- df_merge$country.corrected.pri.province != ""
unique(df_merge[check1]$flag.amended)
unique(df_merge[check1&check2]$country.corrected.pri.province) # I mean primary div here
df_merge[check1]$type.country <- df_merge[check1]$country.corrected.code
df_merge[check1]$flag <- df_merge[check1]$flag.amended
df_merge[check1 & check2]$type.state <- df_merge[check1 & check2]$country.corrected.pri.province

# "gadm shp boundaries wrong; boundaries close"
# --> copy flag.amended
check1 <- grepl("gadm shp boundaries wrong; boundaries close", df_merge$flag.country)
unique(df_merge[check1]$flag.amended)
df_merge[check1]$flag <- df_merge[check1]$flag.amended

# done merging
df <- df_merge[,..df_original_cols]

# # =================
# # DONE ONCE ONLY ##
# # =================
# # sanity checks and geocode again
# check1 <- is.na(df$lat) | is.na(df$lon)
# check2 <- df$flag == "COUNTRY_ONLY" | df$flag == "COUNTRY_ONLY_UNCERTAIN STATE" | df$flag == "" | is.na(df$flag)
# check3 <- !(grepl("\\[|unknown|\\?", df$type.state) | is.na(df$type.state))
# df[check1 & check2 & check3]$flag <- 'COUNTRY_AND_PRI_DIV_ONLY'

# check1 <- grepl("COUNTRY_AND_PRI_DIV_ONLY", df$flag)
# check2 <- is.na(df$lat) | is.na(df$lon)
# search <- df[check1 & check2, c("idx", "type.country", "type.state")]
# search[search$type.state == "VIC"]$type.state <- "VI"
# search[search$type.state == "BSAS"]$type.state <- "BA"
# search[search$type.state == "MZA"]$type.state <- "MZ"
# search[search$type.country == "TW" & search$type.state == "KS"]$type.state <- "KH"
# search[search$type.country == "UP" & search$type.state == "CR"]$type.state <- ""
# search[search$type.country == "IT" & search$type.state == "LB"]$type.state <- "LM"

# search$type.country.state <- paste0(search$type.country, ".", search$type.state)
# search <- merge(search, lookup.pri_div[,c("NAME_0", "NAME_1", "CTY.STATE.CODE")],
#                 by.x="type.country.state", by.y="CTY.STATE.CODE", all.x=T, all.y=F)

# searched <- geocode_lat_long(paste0(search$NAME_0, ", ", search$NAME_1))
# search$lat <- searched$lat; search$lon <- searched$lon

# countries <- c("China", "Brazil", "Australia", "Greenland")
# search$flag <- ""
# search[search$NAME_0 %in% countries]$flag <- "COUNTRY_AND_PRI_DIV_ONLY_LRG_DIV"
# search[!(search$NAME_0 %in% countries)]$flag <- "COUNTRY_AND_PRI_DIV_ONLY_SML_DIV"
# search[is.na(search$NAME_0)]$flag <- "COUNTRY_ONLY"

# write.csv(search, 
#           paste0(dir, "clean/geocoded2.csv"), 
#           fileEncoding="UTF-8", row.names=F)

search <- read.csv(paste0(dir, "clean/geocoded2.csv"), encoding="UTF-8", stringsAsFactors=F)

df$idx <- as.numeric(df$idx)
search$idx <- as.numeric(search$idx)
df_merged <- merge(df, search, by="idx", all.x=T, all.y=T, suffix=c('', '_MERGED'))
df_merged[!is.na(df_merged$lat_MERGED)]$flag <- df_merged[!is.na(df_merged$lat_MERGED)]$flag_MERGED
df_merged[!is.na(df_merged$lat_MERGED)]$lat <- df_merged[!is.na(df_merged$lat_MERGED)]$lat_MERGED
df_merged[!is.na(df_merged$lat_MERGED)]$lon <- df_merged[!is.na(df_merged$lat_MERGED)]$lon_MERGED
df_merged[!is.na(df_merged$lat_MERGED)]$type.state <- df_merged[!is.na(df_merged$lat_MERGED)]$type.state_MERGED
df_merged[!is.na(df_merged$lat_MERGED)]$type.country <- df_merged[!is.na(df_merged$lat_MERGED)]$type.country_MERGED

# done merging
df <- df_merged[,..df_original_cols]

checks <- fread(paste0(dir, "clean/lat_long_check.csv"))
remove_lat_long_idxs <- checks[comment == "Lat long removed"]$idx
df[idx %in% remove_lat_long_idxs]$lat <- ""
df[idx %in% remove_lat_long_idxs]$lon <- ""

modify_lat_lon_idxs <- checks[comment == "Geocode modified"]$idx
for (i in length(modify_lat_lon_idxs)) {
    id <- modify_lat_lon_idxs[i]
    print(paste0("Modifying idx ", id))
    df[idx==id]$lat <- checks[idx==id]$lat
    df[idx==id]$lon <- checks[idx==id]$lon
}

# check1 <- 
#     df$type.locality.update == "" | df$type.locality.update == "[unknown]" | is.na(df$type.locality.update) |
#     df$type.locality.update == "\\[NW Mongolia\\]" | 
#     df$type.locality.update == "\\[N Italy\\]" |
#     df$type.locality.update == "\\[N Spain\\]" |
#     df$type.locality.update == "\\[S Spain\\]" |
#     df$type.locality.update == "\\[S Germany\\]" |
#     df$type.locality.update == "\\[S of France\\]" |
#     df$type.locality.update == "\\[provinces of Florence or Pisa\\]" |
#     df$type.locality.update == "\\[N Germany\\]" |
#     df$type.locality.update == "southern Brasil" |
#     df$type.locality.update == "\\[S Germany\\]" |
#     df$type.locality.update == "Utai" |
#     df$type.locality.update == "\\[\\"Assam\\"\\]" |
#     df$type.locality.update == "\\[Upper Egypt\\]" |
#     df$type.locality.update == "German East Africa \\[Afrique Oriental Allemand or Deutsch Ost-Afrika\\]" |
#     df$type.locality.update == "\\[Borneo\\]" |
#     df$type.locality.update == "central Japan" |
#     df$type.locality.update == "northern Mongolia" |
#     df$type.locality.update == "S Transbaikal" |
#     df$type.locality.update == "Canton, Pestacho bei Tsiuwangtau" |
#     df$type.locality.update == "Amx" |
#     df$type.locality.update == "\\[NE Sichuan\\]" |
#     df$type.locality.update == "\\[England\\]" |
#     df$type.locality.update == "Ramat" |
#     df$type.locality.update == "between Seziwa and Kampala" |
#     df$type.locality.update == "56.34" |
#     df$type.locality.update == "\\[Seaford \\(two syntypes\\)\\]" |
#     df$type.locality.update == "NW coast \\[Nicol Bay, Swan River, or Champion Bay\\]" |
#     df$type.locality.update == "\\[Kimberly\\]" |
#     df$type.locality.update == "\\[Arabia\\]" |
#     df$type.locality.update == "\\[Central Australia\\]" |
#     df$type.locality.update == "\\[interior of South Africa\\]" |
#     df$type.locality.update == "\\"Palm\\"" 
# df[check1]$lat <- NA
# df[check1]$lon <- NA

check0 <- df$flag == "" | is.na(df$flag)
check1 <- df$country == "" | is.na(df$country)
check2 <- is.na(df$lat) | is.na(df$lon)
check3 <- df$type.state == "" | is.na(df$type.state)
check4 <- grepl("CN|AS|BR|CA", df$type.country)
df[check0&check1&!check2]$flag <- "NO_COUNTRY"
df[check0&check1&check2&check3]$flag <- "COUNTRY ONLY"
df[check0&check1&check2&!check3&check4]$flag <- "COUNTRY_AND_PRI_DIV_ONLY_LRG_DIV"
df[check0&check1&check2&!check3&!check4]$flag <- "COUNTRY_AND_PRI_DIV_ONLY_SML_DIV"

# df[df$type.locality.updated == "0"]$type.locality.updated <- ''
df <- df[order(as.numeric(idx))]
write.csv(df, 
          paste0(dir, 
                "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3-clean-lat-lon.csv"),
          na='', row.names=F, fileEncoding="UTF-8")

filepath <- paste0(dir, 
                   "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3-clean-lat-lon.csv")
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - check duplicated rows & make new cols
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean date column"))

# duplicates rows
gs <- paste0(df$genus, df$species)
df$duplicated.row <- duplicated(gs)

# date
df$date.n <- as.numeric(gsub("\\[.*\\]", "", df$date)) # remove square brackets
df[is.na(date.n)]$date.n <- as.numeric(sub("\\D*(\\d+).*", "\\1", df[is.na(date.n)]$author.date))

# date.of.type
df$date.of.type.string <- paste0("'", df$date.of.type)
df$date.of.type.dd <- as.numeric(sub("\\D*(\\d+).*", "\\1", df$date.of.type))
df[df$date.of.type.dd>31,]$date.of.type.dd <- NA

df$date.of.type.mm <- 
    gsub(".*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec).*", "\\1", df$date.of.type)
df[!df$date.of.type.mm %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),]$date.of.type.mm <- ""

df$date.of.type.yyyy <- as.numeric(sub('.*(\\d{4}).*', '\\1', df$date.of.type))

paste_nine = function(numeric){
    char <- strsplit(as.character(numeric), "")[[1]]
    word <- paste0(char[1], "9", char[3], char[4])
    as.numeric(word)
}

df[df$date.of.type.yyyy<1200]$date.of.type.yyyy[] <- 
    lapply(df[df$date.of.type.yyyy<1200]$date.of.type.yyyy, function(x) paste_nine(x)[1])

# checks
# date_cols <- names(df)[grepl("date.of.type", names(df))]
# tmp <- df[,..date_cols]
# tmp$check <- ifelse(tmp$date.of.type.yyyy == tmp$date.of.type.yyyy2, 1, 0)
# tmp$check2 <- ifelse(!is.na(tmp$date.of.type.mm) & tmp$date.of.type.mm %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 1, 0)

# date differences
df$years.lag <- df$date.n - df$date.of.type.yyyy

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - useful columns
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- subset useful columns"))

# Subset useful columns
main <- c('idx',                                       # identifier
          'family', 'subfamily', 'tribe',              # taxonomic info
          'genus', 'subgenus', 'species', 'full.name',
          'author', 'date.n', 'full.name.of.describer',  # description info
          'collector.of.type', 'date.of.type.string',           # collector info
          'date.of.type.yyyy', 'date.of.type.mm', 'date.of.type.dd',
          'years.lag',
          'lat', 'lon',                                  # georeference
          'type.repository', 'country.of.type.repository', 'status', # type info
          'flag', 'duplicated.row') 

loc <- c('type.country', 'type.state',     # locality info
         'type.locality.verbatim', 'type.locality.updated', 'elev.m',
         'source.of.latlon')

checks <- c('date.of.type.string', 
            'other.names', 'original.genus',                             # synonyms
            'host.plant', 'host.plant.of.type', 'host.insect.or.prey', 'type.sex', # life history info
            'sociality',
            'type.status', 'type.depository.notes', 'notes',             # specimen status
            'title', 'journal', 'volume', 'issue', 'paper.type',         # publication info
            'global.mapper', 'distributional.footnotes', 'notes')        # distribution info


cols <- c(main, loc, checks)

df <- df[duplicated.row == "FALSE"][order(as.numeric(idx))]
write.csv(df[,..cols], 
          paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.1-useful-col.csv"), na='', row.names=F, fileEncoding="UTF-8")

