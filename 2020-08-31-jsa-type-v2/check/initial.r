source('2020-08-31-jsa-type-v2/init/init.r')

# See raw files

list.files(v2_dir_data_raw_raw, pattern = "^Apoidea*.*.\\.xlsb")


# Read excel files

file <- paste0(v2_dir_data_raw, v2_basefile, "_2 map.csv")
df <- read_escaped_data_v2(file)

dim(df)



# Tables to asssess:
# Species [type locality]
# Global mapper 
# Publications 
# Authors
# Collectors


# Species ----------------------------------------------------------------------

########### Species status

# Which are the exclusion criteria for rows?

# With "z" or numbers only 
sum(df$genus == "z")
sum(grepl("[0-9]", df$genus))

# To exclude: Morphospecies
# Species level: Nominate subspecies (as species), Valid species
# Below species: Valid subspecies
# Invalid species:  Synonym
table(df$status)

# Exclude duplicates when repeated between lists, see below


# Are the species names duplicated?

# Yes they are duplicated between lists, these need to be removed
# Yes there are duplicates within lists as well (but few)
dim(df[duplicated(paste0(genus, " ", species)),])
dups <- df[duplicated(paste0(genus, " ", species)), c("genus", "species")]

fwrite(
    df[paste0(genus, " ", species) %in% paste0(dups$genus, " ", dups$species), 
        .N,
        c("genus", "species", "status", "file")][order(genus, species, file)],
    paste0(v2_dir_data_raw, 'test.csv')
)

# They need to be combined by global mapper (to identify distribution of sp.)
# or use merged global mapper field.


# Can homonyms be valid species, and should we count them?

# For now, we will count them as valid species
df[grepl("homonym", species) & tolower(status) != "synonym", 
    .N, 
    by=c("species", "genus")]
df[grepl("homonym", species) & tolower(status) != "synonym" & 
    grepl("valid species", tolower(status)), .N, by=c("species", "genus")][
        N>=2
    ]

# How blank synonyms can be resolved?
# Through valid.genus.species.subspecies



########### Locality

# Are lat/ lon with non-standard characters?
df$lat_n <- as.numeric(df$lat)
df$lon_n <- as.numeric(df$lon)

df[(is.na(lat_n) & lat != "") |
   (is.na(lon_n) & lon != ""), c("idx", "lat", "lon")]
# Replace commas with full stop (without space), 
# split text for those with comma, remove equal signs


# Are lat/ lon within min and max range?
df[(abs(lat_n) > 90 | abs(lon_n) > 180), 
    c("file", "genus", "species", "lat_n", "lon_n") ]

# Are lat/ lon in land
df_geo <- df[!(is.na(lat_n) | is.na(lon_n) )]
df_geo <- df[!(abs(lat_n) > 90 | abs(lon_n) > 180)]
v_df <- st_as_sf(df_geo, coords = c("lon_n", "lat_n"), crs = wgs84)
v_df <- st_join(v_df, v_ecoregions, join = st_intersects)

table(v_df$REALM)
data.table(
    v_df[is.na(v_df$REALM) | v_df$REALM == "N/A", c("file", "genus", "species")]
)

# Are lat/lon in type country
v_df <- st_join(v_df, v_continent, join = st_intersects)

table(is.na(v_df$NAME_0))

data.table(
    v_df[is.na(v_df$NAME_0) & 
         !(is.na(v_df$REALM) | v_df$REALM == "N/A"), 
         c("genus", "species")]
)

df_country <- data.table(v_df)

df_country$type.country_n <- gsub("\\:", "", df_country$type.country)

df_country <- merge(
    df_country, lp_country[, c("DL", "A-3")],
    by.x = "type.country_n", by.y = "DL",
    all.x = T, all.y = F
)

df_country <- merge(
    df_country, lp_dl[, c("DL", "GID_0_owner")],
    by.x = "type.country_n", by.y = "DL",
    all.x = T, all.y = F
)

df_country[!is.na(type.country_n) & !is.na(GID_0_owner), 
    c("idx", "type.country_n", "A-3", "GID_0_owner", "GID_0")]

df_country[!is.na(type.country_n) & !is.na(GID_0_owner), ]$`A-3` <-
    df_country[!is.na(type.country_n) & !is.na(GID_0_owner), ]$`GID_0_owner`

# type.country_n = type country from dataset in "DL" format
# A-3 = 3 letter code from "type.country_n"
# GID_0_owner = based on type.country_n
# GID_0 = spatially joined based on lat/ lon

# No country assigned to lat/lon

check <- data.table(
    df_country[is.na(GID_0) & !is.na(`A-3`) &
         !(is.na(REALM) | REALM == "N/A"), ]
)

check[, c("idx", "genus", "species", "A-3", "GID_0", "lat", "lon")]

# With country

check <- data.table(
    df_country[!is.na(GID_0) & 
         !(is.na(REALM) | REALM == "N/A"), ]
)

table(check$`A-3` == check$`GID_0`)

check[`A-3` != GID_0, 
      c("idx", "file", "genus", "species", "lat", "lon", "A-3")]

