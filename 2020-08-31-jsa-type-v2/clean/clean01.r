# Purpose: filter species / taxonomic ranks / species status / 
# format dates / clean lat & lon & locality related info

source('2020-08-31-jsa-type-v2/init/init.r')

# Read 

file <- paste0(v2_dir_data_raw, v2_basefile, "_2.csv")
df <- read_escaped_data_v2(file)


# Species statuses -------------------------------------------------------------

# Remove "z"
df <- df[!(genus == "z" | status == "z"), ]


# Remove numbers
df <- df[!grepl("[0-9]", genus)]


# Remove duplicated valid species
is_dup <- duplicated(
    df[, c("genus", "species", "author", "date", "status")]
)

df <- df[!is_dup]


# Remove morphospecies
df <- df[status != "Morphospecies"]


# Remove those with uncertain family
df <- df[family != "Uncertain"]
df <- df[!grepl("\\?", family)]


# Taxonomic ranks --------------------------------------------------------------

# Tribe with None and Uncertain
df[tolower(tribe) %in% c("none", "uncertain"), .N, tribe] 
#!CHECK: tribe with none or uncertain

# Genus with uncertainty
df[grepl("\\?", tolower(genus)), .N, genus] 
#!CHECK: genus with uncertain placement

# Format dates -----------------------------------------------------------------

# Collection dates

# Year
df$date.of.type.yyyy <- 
    as.integer(sub('.*(\\d{4}).*', '\\1', df$date.of.type))

# Month
df$date.of.type.mm <- gsub(
    ".*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec).*", 
    "\\1", 
    df$date.of.type
)

df[!df$date.of.type.mm %in% c("Jan", "Feb", "Mar", "Apr", "May", 
                              "Jun", "Jul", "Aug", "Sep", "Oct", 
                              "Nov", "Dec"),]$date.of.type.mm <- ""


# Type year
df$date_old <- df$date
df$date <- as.integer(gsub("\\[[^\\]]*\\]", "", df$date, perl = TRUE))

# Calculate lag between collection and date
df$date.lag <- as.numeric(df$date.of.type.yyyy) - as.numeric(df$date)

table(is.na(df$date.of.type.yyyy)) #!CHECK: missing type year
table(is.na(df$date.of.type.mm)) #!CHECK: missing type month
table(is.na(df$date)) #!CHECK: missing description year

# Species type status ----------------------------------------------------------

# Create corrected_valid_species field
valid_species <- gsub("  ", " ", gsub(
    "\\([^\\]]*\\)", "", gsub("\\[[^\\]]*\\]", "",
    df$valid.genus.species.subspecies, perl = TRUE), perl = TRUE))

valid_species <- lapply(
    valid_species, 
    strsplit,
    split = " "
)

df$valid_genus <- unlist(lapply(
    valid_species,
    function(x) gsub("'|=|^-", "", x[[1]][1])
))

df$valid_species <- unlist(lapply(
    valid_species,
    function(x) {
        if(length(x[[1]]) <=3) {
            gsub("'|=|^-", "", x[[1]][2])
        } else {
            NA
        }
    }
))

df$valid_subspecies <- unlist(lapply(
    valid_species,
    function(x) {
        if(length(x[[1]]) <=3) {
            gsub("'|=|^-", "", x[[1]][3])
        } else { 
            NA
        }
    }
))

df$corrected_valid_species <- ""
df[status=="Synonym", ]$corrected_valid_species <- 
    paste0(
        df[status=="Synonym", ]$valid_genus, " ",
        df[status=="Synonym", ]$valid_species
    )

check_binomial <- ifelse(
    df$binomial == df$corrected_valid_species, 
    "Same", "Different"
)

li_valid_species <- unique(paste0(
    df[tolower(status) == "valid species"]$genus, " ",
    df[tolower(status) == "valid species"]$species
))


dim(
    df[check_binomial == "Same" &
       tolower(status) == "synonym" & 
       !(corrected_valid_species %in% li_valid_species),
       c(..bcol, "valid.genus.species.subspecies")]
) #!CHECK: for synonym, valid species name and binomial nomenclature is same
# TODO: make code to change status to "valid species"? 


dim(
    df[check_binomial == "Different" &
       tolower(status) == "synonym" & 
       !(corrected_valid_species %in% li_valid_species),
       c(..bcol, "valid.genus.species.subspecies")]
) #!CHECK: for synonym, valid species does not exist
# TODO: make code to check names from other fields and incorporate the
# valid name?


rm(check_binomial, li_valid_species)


# Species type locality --------------------------------------------------------

# Manual edits due to inconsistent lat/lon; out-of-range lat/lon

# Out-of-range / weird characters
# TODO: 

# In the sea (EcoRegions / GADM)
# TODO:

## Inconsistent
df$lat_n <- as.numeric(df$lat)
df$lon_n <- as.numeric(df$lon)

dim(
    df[(is.na(lat_n) & df$lat != "") | (is.na(lon_n) & df$lon != ""), 
       c(..bcol, "lat", "lon", "lat_n", "lon_n")]
) #!CHECK: lat/lon with odd characters
# TODO: make file to incorporate changes (if insufficiently cleaned)

## Out of range

dim(
    df[(abs(lat_n) > 90 | abs(lon_n) > 180),
       c(..bcol, "lat", "lon", "lat_n", "lon_n")]
) #!CHECK: lat/lon out of range
# TODO: make file to incorporate changes (if insufficiently cleaned)


# Convert all lat/lon which are in the sea to NA / manual georef
df_geo <- df[!(is.na(lat_n) | is.na(lon_n) )]
df_geo <- df_geo[!(abs(lat_n) > 90 | abs(lon_n) > 180)]

v_df <- st_as_sf(df_geo, coords = c("lon_n", "lat_n"), crs = wgs84)
v_df <- st_join(v_df, v_ecoregions, join = st_intersects)

# Convert all lat/lon which do not match type country to NA / manual georef
v_df <- st_join(v_df, v_continent, join = st_intersects)



df_geo <- df[, c("idx", "lat_n", "lon_n", "type.country")]
df_geo$type.country_n <- gsub("\\:", "", df_geo$type.country)

df_geo <- merge(
    df_geo, lp_country[, c("DL", "A-3")],
    by.x = "type.country_n", by.y = "DL",
    all.x = T, all.y = F
)

df_geo <- merge(
    df_geo, lp_dl[, c("DL", "GID_0_owner")],
    by.x = "type.country_n", by.y = "DL",
    all.x = T, all.y = F
)

df_geo[!is.na(type.country_n) & !is.na(GID_0_owner), ]$`A-3` <-
    df_geo[!is.na(type.country_n) & !is.na(GID_0_owner), ]$`GID_0_owner`

df_geo <- df_geo[, c("idx", "A-3", "type.country_n")][!duplicated(idx)]
names(df_geo) <- c("idx", "type.country_n.A3", "type.country_n")


df_join <- data.table(v_df)[, c("idx", "GID_0", "REALM")]

df_join <- merge(
    df_join, lp_country[, c("A-3", "DL")],
    by.x = "GID_0", by.y = "A-3",
    all.x = T, all.y = F
)

df_join <- df_join[, c("idx", "GID_0", "DL", "REALM")][!duplicated(idx)]
names(df_join) <- c("idx", "sj.type.country_A3", "sj.type.country_DL",
                    "sj.realm")

df <- merge(
    df, df_geo,
    by = "idx", all.x = T, all.y = F
)

df <- merge(
    df, df_join,
    by = "idx", all.x = T, all.y = F
)


dim(df[is.na(lat_n) | is.na(lon_n)]) #!CHECK: no lat/lon


dim(  
    df[!(is.na(lat_n) | is.na(lon_n)) &
        (is.na(sj.realm) | sj.realm == "N/A"),
       c(..bcol, "lat_n", "lon_n", "sj.realm")]       
) #!CHECK: likely in the sea (or near to sea); spatial join (EcoRegion2017)
# TODO: make file to incorporate changes (if insufficiently cleaned)


dim(  
    df[!(is.na(lat_n) | is.na(lon_n)) &
        !(is.na(sj.realm) | sj.realm == "N/A") &
        (is.na(sj.type.country_A3) | sj.type.country_A3 == ""),
       c(..bcol, "lat_n", "lon_n", "sj.realm")]       
) #!CHECK: likely in the sea (or near to sea); spatial join (GADM)
# TODO: make file to incorporate changes (if insufficiently cleaned)


dim(
    df[!(is.na(lat_n) | is.na(lon_n)) &
       sj.type.country_DL != type.country_n,
       c(..bcol, "lat_n", "lon_n", "type.country_n", "sj.type.country_DL")]  
) #!CHECK: GADM country assigned is not the same as the type.country
# TODO: make file to incorporate changes (if insufficiently cleaned)


dim(
    df[!(is.na(lat_n) | is.na(lon_n)) &
       (type.country_n == "" | is.na(type.country_n)) &
       !(sj.type.country_DL == "" | is.na(sj.type.country_DL)),
       c(..bcol, "lat_n", "lon_n", "type.country_n", "sj.type.country_DL")]  
) #!CHECK: No type country but with lat/lon

df[!(is.na(lat_n) | is.na(lon_n)) &
       (type.country_n == "" | is.na(type.country_n)) &
       !(sj.type.country_DL == "" | is.na(sj.type.country_DL))
       ]$type.country_n <- 
       df[!(is.na(lat_n) | is.na(lon_n)) &
       (type.country_n == "" | is.na(type.country_n)) &
       !(sj.type.country_DL == "" | is.na(sj.type.country_DL))
       ]$sj.type.country_DL




file <- paste0(v2_dir_data_raw, v2_basefile, "_3.csv")
fwrite(df, file)