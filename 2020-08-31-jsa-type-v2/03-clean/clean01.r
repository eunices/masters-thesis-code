# Purpose: filter species / taxonomic ranks / species status / 
# format dates / clean lat & lon & locality related info

source('2020-08-31-jsa-type-v2/00-init/main.r')

# Read 
file <- paste0(v2_dir_data_raw, v2_basefile, "_2.csv")
df <- read_escaped_data_v2(file)[order(idx)]

df[idx %in% c(18341, 18592)][, c(..bcol, ..pcol)]

# Species statuses -------------------------------------------------------------

# Standardize status
df[status == "Valid Species"]$status <- "Valid species"
df$status <- factor(
    df$status,
    c("Valid species", "Synonym", "Valid subspecies", "Nominate subspecies")
)


# Remove "z"
df <- df[!(genus == "z" | status == "z"), ]


# Remove numbers
df <- df[!grepl("[0-9]", genus)]


# Remove morphospecies
df <- df[status != "Morphospecies"]


# Remove those with uncertain family
df <- df[family != "Uncertain"]
df <- df[!grepl("\\?", family)]

# Tag duplicated species

# Based on unique combination 
df$duplicated <- duplicated(
    df[, c("genus", "species", "author", "date", "status")]
)

# Check for non-obvious duplicates
dups <- df[duplicated(paste0(genus, " ", species)), c("genus", "species")]

dups <- 
    df[
        duplicated == FALSE &
        status %in% c("Synonym", "Valid species") &
        paste0(genus, " ", species) %in% paste0(dups$genus, " ", dups$species)
    ]

dups$date <- gsub("\\[[^\\]]*\\]", "", dups$date, perl = TRUE)

dups <- 
    dups[,
         list(n = .N, 
              idxes = paste0(idx, collapse = ", "),
              status = paste0(sort(unique(status)), collapse = "; "),
              date = paste0(sort(unique(date)), collapse = "; "),
              author = paste0(sort(unique(author)), collapse = "; "),
              file = paste0(sort(unique(file)), collapse = "; ")),
         by = c("genus", "species")]

# cfile <- paste0(v2_dir_data_raw_check, "33-34-dups.csv")
# fwrite(dups, cfile)


# Valid species with duplicated names 

dups_valid <- separate_rows(
    dups[status == "Valid species", c("idxes")],
    "idxes",
    sep = ", "
)

cols <- c(bcol, pcol)
dups_valid <- df[idx %in% dups_valid$idxes, ..cols][
    order(genus, species, status)
]

dups_valid$duplicated <- duplicated(
    dups_valid[, c("genus", "species")]   # take the first duplicate
)

df <- merge(
    df, dups_valid[, c("idx", "duplicated")],
    all.x = TRUE, all.y = FALSE,
    by.x = "idx", by.y = "idx",
    suffixes = c("", "_n")
)

df[!is.na(duplicated_n)]$duplicated <- df[!is.na(duplicated_n)]$duplicated_n
df$duplicated_n <- NULL


# Duplicates (synonyms and valid species) but with same date or author
dups_nonvalid <- separate_rows(
    dups[status == "Valid species; Synonym" &
         (!grepl("; ", date) & !grepl("; ", author)), 
         c("idxes")],
    "idxes",
    sep = ", "
)

dups_nonvalid <- df[idx %in% dups_nonvalid$idxes, ..cols][
    order(genus, species, status)
]

dups_nonvalid$duplicated <- duplicated(      
    dups_nonvalid[, c("genus", "species")]  # take the first duplicate
)

# cfile <- paste0(v2_dir_data_raw_clean, "clean01-nonvalid-dups.csv")
# fwrite(dups_nonvalid, cfile)

df <- merge(
    df, dups_nonvalid[, c("idx", "duplicated")],
    all.x = TRUE, all.y = FALSE,
    by.x = "idx", by.y = "idx",
    suffixes = c("", "_n")
)

df[!is.na(duplicated_n)]$duplicated <- df[!is.na(duplicated_n)]$duplicated_n
df$duplicated_n <- NULL

table(df$duplicated) #!CHECK:

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
# TODO: make code to check names from other names and incorporate the
# valid name?


rm(check_binomial, li_valid_species)


# Duplicated subspecies --------------------------------------------------------

dups <- 
    unique(df[grepl("subspecies", status) &
       duplicated(valid.genus.species.subspecies)]$valid.genus.species.subspecies)

cfile <- paste0(v2_dir_data_raw_check, "35-36-subspecies-dups-missing.csv")
fwrite(
    df[grepl("subspecies", status) & 
       valid.genus.species.subspecies %in% dups][
    , c("file", ..bcol, ..pcol, "valid_subspecies",
        "valid.genus.species.subspecies", "duplicated")
    ][order(genus, species, valid_subspecies),],
    cfile    
)

# Species type locality --------------------------------------------------------

# Manual edits due to inconsistent lat/lon; out-of-range lat/lon

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