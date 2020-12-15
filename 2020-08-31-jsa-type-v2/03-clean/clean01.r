# Purpose: filter species / taxonomic ranks / species status / 
# format dates / clean lat & lon & locality related info

source('2020-08-31-jsa-type-v2/00-init/main.r')


# Read 
file <- paste0(v2_dir_data_raw, v2_basefile, "_2.csv")

df <- read_escaped_data_v2(file)[order(idx)]

# CHECK
df[idx %in% c(18341, 18592)][, c(..bcol, ..pcol)]


# Species statuses -------------------------------------------------------------

# Standardize status
df[status == "Valid Species"]$status <- "Valid species"

# CHECK
table(df$status)

statuses <- c(
    "Valid species", "Synonym", "Valid subspecies", "Nominate subspecies",
    "Morphospecies"
)

df$status <- factor(df$status, statuses)

# CHECK
table(is.na(df$status))

# Remove rows ------------------------------------------------------------------

# Remove "z" rows
df <- df[!(genus == "z" | is.na(status)), ]

# Remove those with numbers in genus
df <- df[!grepl("[0-9]", genus)]

# Remove morphospecies
df <- df[status != "Morphospecies"]

# Remove those with uncertain family
df <- df[family != "Uncertain"]

df <- df[!grepl("\\?", family)]

# Tag duplicated species based on unique combination
df$duplicated <- duplicated(
    df[, c("genus", "species", "author", "date", "status")]
)

# Check for non-obvious duplicates

# Get unique combinations of duplicated genus and species
dups <- unique(
    df[duplicated(paste0(genus, " ", species)), c("genus", "species")]
)

# Subset for dups which are synonyms or valid species
dups <- 
    df[
        status %in% c("Synonym", "Valid species") &
        duplicated == FALSE &
        paste0(genus, " ", species) %in% paste0(dups$genus, " ", dups$species)
    ]

# Extract date
dups$date <- gsub("\\[[^\\]]*\\]", "", dups$date, perl = TRUE)

# CHECK
dups <- 
    dups[,
         list(n = .N, 
              idxes = paste0(idx, collapse = ", "),
              status = paste0(sort(unique(status)), collapse = "; "),
              date = paste0(sort(unique(date)), collapse = "; "),
              author = paste0(sort(unique(author)), collapse = "; "),
              file = paste0(sort(unique(file)), collapse = "; ")),
         by = c("genus", "species")]

cfile <- paste0(v2_dir_data_raw_check, "33-34-species-dups.csv")
fwrite(dups, cfile)
# note: no action taken except for valid species


# Subset duplicates w/ **only valid species**
# and **valid species and synonym" for the name,
# use the first
dups_valid <- separate_rows(
    dups[status %in% c("Valid species", "Valid species; Synonym"), c("idxes")],
    "idxes",
    sep = ", "
)

cols <- c(bcol, pcol)
dups_valid <- df[idx %in% dups_valid$idxes, ..cols][
    order(genus, species, status)
]

dups_valid$duplicated <- duplicated(
    dups_valid[, c("genus", "species")] 
) # take first duplicated, with valid species taking precedence

df <- merge(
    df, dups_valid[, c("idx", "duplicated")],
    all.x = TRUE, all.y = FALSE,
    by.x = "idx", by.y = "idx",
    suffixes = c("", "_n")
)

df[!is.na(duplicated_n)]$duplicated <- df[!is.na(duplicated_n)]$duplicated_n
df$duplicated_n <- NULL

# CHECK
table(df$duplicated)


# Taxonomic ranks --------------------------------------------------------------

# CHECK: tribe with None and Uncertain
df[tolower(tribe) %in% c("none", "uncertain"), .N, tribe] 

# CHECK: Genus with uncertainty
df[grepl("\\?", tolower(genus)), .N, genus] 

# Remove brackets for uncertain genera
# and treat uncertain genera as correct
df[grepl("\\?", tolower(genus))]$genus <- gsub(
    "\\[|\\]|\\?", "", df[grepl("\\?", tolower(genus))]$genus
)

# Format dates -----------------------------------------------------------------

# Type year
df$date_old <- df$date
df$date <- gsub("\\)", "", df$date) # note: date field had extra bracket
df$date <- as.integer(gsub("\\[[^\\]]*\\]", "", df$date, perl = TRUE))

# CHECK: missing description year
table(is.na(df$date)) 

# Collection dates

# Year
df$date.of.type.yyyy <- as.integer(sub('.*(\\d{4}).*', '\\1', df$date.of.type))
df[grepl("neotype", tolower(df$date.of.type))]$date.of.type.yyyy <- NA

# CHECK: missing type year
table(is.na(df$date.of.type.yyyy))

# Month
df$date.of.type.mm <- gsub(
    ".*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec).*", 
    "\\1", 
    df$date.of.type
)

df[
    !df$date.of.type.mm %in% c("Jan", "Feb", "Mar", "Apr", "May", 
                               "Jun", "Jul", "Aug", "Sep", "Oct", 
                               "Nov", "Dec"),
]$date.of.type.mm <- ""

# CHECK: missing type month
table(is.na(df$date.of.type.mm)) 

# Calculate lag between collection and date
df$date.lag <- df$date - as.integer(df$date.of.type.yyyy)

# CHECK: that description date should be later
table(df$date.lag < 0)
table(df$date.lag >= -10 & df$date.lag <0) # consider this to be acceptable
table(df$date.lag < -10)
table(df$date.lag < -1000)

# Set the date lag of >1000 to be NA
df[date.lag < -10,]$date.of.type.yyyy <- NA
df[date.lag < -10,]$date.lag <- NA


# Species type status ----------------------------------------------------------

# Create a field with valid genus, species and separately, and 
# "corrected_valid_species" (binomial)

valid_species <- gsub("  ", " ", gsub(
    "\\([^\\]]*\\)", "", gsub("\\[[^\\]]*\\]", "",
    df$valid.genus.species.subspecies, perl = TRUE), perl = TRUE
))

valid_species <- lapply(valid_species, strsplit, split = " ")

# Valid genus
df$valid_genus <- unlist(lapply(
    valid_species, function(x) gsub("'|=|^-", "", x[[1]][1])
))

# Valid species
df$valid_species <- unlist(lapply(
    valid_species,
    function(x) {if(length(x[[1]]) <=3) gsub("'|=|^-", "", x[[1]][2]) else NA}
))

# Valid subspecies
df$valid_subspecies <- unlist(lapply(
    valid_species,
    function(x) {if(length(x[[1]]) <=3) gsub("'|=|^-", "", x[[1]][3]) else NA}
))

# Corrected valid species
df$corrected_valid_species <- ""
df[status=="Synonym", ]$corrected_valid_species <- 
    paste0(
        df[status=="Synonym", ]$valid_genus, " ",
        df[status=="Synonym", ]$valid_species
    )

# Duplicated subspecies --------------------------------------------------------

# Get unique list of duplicated subspecies
dups <- unique(df[
    grepl("subspecies", status) &
    duplicated(valid.genus.species.subspecies)
]$valid.genus.species.subspecies)

# CHECK
cfile <- paste0(v2_dir_data_raw_check, "35-36-subspecies-dups.csv")
fwrite(
    df[grepl("subspecies", status) & valid.genus.species.subspecies %in% dups][
    , c(
        "file", ..bcol, ..pcol, "valid_subspecies",
        "valid.genus.species.subspecies", "duplicated"
        )
    ][order(genus, species, valid_subspecies),],
    
    cfile    
)

# Species type locality --------------------------------------------------------

df$lat_n <- as.numeric(df$lat)
df$lon_n <- as.numeric(df$lon)

# CHECK: odd characters
dim(
    df[(is.na(lat_n) & df$lat != "") | (is.na(lon_n) & df$lon != ""), 
       c(..bcol, "lat", "lon", "lat_n", "lon_n")]
) 

# CHECK: out of range
dim(
    df[(abs(lat_n) > 90 | abs(lon_n) > 180),
       c(..bcol, "lat", "lon", "lat_n", "lon_n")]
)

# CHECK: no lat/lon
dim(df[is.na(lat_n) | is.na(lon_n)]) 


# Convert all lat/lon which are in the sea OR do not match type country 
# to NA / manual georef

# Create spatial layer with lat/lon
df_geo <- df[!(is.na(lat_n) | is.na(lon_n) )]
df_geo <- df_geo[!(abs(lat_n) > 90 | abs(lon_n) > 180)]
v_df <- st_as_sf(df_geo, coords = c("lon_n", "lat_n"), crs = wgs84)

# Join to ecoregions
v_df <- st_join(v_df, v_ecoregions, join = st_intersects)

# Join to country
v_df <- st_join(v_df, v_continent, join = st_intersects)

# Create comparable type country
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

# Convert spatial layer to table
df_join <- data.table(v_df)[, c("idx", "GID_0", "REALM")]

df_join <- merge(
    df_join, lp_country[, c("A-3", "DL")],
    by.x = "GID_0", by.y = "A-3",
    all.x = T, all.y = F
)

df_join <- df_join[, c("idx", "GID_0", "DL", "REALM")][!duplicated(idx)]

names(df_join) <- c(
    "idx", "sj.type.country_A3", "sj.type.country_DL", "sj.realm"
)

# Merge data original and spatial join country
df <- merge(
    df, df_geo,
    by = "idx", all.x = T, all.y = F
)

df <- merge(
    df, df_join,
    by = "idx", all.x = T, all.y = F
)

# CHECK: in the sea? (EcoRegion 2017)
dim(  
    df[!(is.na(lat_n) | is.na(lon_n)) &
        (is.na(sj.realm) | sj.realm == "N/A"),
       c(..bcol, "lat_n", "lon_n", "sj.realm")]       
)

# CHECK: in the sea? (GADM)
dim(  
    df[!(is.na(lat_n) | is.na(lon_n)) &
        !(is.na(sj.realm) | sj.realm == "N/A") &
        (is.na(sj.type.country_A3) | sj.type.country_A3 == ""),
       c(..bcol, "lat_n", "lon_n", "sj.realm")]       
)
# ACTION: to get the nearest land body in the ch2 script

# CHECK: assigned different country
dim(
    df[!(is.na(lat_n) | is.na(lon_n)) &
       sj.type.country_DL != type.country_n,
       c(..bcol, "lat_n", "lon_n", "type.country_n", "sj.type.country_DL")]  
) 
# TODO: make file to incorporate changes (if insufficiently cleaned)

# CHECK: No type country but with lat/lon
dim(
    df[!(is.na(lat_n) | is.na(lon_n)) &
       (type.country_n == "" | is.na(type.country_n)) &
       !(sj.type.country_DL == "" | is.na(sj.type.country_DL)),
       c(..bcol, "lat_n", "lon_n", "type.country_n", "sj.type.country_DL")]  
) 
# ACTION: to assign the country based on the lat/lon

# Assign country based on those without lat/lon
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
