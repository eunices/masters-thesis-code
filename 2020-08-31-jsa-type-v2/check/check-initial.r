source('2020-08-31-jsa-type-v2/init/init.r')

# See raw files

list.files(v2_dir_data_raw_raw, pattern = "^Apoidea*.*.\\.xlsb")


# Read excel files

file <- paste0(v2_dir_data_raw, v2_basefile, "_2.csv")
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



# Are the species names duplicated?

# Yes they are duplicated between lists, these need to be removed
# Yes there are duplicates within lists as well (but few)
# They need to be combined by global mapper (to identify distribution of sp.)
# or use merged global mapper field.

# How blank synonyms can be resolved?
# Through valid.genus.species.subspecies

# Are date and date.n same?
df$date <- as.integer(gsub("\\[[^\\]]*\\]", "", df$date, perl = TRUE))
fwrite(
    df[is.na(date)][
        , c("idx", "file",  "genus", "species", 
            "author", "date", "author.date")     
], 
    paste0(v2_dir_data_raw_check, "1-missing-date.csv")
)

df$author.date_date <- gsub(".*?([0-9]+).*", "\\1",
    gsub("\\[[^\\]]*\\]", "", df$author.date, perl = TRUE)
)
df$author.date_date.n <- as.integer(df$author.date_date)
df[is.na(author.date_date.n) & date != "z" & author.date != "", 
   c("idx", "file", "genus", "species", 
     "date", "author.date", "author.date_date")]

fwrite(
    df[!is.na(author.date_date.n) & date != "z" & author.date != ""][
    date != author.date_date.n 
][, c("idx", "file", "genus", "species", "date", "author.date_date.n", "author.date")     
], 
    paste0(v2_dir_data_raw_check, "2-date-discrepancy.csv")
)

########### Locality

# Are lat/ lon with non-standard characters?
df$lat_n <- as.numeric(df$lat)
df$lon_n <- as.numeric(df$lon)

fwrite(
    df[(is.na(lat_n) & lat != "") |
        (is.na(lon_n) & lon != ""), 
        c("idx", "file", "genus", "species", "lat", "lon")],
    paste0(v2_dir_data_raw_check, "3-lat-lon-non-standard.csv")
)
# Replace commas with full stop (without space), 
# split text for those with comma, remove equal signs


# Are lat/ lon within min and max range?
write.csv(
    df[(abs(lat_n) > 90 | abs(lon_n) > 180), 
          c("file", "genus", "species", "lat_n", "lon_n") ],
    paste0(v2_dir_data_raw_check, "4-lat-lon-out-of-range.csv")
)

# How many lat/ lon are missing?
table(is.na(df$lat_n) | is.na(df$lon_n))


# Are lat/ lon in land
df_geo <- df[!(is.na(lat_n) | is.na(lon_n) )]
df_geo <- df_geo[!(abs(lat_n) > 90 | abs(lon_n) > 180)]
v_df <- st_as_sf(df_geo, coords = c("lon_n", "lat_n"), crs = wgs84)
v_df <- st_join(v_df, v_ecoregions, join = st_intersects)

fwrite(
    data.table(
        v_df[is.na(v_df$REALM) | v_df$REALM == "N/A", 
        c("idx", "file", "genus", "species", "author", "date", "status")]
    ),
    paste0(v2_dir_data_raw_check, "8-lat-lon-water-body.csv")
)

# Are lat/lon in type country
v_df <- st_join(v_df, v_continent, join = st_intersects)

table(is.na(v_df$NAME_0))

fwrite(
    data.table(
        v_df[is.na(v_df$NAME_0) & 
             !(is.na(v_df$REALM) | v_df$REALM == "N/A"), 
            c("idx", "file", "genus", "species", "author", "date", "status",
              "type.country",
              "NAME_0")]),
    paste0(v2_dir_data_raw_check, "9-lat-lon-water-body2.csv")
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


# With country

check <- data.table(
    df_country[!is.na(GID_0) & 
         !(is.na(REALM) | REALM == "N/A"), ]
)

table(check$`A-3` == check$`GID_0`)

fwrite(
    check[`A-3` != GID_0, 
      c("idx", "file", "genus", "species", 
        "author", "date", "status",
        "lat", "lon", "type.country", 
        "GID_0")],
      paste0(v2_dir_data_raw_check, "10-lat-lon-type-country-mismatch.csv")
)

fwrite(
    df_country[(is.na(type.country) | type.country == "") & 
        !(GID_0 == "" | is.na(GID_0))
        !(is.na(lat.n) | is.na(lon.n)), 
      c("idx", "file", "genus", "species", 
        "author", "date", "status",
        "lat", "lon", "type.country", "GID_0", "type.locality.verbatim")],
      paste0(v2_dir_data_raw_check, "26-no-country-with-lat-lon.csv")
)


# Global distribution ----------------------------------------------------------

names(df)[grepl("global", names(df))]
df$merged.global.mapper[1:10]
df$global.mapper[1:10]

table(is.na(df$global.mapper))
table(is.na(df$merged.global.mapper))

# remove words in sq brackets
df$global.mapper_n <-
    gsub("\\[[^\\]]*\\];|\\[[^\\]]*\\]","", df$global.mapper, perl = T)

df[idx==19152, c("global.mapper_n", "global.mapper")]

df_global <- separate_rows(
    df[, c("idx", "global.mapper_n")], 
    global.mapper_n,  
    sep = " "
)

df_global$global.mapper2 <- 
    gsub( ":.*$", "", df_global$global.mapper)

dim(df_global)
df_global <- df_global[!(is.na(global.mapper2) | global.mapper2 == "")]
dim(df_global)

df_global <- merge(
    df_global, lp_country[, c("DL", "A-3", "Country")],
    by.x = "global.mapper2", by.y = "DL",
    all.x = T, all.y = F)

df_global <- merge(
    df_global, lp_dl[, c("DL", "location_long", "GID_0_owner")],
    by.x = "global.mapper2", by.y = "DL",
    all.x = T, all.y = F
)

df_global$cty <- ""
df_global[!is.na(Country)]$cty <- df_global[!is.na(Country)]$`A-3`

df_global[is.na(Country) & !is.na(GID_0_owner)]$cty <- 
    df_global[is.na(Country) & !is.na(GID_0_owner)]$GID_0_owner

table(df_global$cty == "")

fwrite(
    df_global[cty==""],
    # paste0(v2_dir_data_raw_check, "11-distribution-missing.csv")
    paste0(v2_dir_data_raw, "test.csv")
)

df[idx==11084, c("idx", "file", "genus", "species", "global.mapper")]
df[idx==5421, c("idx", "file", "genus", "species", "global.mapper")]

# Not formatted properly
fwrite(
    df[idx %in% c(3074, 5712, 7299, 7821, 7824, 
    8351, 8611, 8630, 9843, 11824, 15092,
    15135, 17419, 18865, 20109, 20140, 20294, 
    20574, 25100, 29473, 29918, 29974, 33900),
    c("idx", "file", "genus", "species", 
      "author", "date", "status", "global.mapper")],
    paste0(v2_dir_data_raw_check, "11c-distribution-format-odd.csv")
)

df[idx==1280, c("idx", "file", "genus", "species", "global.mapper")]

# Square brackets not closed
fwrite(
    df[idx %in% c(20694, 16058, 9862, 9342, 22842, 22843, 11449),
     c("idx", "file", "genus", "species", 
      "author", "date", "status", "global.mapper")],
    paste0(v2_dir_data_raw_check, "11d-distribution-brackets-odd.csv")
)

df[idx %in% c(5421, 5422, 5423, 5424),
   c("idx", "file", "genus", "species", "global.mapper")]

# Publication ------------------------------------------------------------------
names(df) 
df_pub <- df[, c(1, 28, 36:47)]

df_pubs <- df_pub[, list(
    N = .N,
    idxes = paste0(idx, collapse=", ")
), by = c("date", "paper.authors", "paper.editors", 
          "title", "journal", "volume", "issue",
          "page.numbers.publication", "country.of.publication",
          "city.of.publication", "paper.type")][
    order(date, journal, title, paper.authors, volume, issue, page.numbers.publication)
          ]

df_pubs$issue <- paste0("'", df_pubs$issue)
df_pubs$volume <- paste0("'", df_pubs$volume)
df_pubs$page.numbers.publication <-
    paste0("", df_pubs$page.numbers.publication)

fwrite(
    df_pubs,
    paste0(v2_dir_data_raw_check, "12-pub.csv")
)


# Collector  -------------------------------------------------------------------



# Repository -------------------------------------------------------------------



# Are date of description > date of collection?
df$date.of.type.yyyy <- as.numeric(sub('.*(\\d{4}).*', '\\1', df$date.of.type))
df$date.of.type.n <- paste0("''", df$date.of.type)
fwrite(
    df[date.of.type.yyyy > date, c("idx", "file", "genus", "species", 
     "date", "author.date", "date.of.type.n", "date.of.type.yyyy")],
    paste0(v2_dir_data_raw_check, "19-date-of-coll.csv")
)

# Taxonomic ranks present
df[grepl("\\?", tolower(genus)), c("idx", "file", "genus", "species", 
     "date", "author.date", "status")]

df[, .N, family][order(family)]
df[, .N, tribe][order(tribe)]

fwrite(
    df[, .N, genus][order(genus)],
    paste0(v2_dir_data_raw, "test.csv")
)

# Date of collection erroneous?

# Create a string data field
df$date.of.type.string <- paste0("'", df$date.of.type)
df$date.of.type.dd <- as.numeric(sub("\\D*(\\d+).*", "\\1", df$date.of.type))
df[df$date.of.type.dd>31,]$date.of.type.dd <- NA

# Create month field
df$date.of.type.mm <- 
    gsub(".*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec).*", "\\1", df$date.of.type)
df[!df$date.of.type.mm %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),]$date.of.type.mm <- ""

# Create year field
df$date.of.type.yyyy <- as.integer(sub('.*(\\d{4}).*', '\\1', df$date.of.type))

fwrite(
    df[as.numeric(df$date.of.type.yyyy) <1700, 
        c("idx", "file", "genus", "species", 
          "date", "author.date", "status", "date.of.type")],
    paste0(v2_dir_data_raw_check, "20-date-of-coll-1700.csv")
)

fwrite(
    df[date.of.type.mm == "" & date.of.type.string != "'NA" & 
    grepl("[A-z]", date.of.type.string),
    c("idx", "file", "genus", "species", "date", "author.date", "status",
    "date.of.type.string", "date.of.type.mm")],
    paste0(v2_dir_data_raw_check, "21-date-of-coll-month.csv")
)

fwrite(
    df[is.na(date.of.type.yyyy) &
    grepl("[0-9]", date.of.type.string),
    c("idx", "file", "genus", "species", "date", "author.date", "status",
    "date.of.type.string", "date.of.type.yyyy")],
    paste0(v2_dir_data_raw_check, "22-date-of-coll-year.csv")
)

# Valid species field with non-standard format

# Check synonyms have a valid species associated
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



valid_species_len <- unlist(lapply(
    valid_species, 
    function(x) length(x[[1]])
))


fwrite(
    df[valid_species_len > 3 , 
    c("idx", "file", "genus", "species", "date", "author.date", "status",
    "valid.genus.species.subspecies", "valid_genus",
      "valid_species", "valid_subspecies")], 
      paste0(v2_dir_data_raw_check, "23-valid-species.csv")
)

df$corrected_valid_species <- ""
df[status=="Synonym", ]$corrected_valid_species <- 
    paste0(
        df[status=="Synonym", ]$valid_genus, " ",
        df[status=="Synonym", ]$valid_species
    )

li_valid_species <- unique(paste0(
    df[tolower(status) == "valid species"]$genus, " ",
    df[tolower(status) == "valid species"]$species
))

df$binomial <- paste0(df$genus, " ", df$species)

df$check_binomial <- ifelse(df$binomial == df$corrected_valid_species, 
    "Same", "Different")

fwrite(
    df[check_binomial == "Same" &
        tolower(status) == "synonym" & 
       !(corrected_valid_species %in% li_valid_species), 
       c("idx", "file", "genus", "species", "date", "author.date", "status",
        "valid.genus.species.subspecies", "valid_genus", "valid_species",
        "check_binomial")],
    paste0(v2_dir_data_raw_check, "24-valid-species-same.csv")
)

fwrite(
    df[check_binomial == "Different" &
        tolower(status) == "synonym" & 
       !(corrected_valid_species %in% li_valid_species), 
       c("idx", "file", "genus", "species", "date", "author.date", "status",
        "valid.genus.species.subspecies", "valid_genus", "valid_species",
        "check_binomial")],
    paste0(v2_dir_data_raw_check, "25-valid-species-not-in-li.csv")
)


# Are full name of describer and describer the same?

