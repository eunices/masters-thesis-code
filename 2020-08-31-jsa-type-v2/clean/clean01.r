# Purpose: filter species / taxonomic ranks / species status / 
# clean lat & lon & locality related info

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

# Genus with Uncertain in sq brackets
df[grepl("\\?", tolower(genus)), .N, genus]



# Species type status ----------------------------------------------------------

# Check synonyms have a valid species associated
# df$correct_synonym




# Species type locality --------------------------------------------------------

# Manual edits due to inconsitent lat/lon; out-of-range lat/lon

## Inconsistent
df$lat.n <- as.numeric(df$lat)
df$lon.n <- as.numeric(df$lon)


# Convert all lat/lon which are in the sea to NA / manual georef
# TODO:

# Convert all lat/lon which do not match type country to NA / manual georef
# TODO:






file <- paste0(v2_dir_data_raw, v2_basefile, "_3.csv")
fwrite(df, file)