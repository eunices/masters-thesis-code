# Purpose: tabulate fields for synonyms and valid species

source('2020-08-31-jsa-type-v2/00-init/main.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_6.csv")
df <- read_escaped_data_v2(file)

# Calculate metrics summarised for synonyms ------------------------------------

# Count synonyms
sum <- df[
    duplicated == FALSE &
    status %in% c("Synonym"),
    list(N = .N), by=c("corrected_valid_species", "status")
]

var_count <- dcast(
    sum, 
    corrected_valid_species ~ status, 
    value.var = "N"
)

names(var_count) <- c("binomial", "n_synonyms")
var_count[, c("genus", "species") := tstrsplit(binomial, " ", fixed=TRUE)]
var_count <- var_count[!(binomial == "" | is.na(binomial))]
var_count[is.na(var_count)] <- 0

df <- merge(
    df, var_count, 
    by = c("genus", "species"), 
    all.x = TRUE, all.y=FALSE
)


# Count subspecies
subsp_count <- df[
    duplicated == FALSE &
    status %in% c("Nominate subspecies", "Valid subspecies"),
    list(N = .N), by=c("genus", "species")
]

subsp_count <- subsp_count[!(genus == "" | species == "" |
    is.na(genus) | is.na(species))]
names(subsp_count) <- c("genus", "species", "n_subspecies")
subsp_count[is.na(subsp_count)] <- 0

df <- merge(
    df, subsp_count, 
    by = c("genus", "species"), 
    all.x = TRUE, all.y=FALSE
)


df <- df[order(idx)]


file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
fwrite(df, file)

