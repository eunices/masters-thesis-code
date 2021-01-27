# Purpose: Distribution of each species

source('2020-08-31-jsa-type-v2/00-init/main.r')
print(paste0(Sys.time(), " ----- df04.r"))


# Read data --------------------------------------------------------------------
file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
df <- read_escaped_data_v2(file)

cols <- c(
    bcol, "duplicated",
    "global.mapper_n", "continents.mapper_n", "biogeo_wwf.mapper_n",
    "biogeo_ecor2017.mapper_n", "latitude_type2.mapper_n"
)
df <- df[, ..cols]


# Create distribution dataset --------------------------------------------------

# By country
df_dist <- data.table(separate_rows(df, global.mapper_n, sep = "; "))
df_dist <- data.table(separate_rows(df_dist, continents.mapper_n, sep = "; "))
df_dist <- data.table(separate_rows(df_dist, biogeo_wwf.mapper_n, sep = "; "))
df_dist <- data.table(separate_rows(df_dist, biogeo_ecor2017.mapper_n, sep = "; "))
df_dist <- data.table(separate_rows(df_dist, latitude_type2.mapper_n, sep = "; "))

cols <- bcol[bcol != 'idx']
df_dist <- df_dist[, list(
        country = paste0(
            sort(unique(global.mapper_n)), collapse = "; "
        ),

        continent = paste0(
            sort(unique(continents.mapper_n)), collapse = "; "
        ),

        biogeo_wwf = paste0(
            sort(unique(biogeo_wwf.mapper_n)), collapse = "; "
        ),

        biogeo_ecor2017 = paste0(
            sort(unique(biogeo_ecor2017.mapper_n)), collapse = "; "
        ),

        latitude = paste0(
            sort(unique(latitude_type2.mapper_n)), collapse = "; "
        )
    ), 
    by = cols
]

df_dist <- df_dist[order(df_dist)]


# Write data -------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "-distribution.csv")
fwrite(df_dist, file)