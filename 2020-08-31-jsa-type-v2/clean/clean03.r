# Purpose: clean global mapper distribution

source('2020-08-31-jsa-type-v2/init/init.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_4.csv")
df <- read_escaped_data_v2(file)


# Filter global mapper ---------------------------------------------------------

df_map1 <- df[, c("idx", "global.mapper")]

df_map2 <- df[, c("idx", "merged.global.mapper")]
names(df_map2) <- c("idx", "global.mapper")

df_map <- rbind(df_map1, df_map2)

df_map$global.mapper <- gsub(
    "\\[[^\\]]*\\]", "", 
    df_map$global.mapper, 
    perl = TRUE
)

df_map <- separate_rows(df_map, global.mapper, sep = " ")

df_map <- df_map[!is.na(global.mapper), ]

df_map$global.mapper <- gsub(
    ":.*", "",
    df_map$global.mapper,
)

df_map$nchar <- nchar(df_map$global.mapper)

df_map <- df_map[nchar == 2]

df_map <- df_map[grepl("^[[:alpha:]]+$", global.mapper, perl = TRUE)]

df_map <- unique(df_map)

cols_lp_country <- c(
    "DL", 
    "continent",
    "biogeo_wwf", "prop_area_biogeo_wwf",
    "biogeo_ecor2017", "prop_area_biogeo_ecor2017",
    "Latitude_type", "Latitude_type2", "prop_area_Latitude_type2"
)

df_map <- merge(
    df_map, 
    lp_country[, ..cols_lp_country],
    by.x = "global.mapper", 
    by.y = "DL",
    all.x = TRUE, 
    all.y = FALSE
)
 
df_map <- merge(
    df_map, 
    lp_dl[, c("DL", "biogeo_ecor2017_owner", "continent_proximity_owner")],
    by.x = "global.mapper",
    by.y = "DL",
    all.x = TRUE,
    all.y = FALSE
)

df_map[is.na(continent) &
       !is.na(continent_proximity_owner)]$continent <- df_map[
        is.na(continent) &
        !is.na(continent_proximity_owner)
    ]$continent_proximity_owner

df_map$continent_proximity_owner <- NULL

df_map[
    is.na(biogeo_ecor2017) & 
    !is.na(biogeo_ecor2017_owner)
]$biogeo_ecor2017 <- df_map[
        is.na(biogeo_ecor2017) &
        !is.na(biogeo_ecor2017_owner)
    ]$biogeo_ecor2017_owner

df_map$biogeo_ecor2017_owner <- NULL

# Note: this process above is not done for WWF ecoregion (in the lp_dl file)
# but only for Ecoregion2017 as it is going to be used for analyses eventually.

# The centroid of each location was determined to nearest continent or 
# biogeographic realm, and if there was no spatial join, a manual addition was 
# done as seen in the comments column.

df_map1 <- df_map[, 
    list(
        global.mapper_n = paste0(unique(sort(global.mapper)), collapse = "; "),
        continents.mapper_n = paste0(unique(sort(continent)), collapse = "; ")
    ),
    by = "idx"
]

threshold <- .6

df_map2 <- df_map[prop_area_biogeo_wwf >= threshold, 
    list(
        biogeo_wwf.mapper_n = paste0(unique(sort(biogeo_wwf)), collapse = "; ")
    ),
    by = "idx"
]

df_map3 <- df_map[prop_area_biogeo_ecor2017 >= threshold | 
                  is.na(prop_area_biogeo_ecor2017), list(
        biogeo_ecor2017.mapper_n = 
            paste0(unique(sort(biogeo_ecor2017)), collapse = "; ")
    ),
    by = "idx"
]

df_map4 <- df_map[prop_area_Latitude_type2 >= threshold, list(
        latitude_type2.mapper_n = 
        paste0(unique(sort(Latitude_type)), collapse = "; ")
    ),
    by = "idx"
]

# Combine 
df_map <- merge(
    df_map1, df_map2, by = "idx", all.x = TRUE, all.y = TRUE
)

df_map <- merge(
    df_map, df_map3, by = "idx", all.x = TRUE, all.y = TRUE
)

df_map <- merge(
    df_map, df_map4, by = "idx", all.x = TRUE, all.y = TRUE
)

df_map[grepl("; ", continents.mapper_n)]

df <- merge(
    df, 
    df_map,
    by = "idx", 
    all.x = TRUE,
    all.y = F
)


file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
fwrite(df, file)