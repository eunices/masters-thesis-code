# Purpose: clean global mapper distribution

source('2020-08-31-jsa-type-v2/init/init.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_4.csv")
df <- read_escaped_data_v2(file)

file <- paste0(v2_dir_data_raw, v2_basefile, "_1.csv")
df_raw <- read_escaped_data_v2(file)


# Filter global mapper ---------------------------------------------------------

df_map1 <- df_raw[, c("idx", "global.mapper")]

df_map2 <- df_raw[, c("idx", "merged.global.mapper")]
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

df_map <- merge(
    df_map, lp_country[, c("DL", "A-3")],
    by.x = "global.mapper", by.y = "DL",
    all.x = T, all.y = F
)

df_map <- merge(
    df_map, lp_dl[, c("DL", "GID_0_owner")],
    by.x = "global.mapper", by.y = "DL",
    all.x = T, all.y = F
)

df_map

df_map[is.na(`A-3`) & !is.na(GID_0_owner), ]$`A-3` <-
    df_map[is.na(`A-3`) & !is.na(GID_0_owner), ]$`GID_0_owner`

table(is.na(df_map$`A-3`)) #!CHECK: number of NA

df_map <- df_map[!is.na(`A-3`)]

df_map <- df_map[, 
    list(global.mapper_n = paste0(sort(global.mapper), collapse = " ")),
    by = "idx"
]

df <- merge(
    df, 
    df_map,
    all.x = TRUE,
    all.y = F
)


file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
fwrite(df, file)