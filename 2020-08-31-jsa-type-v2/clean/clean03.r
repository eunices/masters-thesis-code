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

df_map <- separate_rows(df_map, global.mapper, sep = " ")
df_map <- df_map[!is.na(global.mapper), ]

df_map


file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
fwrite(df, file)