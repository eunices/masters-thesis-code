# Purpose: calculate author metrics

source('2020-08-31-jsa-type-v2/init/init.r')

# Read describers data
file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_1.csv")
df_des <- read_escaped_data_v2(file)

# Read data
file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
df <- read_escaped_data_v2(file)


# Separate df by author for metrics
cols <- c("file", "idx", "duplicated", "genus", "species",
          "author", "status", "full.name.of.describer")

df <- df[duplicated == FALSE, ..cols]

df <- separate_rows(df, full.name.of.describer, sep = "; ")





 
file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_2.csv")
fwrite(df, file)