# Purpose: clean author names

source('2020-08-31-jsa-type-v2/init/init.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_3.csv")
df <- read_escaped_data_v2(file)


# Standardize author names -----------------------------------------------------


# Full name consistent with author ---------------------------------------------



file <- paste0(v2_dir_data_raw, v2_basefile, "_4.csv")
fwrite(df, file)