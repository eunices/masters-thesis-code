source('2020-08-31-jsa-type-v2/init/init.r')

# See raw files

list.files(v2_dir_data_raw_raw, pattern = "^Apoidea*.*.\\.xlsb")


# Read excel files

file <- paste0(v2_dir_data_raw, v2_basefile, "_2 map.csv")
df <- read_escaped_data_v2(file)

dim(df)