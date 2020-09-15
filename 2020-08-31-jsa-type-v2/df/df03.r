# Purpose: calculate taxonomic effort over time

source('2020-08-31-jsa-type-v2/init/init.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
df <- read_escaped_data_v2(file)


# copy from df1.6.r


file <- paste0(v2_dir_data_raw, v2_basefile, "-describer-effort.csv")
fwrite(df, file)