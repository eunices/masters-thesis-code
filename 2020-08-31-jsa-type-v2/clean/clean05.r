# Purpose: clean author biodata

source('2020-08-31-jsa-type-v2/init/init.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
df <- read_escaped_data_v2(file)


# Clean author biodata ---------------------------------------------------------

# copy script from 
# run_describer_split_loop(df_auth, strsplit_cty = " ")

file <- paste0(v2_dir_data_raw, v2_basefile, "_6.csv")
fwrite(df, file)