# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Create other datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
dir_script <- '2020-08-31-jsa-type-v2/'
source(paste0(dir_script, "subset.r"))

df <- get_df()

df <- df[
    duplicated == FALSE & status %in% c("Valid species", "Synonym"), 
    c("genus", "species", "status", "date", "lat", "lon")
]

df <- unique(df[, c("date", "lat", "lon")])[order(date)]
df <- df[!(is.na(lat) | is.na(lon))]
wfile <- paste0(v2_dir_data_webapp, "ch0-geo.csv")
fwrite(df, wfile, na="")


network <- get_describer_network()
wfile <- paste0(v2_dir_data_webapp, "ch3-fig-03-data.csv")
fwrite(network, wfile, na="")

