# Purpose: clean dates / journal names / publication

source('2020-08-31-jsa-type-v2/init/init.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
df <- read_escaped_data_v2(file)

# Clean dates -------------------------------------------------------------

# Manually
# TODO: 

cfile <- paste0(v2_dir_data_raw_clean, "clean04-check-date-lag_edit.csv")

if(file.exists(cfile)) {
    df_dates <- read_escaped_data_v2(cfile)
}


# Description dates
df[date<1700]$date <- NA

# Collection dates
df[date.of.type.yyyy<1500]$date.of.type.yyyy <- NA

# Lag between dates
cols <- unique(
    c(bcol, "date", "date.of.type.yyyy", "date.lag"), 
    fromLast = TRUE
)

cfile <- paste0(v2_dir_data_raw_clean, "clean04-check-date-lag.csv")
fwrite(df[date.of.type.yyyy > date, ..cols], cfile)


# Clean journal names ----------------------------------------------------------



# Clean publications -----------------------------------------------------------
# TODO: 




file <- paste0(v2_dir_data_raw, v2_basefile, "_6.csv")
fwrite(df, file)