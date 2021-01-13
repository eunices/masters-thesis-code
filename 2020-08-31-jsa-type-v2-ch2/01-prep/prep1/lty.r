source(paste0(dir_script_ed, '/01-prep/prep1/util.r'))

# Read data
df <- get_species_raw_data()

# Assess data quality for lat and lon
li_df <- assess_latlon(df, filepath_log) 

# Lookup based on country for those rows with no lat/lon
# to assign latitude column
join_cty <- lookup_for_no_ll_lt(li_df$join_cty)

# Combine files
join <- rbind(li_df$join_ll, join_cty, fill = T)

# Create latitude reference lines
ref_lines <- create_latitude_lines(lat, lat_splits)

# Create latitude column
join <- assign_latitude(join, ref_lines)
join[, 
    list(min=min(lat_n), max=max(lat_n), .N), by="latitude"
][order(min)]

# Subset relevant columns
cols_ll_final <- c(cols_std,  "latitude")
join <- join[, ..cols_ll_final]

# Log relevant statistics
write_ending_log(join, filepath_log)

# Format data and write as data.csv
format_data(join, dir_model_folder)

