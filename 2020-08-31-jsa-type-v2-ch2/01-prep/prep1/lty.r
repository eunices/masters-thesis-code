source('2020-08-31-jsa-type-v2-ch2/01-prep/prep1/util.r')

# Read data
df <- get_df()

# Assess data quality for lat and lon
li_df <- assess_latlon(df, filepath_log) 

# Assign latitude line based on lat/lon
# join <- assign_lat_lon(li_df$join_ll)

# Lookup based on country for those rows with no lat/lon
join_cty <- lookup_for_no_ll(li_df$join_cty)

# Combine files
join <- rbind(join_shp, join_cty, fill = T)

# Subset relevant columns
cols_ll_final <- c(cols_std,  "BIOME_CAT")
join <- join[, ..cols_ll_final]

# Log relevant statistics
write_ending_log(join, filepath_log)

# Format data and write as data.csv
format_data(join, dir_model_folder)
