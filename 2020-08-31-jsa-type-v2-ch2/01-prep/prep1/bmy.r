source('2020-08-31-jsa-type-v2-ch2/01-prep/prep1/util.r')

# Read data
df <- get_df()

# Assess data quality for lat and lon
li_df <- assess_latlon(df, filepath_log) 

# Join spatially
shp_grp <- get_shp_biomes() # read shp file
join_shp <- join_spatially(li_df$join_ll, shp_grp)

# Subset relevant columns
cols_eco <- names(join_shp)[names(join_shp) %in% c(
    cols_ll, "ecoregions2017_biome", "lat", "lon"
)]
join <- join_shp[, ..cols_eco] 
# note: unable to lookup as biomes are not related to country

# Join based on nearest
if(!file.exists(filepath_nearest_loc_biome)) {
    persist_nearest_shp(join, shp_grp, filepath_nearest_loc_biome)
} else {
    join <- join_nearest_biome(join, filepath_nearest_loc_biome)
}

# Subset relevant columns again
cols_ll_final <- c(cols_std,  "BIOME_CAT")
join <- join[, ..cols_ll_final]

# Log relevant statistics
write_ending_log(join, filepath_log)

# Format data and write as data.csv
format_data(join, dir_model_folder)
