# Parameters

# Columns associated with the different dataset

cols_std <- c("idx", "full.name.of.describer", "date")
cols_dis <- c("genus", "species", "author", "date")

cols_ll  <- c(
    "idx", "lat_n", "lon_n", "type.country_n.A3", 
    "type.locality.verbatim", "type.locality.updated",
    "full.name.of.describer", "date"
)

# Min. thresholds for latitude
lat <- list( 
    trop = 0,
    subtrop = 23.436740, 
    temp = 35.000000, 
    pol = 66.563250
)

lat_splits <- list(
    trop = 2,
    temp = 3
)

# Lookup files

lookup_cty <- get_lp_statoid()
lookup_cty$area_km2 <- as.numeric(lookup_cty$area_km2)
lookup_cty_subset_ll <- lookup_cty[
    area_km2 < quantile(area_km2, .95, na.rm = T),
] # exclude the largest countries TODO: may modify to look at % of country 
lookup_cty_subset_biogeo <- lookup_cty[prop_area_biogeo_wwf >= 0.6,] # subset

lookup_bm <- get_lp_biome()

# Extra data lookup files 

filepath_nearest_loc_biome <- paste0(
    v2_dir_data_ch2, 
    '2020-11-03-nearest-loc.csv'
)
# note: this process took >3h so it was persisted instead

