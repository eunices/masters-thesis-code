# Parameters

# Columns associated with the different dataset

cols_std <- c("idx", "full.name.of.describer", "date")
cols_dis <- c("genus", "species", "author", "date")

cols_ll  <- c(
    "idx", "lat_n", "lon_n", "type.country_n.A3", 
    "type.locality.verbatim", "type.locality.updated",
    "full.name.of.describer", "date"
)

# Thresholds for latitude

ltrop <- 23.436740; lsubtrop <- 35.000000
ltemp <- 35.000000; lpol <- 66.563250



# Lookup files

lookup_cty <- get_lp_statoid()
lookup_cty_subset <- lookup_cty[prop_area_biogeo_wwf >= 0.6,] # subset

lookup_bm <- get_lp_biome()

# Extra data lookup files 

filepath_nearest_loc_biome <- paste0(
    v2_dir_data_ch2, 
    '2020-11-03-nearest-loc.csv'
)
# note: this process took >3h so it was persisted instead

