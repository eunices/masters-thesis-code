# This will be the R script to call various functions from the package
# Initialise
source(paste0("2020-03-08-red-list-sg-hym/03-analyse/R/", "init.r"))
source(paste0(folder_script, "util.r"))
source(paste0(folder_script, "fun.r"))

# Libraries
library(sf)

# Parameters
set.seed(2020)

# Scripts/tests

#-------------------------------------------------------------------------------------------------
# Geocode localities with only road names
#! to be done externally

#-------------------------------------------------------------------------------------------------
# Classify parks
p = classify_parks()
p

#-------------------------------------------------------------------------------------------------
# Assign IUCN category

df_species <- df_test

df_species_epsg <- 4326

coord_names <- c("X", "Y")

geolayers <- list(v_islands=NA, 
				  v_parks_nat_res=NA,
				  v_parks_all=NA,
				  v_greenery=NA,
				  v_planning_areas=NA) 

# TODO: clean data - georeference localities 

# function
v_species <- create_species_sf_obj(df_species, df_species_epsg, coord_names)







# df_iucn <- generate_iucn_categories(df_test, epsg, coords, geolayers)
# df_iucn

#-------------------------------------------------------------------------------------------------
# Plotting these points on a map


