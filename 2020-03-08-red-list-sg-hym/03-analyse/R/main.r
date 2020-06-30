# This will be the R script to call various functions from the package
# Initialise
source(paste0("2020-03-08-red-list-sg-hym/03-analyse/R/", "init.r"))
source(paste0(folder_script, "util.r"))
source(paste0(folder_script, "lookup.r"))
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



#------------------ Data 

df_species <- df_test
df_species$id <- 1:dim(df_species)[1]

coord_columns <- c("X", "Y")

vector_layers <- list(v_islands=NA, 
		     		  v_parks_nat_res=NA,
		     		  v_parks_all=NA,
		     		  v_greenery=NA,
		     		  v_planning_areas=NA) 


identifier_columns <- c("id", "species", "type")
collection_date_column <- "collection_date"

date_cut_off <- as.Date("1960-01-01")



#------------------ Individual functions


# TODO: clean data - georeference localities 

df_habitat <- 
	generate_habitat(df_species, vector_layers, identifier_columns, coord_columns)

# TODO: manual cleaning of those records with no site_name_final, habitat_final and habitat_IUCN
# These have column "note" as "HABITAT ASSIGNMENT: MANUAL"

# Site names, habitat, habitat_IUCN be the same as those provided 
# in the lookup list using "generate_site_names()"

df_habitat_sp_mat <- 
	generate_habitat_sp_matrix(df_habitat, collection_date_column, date_cut_off)

df_bool <- 
	generate_boolean_check_vars(df_species, date_cut_off)

df_iucn <- 
	generate_iucn_status(df_bool, df_habitat_sp_mat)

table(df_iucn$category_IUCN)


#------------------ Entire pipeline

df_iucn <- generate_iucn_table(df_species, date_cut_off, 
							   coord_columns, identifier_columns, collection_date_column)

df_iucn[, list(.N, 
			   .N/dim(df_iucn)[1] * 100), by="category_IUCN"]

#-------------------------------------------------------------------------------------------------
# Plotting these points on a map


