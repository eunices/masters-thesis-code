# Initialise
source(paste0("2020-03-08-red-list-sg-hym/03-analyse/R/", "init.r"))
source(paste0(folder_script, "util.r"))
source(paste0(folder_script, "fun.r"))

# Libraries
library(sf)

# Scripts/tests


#-------------------------------------------------------------------------------------------------
# Read all the shapefiles layers
#! to be done externally; done in init.r


# Read the ant coordinates, supplied as a matrix or data.frame
#! to be done externally; done in init.r for test data


# Geocode localities with only road names
#! to be done externally

#-------------------------------------------------------------------------------------------------
# Classify parks
p = classify_parks()
p

#-------------------------------------------------------------------------------------------------
# Assign IUCN category

df_iucn = generate_iucn_categories(df_test, 4326, c("X", "Y"), list(v_islands=NA, 
				                                                    v_parks_nat_res=NA,
				                                                    v_parks_all=NA,
				                                                    v_greenery=NA,
				                                                    v_planning_areas=NA))
df_iucn

#-------------------------------------------------------------------------------------------------
# Plotting these points on a map


