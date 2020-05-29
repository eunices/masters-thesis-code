
# Libraries
library(sf)
library(data.table)

# Folders
folder_data_root = "data/geo_processed/red-list-sg-ants/"
folder_data = paste0(folder_data_root, "final/")
folder_test = paste0(folder_data_root, "2020-05-29-random-pts/test/") # test data
folder_script = "2020-03-08-red-list-sg-hym/03-analyse/R/"

# File names
## For data analyses
g_islands = paste0(folder_data, "islands.gpkg")
g_parks_nat_res = paste0(folder_data, "parks-nature-reserves.gpkg")
g_parks_all = paste0(folder_data, "parks-all.gpkg")
g_greenery = paste0(folder_data, "greenery.gpkg")
g_planning_areas = paste0(folder_data, "planning-areas.gpkg")

## For visualisation purposes
g_reservoirs = paste0(folder_data, "reservoirs.gpkg")

## Test data
csv_test = paste0(folder_test, "test-points.csv")

# Read geospatial files
vec_islands = st_read(g_islands)
vec_parks_nat_res = st_read(g_parks_nat_res)
vec_parks_all = st_read(g_parks_all)
vec_greenery = st_read(g_greenery)
vec_planning_areas = st_read(g_planning_areas)

vec_reservoirs = st_read(g_reservoirs)

df_test = fread(csv_test)