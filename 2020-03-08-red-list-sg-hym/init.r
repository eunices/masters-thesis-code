
source("2019-06-19-jsa-type/init/util.r")
source("keys.r")
register_google(key = geocode_api)

# Filepaths
folder_geo = "data/geo_processed/red-list-sg-ants/"
folder_new_parks = paste0(folder_geo, "2020-05-25-sg-parks/")
folder_final = paste0(folder_geo, "final/")
folder_rast = paste0(folder_geo, "2020-03-08-high-res-sg-map/Figure 1/")

# Constants
wgs84 = 4326
svy21 = 3414
svy21_proj4 = "+proj=tmerc +lat_0=1.366666666666667 +lon_0=103.8333333333333 +k=1 +x_0=28001.642 +y_0=38744.572 +ellps=WGS84 +units=m +no_defs"