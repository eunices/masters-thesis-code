# Cleaning up the files
# Subset columns, calculate area
# Giving names to each big park (manual), merge files (manual)

source('2020-03-08-red-list-sg-hym/init.r')

# Libraries
library(sf)
library(data.table)
library(googleway)

# Files
parks = paste0(folder_geo, "2020-03-08-sdcp-parks/G_MP08_PARKS_PL.shp")
islands = paste0(folder_geo, "final/islands.gpkg")
parks_sml = paste0(folder_geo, "final/parks-small.gpkg")

new_parks = paste0(folder_new_parks, "sg-parks.shp")
new_parks2 = paste0(folder_new_parks, "sg-parks-complete.shp")
new_parks3a = paste0(folder_new_parks, "sg-parks-missing-small.shp")
new_parks3b = paste0(folder_new_parks, "sg-parks-missing-big.shp")
new_parks4 = paste0(folder_new_parks, "parks-all.gpkg")
new_parks5 = paste0(folder_new_parks, "parks-all-non-islands.gpkg")
new_parks5_names = paste0(folder_new_parks, "parks-all-non-islands.csv")
new_parks6 = paste0(folder_new_parks, "parks-all-non-islands-edit.gpkg")


####################################################################################################
####################################################################################################
####################################################################################################
# PART 1

# Read bee sites
sites = fread("data/red-list-sg-ants/2020-03-09/2020-03-09-park-site-habitat-data-bee-geocoded-clean.csv")
s = st_as_sf(sites, coords = c("lon", "lat")) 
s = st_set_crs(s, wgs84) # set geographic CRS
s = st_transform(s, svy21)

# Read polygons
p = st_read(parks)
p = st_set_crs(p, svy21)

# Read islands
is = st_read(islands)
is = st_set_crs(is, svy21)

# Read small parks (after removing islands)
p_sml = st_read(parks_sml)
p_sml = st_set_crs(p_sml, svy21)

# Join datasets
join = st_join(p, s)

# Remove names unecessary
cols = c("NAME", "Site name", "src", "FMEL_UPD_D", "OBJECTID", "UNIQUE_ID", "INC_CRC", "geometry")
join = join[, cols]

# Combine names
join$`Site name` = toupper(join$`Site name`)
join$NAME = as.character(join$NAME)
join$NAME = ifelse(!is.na(join$`Site name`) & is.na(join$NAME), 
                   as.character(join$`Site name`), as.character(join$NAME)) # use new site name if empty

# Rename names
names(join) = toupper(gsub(" ", "_", names(join)))
join$SITE_NAME = NULL # remove redundant name
names(join)[which(names(join)=="GEOMETRY")] = "geometry"

# With names
join_names = join[!is.na(join$NAME),]
join_no_names = join[is.na(join$NAME),]

# Modify those without names

# Get area
threshold = 60000 # sq m           for names
threshold = 600
hist(st_area(join_no_names))
hist(st_area(join_no_names)[as.numeric(st_area(join_no_names))<10000])
hist(st_area(join_no_names)[as.numeric(st_area(join_no_names))<threshold])
join_no_names$AREA_SQ_M = as.numeric(st_area(join_no_names))
join_no_names$AREA_THRESHOLD = ifelse(join_no_names$AREA_SQ_M>threshold, TRUE, FALSE)
table(join_no_names$AREA_THRESHOLD)

# Get centroid
join_no_names_centroid = st_centroid(join_no_names)
join_no_names_centroid = st_transform(join_no_names_centroid, wgs84)
join_no_names$LAT = st_coordinates(join_no_names_centroid)[,2]
join_no_names$LON = st_coordinates(join_no_names_centroid)[,1]

# # Further subset dataset
# # join_no_names small parks
# join_no_names_a = join_no_names[!join_no_names$AREA_THRESHOLD,]
# # join_no_names big parks
# join_no_names_b = join_no_names[join_no_names$AREA_THRESHOLD,]

# # Reverse geocode small parks to road names
# to_rev_geo <- join_no_names_a[, c('LAT', 'LON')]

# addresses = c()
# for (i in 1:dim(to_rev_geo)[1]) {
#   print(paste0("Geocoding ", i))
#   latlon = c(to_rev_geo[i,]$LAT, to_rev_geo[i,]$LON)
#   rev_geocoded = google_reverse_geocode(latlon, result_type=NULL, 
#                                         location_type=NULL, language=NULL, 
#                                         key=geocode_api, simplify=TRUE)
#   addresses = c(addresses, rev_geocoded$results$formatted_address[1])
# }

# join_no_names_a$NAME = "A SMALL GREEN SPACE"
# join_no_names_a$ADDRESS = addresses



dir.create(paste0(folder_geo, folder_new_parks))
st_write(join, new_parks)
st_write(join_names, new_parks2)
st_write(join_names, new_parks2)
st_write(join_no_names_a, new_parks3a) 
st_write(join_no_names_b, new_parks3b) # all of these were removed in the end, as they were mostly in islands


####################################################################################################
####################################################################################################
####################################################################################################
# PART 2


# Joing small islands with small parks with those with names
names(join_names)
# MANUAL step done before: remove those small parks in islands in parks-all.gpkg
names(p_sml)

join_names$ADDRESS = ""
p_sml$geometry = p_sml$geom
st_geometry(p_sml) = "geometry"
p_sml$geom = NULL
p_sml$AREA_TH = NULL
p_sml$LAT = NULL
p_sml$LON = NULL
p_sml$AREA_SQ = NULL

p_sml$ADDRESS = as.character(p_sml$ADDRESS)


names(join_names)[which(names(join_names)=="FMEL_UPD_D")] = "FMEL_UP"
names(join_names)[which(names(join_names)=="OBJECTID")] = "OBJECTI"
names(join_names)[which(names(join_names)=="UNIQUE_ID")] = "UNIQUE_"

all_parks = rbind(join_names, p_sml)
all_parks$AREA_SQ_M = st_area(all_parks)
all_parks$AREA_THRES = ifelse(as.numeric(all_parks$AREA_SQ_M) > threshold, TRUE, FALSE)
# MANUAL: goes through merging of parks in parks-all-non-islands.gpkg



####################################################################################################
####################################################################################################
####################################################################################################
# PART 3

# TODO: lastly after cleaning up the parks file, to calculate area
# have one file and split into 2 files (small and big)
# assign to habitat type
# change all to gpkg svy21

all_parks_edited = st_read(new_parks5)
all_parks_edited = st_set_crs(all_parks_edited, svy21)

all_parks_edited$AREA_SQ_M = st_area(all_parks_edited)
all_parks_edited$AREA_THRES = NULL

all_parks_edited = all_parks_edited[order(all_parks_edited$UNIQUE_),] 
all_parks_edited$OBJECTI = 1:dim(all_parks_edited)[1] # identifier 

all_parks_edited$FMEL_UP
all_parks_edited$INC_CRC = as.character(all_parks_edited$INC_CRC)
table(duplicated(all_parks_edited$OBJECTI))
all_parks_edited[(duplicated(all_parks_edited$UNIQUE_)),]

# check_names = all_parks_edited[as.numeric(all_parks_edited$AREA_SQ_M)>=10000 & all_parks_edited$NAME == "A SMALL GREEN SPACE" ,]
# st_geometry(check_names) = NULL
# write.csv(check_names, new_parks5_names, row.names=F)

# Merge names back in
new_names = fread(new_parks5_names)
new_names = new_names[!is.na(NEW_NAME) & NEW_NAME != ""]
new_names = new_names[,c("UNIQUE_", "NEW_NAME")]
all_parks_edited = merge(all_parks_edited, new_names, all.x=T, all.y=F, by="UNIQUE_")
all_parks_edited$NAME = as.character(all_parks_edited$NAME)
all_parks_edited$NAME = ifelse(!is.na(all_parks_edited$NEW_NAME), all_parks_edited$NEW_NAME, 
                               all_parks_edited$NAME)

all_parks_edited$SRC = NULL
all_parks_edited$NEW_NAME = NULL

all_parks_edited[all_parks_edited$INC_CRC == "C64B25E5A89EDE1F" & !is.na(all_parks_edited$UNIQUE_),]$NAME = toupper("Compassvale Ancillary Park")

all_parks_edited = all_parks_edited[,c("NAME", "ADDRESS", "AREA_SQ_M",
                                       "FMEL_UP", "UNIQUE_", "INC_CRC", "geometry")]

st_write(all_parks_edited, new_parks6)

# Wendy says to use a threshold of 100 * 100 m
