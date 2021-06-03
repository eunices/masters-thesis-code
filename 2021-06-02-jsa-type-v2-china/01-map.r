source('2021-06-02-jsa-type-v2-china/init.r')

# Create map dataset

df_all <- get_df()

df_all <- df_all[duplicated == FALSE]

df_all$lat_old <- df_all$lat
df$lon_old <- df_all$lon

df_all$lat <- as.numeric(df_all$lat)
df_all$lon <- as.numeric(df_all$lon)


df <- df_all[
    status %in% c("Valid species", "Synonym"), 
    c("idx", "genus", "species", "status", "date", "lat", "lon", "type.country_n", "type.state")
]
# note: "type.state" field was not cleaned!

# Checks
df_all[type.country_n == "CH", .N, by=status]
df_all[type.country_n == "CH" & (is.na(lat) | is.na(lon)), .N, by=status]
df_all[type.country_n == "CH" & !(is.na(lat) | is.na(lon)), .N, by=status]

# Note: mapped/ plotted in species description curves for China, using lat/lon
# which means 160 valid species (of 665 species; 24.1%) were omitted from the map and curves
# (these with country China, but no lat/lon)
# but were included in 04-flow and 05-type-repo (also including synonyms)
round((160/665*100), 1) 


dir_geo_chn <- "data/geo_processed/gadm/china/gadm36_CHN_shp/"
list.files(dir_geo_chn)
v_chn <- st_read(paste0(dir_geo_chn, "gadm36_CHN_0.shp"))
v_chn_pri <- st_read(paste0(dir_geo_chn, "gadm36_CHN_1.shp"))

v_df <- st_as_sf(df[!(is.na(lat) | is.na(lon))], coords = c("lon", "lat"), crs = wgs84)
v_df <- st_transform(v_df, st_crs(v_chn_pri))
v_df <- st_join(v_df, v_chn_pri, join = st_intersects)

df_merged <- data.table(v_df)[, c("idx", "GID_0", "NAME_1")]
names(df_merged) <- c("idx", "china", "pri")

df <- merge(df, df_merged, by="idx", all.x=T, all.y=F)
dim(df)


v_chn_pri <- st_read(paste0(v2_dir_china, "01-map/Prov_ann/Prov_ann.shp"))

v_df <- st_as_sf(df[!(is.na(lat) | is.na(lon))], coords = c("lon", "lat"), crs = wgs84)
v_df <- st_transform(v_df, st_crs(v_chn_pri))
v_df <- st_join(v_df, v_chn_pri, join = st_intersects)

df_merged <- data.table(v_df)[, c("idx", "Eng_NAME")]
names(df_merged) <- c("idx", "pri")

df <- merge(df, df_merged, by="idx", all.x=T, all.y=F, suffixes=c("", "_max"))
dim(df)

wfile <- paste0(v2_dir_china, "01-map/lat-lon.csv")
fwrite(df, wfile, na="")





# Mismatches 
# fortunately these are few can be trivialised (/ignored)

df[type.country_n=="CH" & is.na(china)] 
# due to boundary issues of GADM shp or plotting in the sea
# (should be rectified on the GADM shp, or lat/lon should be modified)

df[type.country_n!="CH" & china=="CHN"] 
# erroneous georeferencing (should be excluded)

df[type.country_n == "CH", .N, by=status]
# tally with values above