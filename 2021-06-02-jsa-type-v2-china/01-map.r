source('2021-06-02-jsa-type-v2-china/init.r')

# Create map dataset

df_all <- get_df()

df <- df_all[
    duplicated == FALSE & status %in% c("Valid species", "Synonym"), 
    c("idx", "genus", "species", "status", "date", "lat", "lon")
]

df$lat <- as.numeric(df$lat)
df$lon <- as.numeric(df$lon)

df <- df[!(is.na(lat) | is.na(lon))]

dir_geo_chn <- "data/geo_processed/gadm/china/gadm36_CHN_shp/"
list.files(dir_geo_chn)
v_chn <- st_read(paste0(dir_geo_chn, "gadm36_CHN_0.shp"))
v_chn_pri <- st_read(paste0(dir_geo_chn, "gadm36_CHN_1.shp"))

v_df <- st_as_sf(df, coords = c("lon", "lat"), crs = wgs84)
v_df <- st_join(v_df, v_chn_pri, join = st_intersects)

df_merged <- data.table(v_df)[, c("idx", "GID_0", "NAME_1")]
names(df_merged) <- c("idx", "china", "pri")


df <- merge(df, df_merged, by="idx")

wfile <- paste0(v2_dir_china, "01-map/lat-lon.csv")
fwrite(df, wfile, na="")







