source('2019-07-15-edie-et-al/init_a.r')

library(sf)

# read data frame
df <- get_df1(write=F); dimdf <- dim(df)
teow <- sf::st_read('data/geo_processed/teow/official/wwf_terr_ecos_dissolved.shp', quiet=T)
lookup_cty <- fread('data/lookup/2019-05-29-statoid-country-codes.csv', na=c(''), encoding='UTF-8')

# derive new columns for those with lat lon
is_latlon_absent <- (is.na(df$lat) | is.na(df$lon))
is_cty_absent <- df$type.country.n == ""

cols <- c("idx", "lat", "lon", 
          "type.country.n", "type.state.n", 
          "type.country.n.full", "type.state.n.full", 
          "type.country.n", "type.state.n", 
          "type.locality.verbatim", "type.locality.updated", 
          "flag", "source.of.latlon.n", 
          "full.name.of.describer" , "date.n")

ll <- sf::st_as_sf(df[!is_latlon_absent, ..cols], 
                   coords = c('lon', 'lat'),
                   crs = "+init=epsg:4326")

join_shp <- sf::st_join(ll, teow, join = st_intersects)
join_shp <- data.table(data.frame(join_shp))[order(as.numeric(idx))]
join_shp$geometry <- as.character(join_shp$geometry)
names(join_shp)[which(names(join_shp)=="REALM_EDIT")] <- "biogeo_wwf"

# derive new columns for those with country/ country + state only
join_cty <- merge(df[is_latlon_absent & !is_cty_absent, ..cols],
                  lookup_cty[prop_area_biogeo_wwf >= 0.6, c("DL", "biogeo_wwf")], 
                  by.x="type.country.n", by.y="DL", all.x=T, all.y=F)
join_cty <- join_cty[!is.na(biogeo_wwf),]

# combine datasets
join <- rbind(join_shp, join_cty, fill=T)
join$lat <- NULL; join$lon <-  NULL

write.csv(join, paste0(dir_analysis_edie_tmp, 'format.csv'), row.names=F)
