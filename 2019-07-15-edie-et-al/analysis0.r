source('2019-07-15-edie-et-al/init_a.r')

library(sf)

# read data frame
df <- get_df1(write=F)
teow <- sf::st_read('data/geo_processed/teow/official/wwf_terr_ecos_dissolved.shp', quiet=T)

# derive new columns
ll <- sf::st_as_sf(df[!(is.na(lat) | is.na(lon)), 
                     c("idx", "lat", "lon", 
                       "type.country.n", "type.state.n", 
                       "type.country.n.full", "type.state.n.full", 
                       "type.country", "type.state", 
                       "type.locality.verbatim", "type.locality.updated", 
                       "flag", "source.of.latlon.n", 
                       "full.name.of.describer" , "date.n")], 
                   coords = c('lon', 'lat'),
                   crs = "+init=epsg:4326")

join_shp <- sf::st_join(ll, teow, join = st_intersects)
join_shp <- data.table(data.frame(join_shp))[order(as.numeric(idx))]
join_shp$geometry <- as.character(join_shp$geometry)

write.csv(join_shp, paste0(dir_analysis_edie_tmp, 'format.csv'))
