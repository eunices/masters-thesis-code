source('2019-07-15-edie-et-al/init_a.r')

library(sf)

# read data frame
df <- get_df1(write=F)
cty <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_1.shp', quiet=T)

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

countries_shp <- sf::st_join(ll, cty, join = st_intersects)
countries <- data.table(data.frame(countries_shp))

lookup <- lookup.pri_div[!duplicated(lookup.pri_div$HASC_1),c("HASC_1", "CTY.STATE.CODE")]

countries <- merge(countries, lookup, by="HASC_1", all.x=T, all.y=F)
setDT(countries)[, c("country", "state") := tstrsplit(CTY.STATE.CODE, "\\.")]
countries$CTY.STATE.CODE <- NULL
countries <- countries[!duplicated(idx)]
