source('keys.r')
source('2019-06-19-jsa-type/init/helper.r')

register_google(key = geocode_api)

library(data.table)
library(googleway)

d1 <- fread('data/2020-03-09-red-list-sg-ants/2020-03-09-park-site-habitat-data.csv')
d2 <- fread('data/2020-03-09-red-list-sg-ants/2020-03-09-park-site-habitat-data-bee.csv')

# Reverse geocode d1 for place # TODO:
to_rev_geo <- d1[, c('lat', 'lon')]
latlon <- c(to_rev_geo[1,]$lat, to_rev_geo[1,]$lon)
rev_geocoded <- google_reverse_geocode(latlon, result_type = NULL, 
                                       location_type = NULL, language = NULL, 
                                       key = geocode_api, simplify = TRUE)
names(rev_geocoded)
names(rev_geocoded$plus_code)
rev_geocoded$plus_code$compound_code
rev_geocoded$plus_code$global_code
names(rev_geocoded$results)
rev_geocoded$results$address_components
rev_geocoded$results$formatted_address
rev_geocoded$results$geometry
rev_geocoded$results$place_id
rev_geocoded$results$plus_code
rev_geocoded$results$types

rev_geocoded$status

for (i in 2:dim(to_rev_geo)[1]) {
    rev_geocoded_row <- revgeo(longitude=to_rev_geo[i, c("lon")],
                               latitude=to_rev_geo[i, c("lat")],
                               provider = 'google', output='frame')#, API=geocode_api)
    rev_geocoded_row
    rev_geocoded <- cbind(rev_geocoded, rev_geocoded_row)
}

# Geocode d2
to_geocode <- paste0(d2$`Site name`, ", ", "Singapore")
geocoded <- geocode_lat_long(to_geocode)
filename <- 'data/2020-03-09-red-list-sg-ants/2020-03-09-park-site-habitat-data-bee-geocoded.csv'
write.csv(cbind(to_geocode, geocoded), filename, na='', row.names=F, fileEncoding="UTF-8") 