
geocode_lat_long <- function(to_geocode) {
    # to_geocode is a list of localities
    # doing it one locality at a time saves the dataframe if the geocode api does not work
    geocoded <- data.frame(lat=as.numeric(), lon=as.numeric())
    for (i in 1:length(to_geocode)) {
        if (to_geocode[i] == "" | is.na(to_geocode[i])) {
            geocoded <- rbind(geocoded, data.frame(lat=NA, lon=NA))
            print(paste0("Not geocoded for row ", i))
        } else {
            geocoded_row <- try(geocode(to_geocode[i]))
            geocoded_row <- as.data.frame(geocoded_row)
            geocoded <- rbind(geocoded, geocoded_row)
            print(paste0("Geocoded for row ", i))
        }
    }
    geocoded
}
