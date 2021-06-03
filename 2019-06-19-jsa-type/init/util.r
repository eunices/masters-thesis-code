suppressMessages(library(ggmap))
suppressMessages(library(revgeo))
source('keys.r')


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

# revgeo(longitude=103.959701, latitude=1.379857, provider = 'google', output='frame', API=geocode_api)

ybreaks = function(lims, breaks) {
  min = round(lims[1]/breaks, 0)
  seq(min*breaks, lims[2], breaks)
}

ybreaks.1 = function(lims) {ybreaks(lims, 0.1)}
ybreaks1 = function(lims) {ybreaks(lims, 1)}
ybreaks2 = function(lims) {ybreaks(lims, 2)}
ybreaks5 = function(lims) {ybreaks(lims, 5)}
ybreaks10 = function(lims) {ybreaks(lims, 10)}
ybreaks20 = function(lims) {ybreaks(lims, 20)}
ybreaks50 = function(lims) {ybreaks(lims, 50)}
ybreaks100 = function(lims) {ybreaks(lims, 100)}
ybreaks200 = function(lims) {ybreaks(lims, 200)}
ybreaks1000 = function(lims) {ybreaks(lims, 1000)}
ybreaks2000 = function(lims) {ybreaks(lims, 2000)}
ybreaks4000 = function(lims) {ybreaks(lims, 4000)}


read_escaped_data = function(filepath, escape=T) {
    df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
    if(escape) df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
    df
}