library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(spatialEco)


filepath <- 'data/geo_processed/gadm/gadm36_0_utf8_continents.shp'
# filepath <- 'data/geo_processed/gadm/gadm36_1_utf8.shp'
shp.pol_boundaries <- rgdal::readOGR(filepath, use_iconv=TRUE, encoding = "UTF-8")
shp.pol_boundaries@data$idx <- 1:dim(shp.pol_boundaries@data)[1]


moll <- '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
shp.pol_boundaries_moll <- sp::spTransform(shp.pol_boundaries, raster::crs(moll))

gCentroidWithin <- function(pol) {
    # From https://stackoverflow.com/questions/44327994/calculate-centroid-within-inside-a-spatialpolygon

    pol$.tmpID <- 1:length(pol)
    # initially create centroid points with gCentroid
    initialCents <- gCentroid(pol, byid = T)

    # add data of the polygons to the centroids
    centsDF <- SpatialPointsDataFrame(initialCents, pol@data)
    centsDF$isCentroid <- TRUE

    # check whether the centroids are actually INSIDE their polygon
    centsInOwnPoly <- sapply(1:length(pol), function(x) {
        gIntersects(pol[x,], centsDF[x, ])
    })
    # substitue outside centroids with points INSIDE the polygon
    newPoints <- SpatialPointsDataFrame(gPointOnSurface(pol[!centsInOwnPoly, ], 
                                                        byid = T), 
                                        pol@data[!centsInOwnPoly,])
    newPoints$isCentroid <- FALSE
    centsDF <- rbind(centsDF[centsInOwnPoly,], newPoints)

    # order the points like their polygon counterpart based on `.tmpID`
    centsDF <- centsDF[order(centsDF$.tmpID),]

    # remove `.tmpID` column
    centsDF@data <- centsDF@data[, - which(names(centsDF@data) == ".tmpID")]

    cat(paste(length(pol), "polygons;", sum(centsInOwnPoly), "actual centroids;", 
                sum(!centsInOwnPoly), "Points corrected \n"))

    return(centsDF)
}

shp.pol_boundaries_moll.cen <- gCentroidWithin(shp.pol_boundaries_moll) # centroid

shp.pol_boundaries.cen <- sp::spTransform(shp.pol_boundaries_moll.cen, raster::crs(shp.pol_boundaries))
shp.pol_boundaries.cen <- SpatialPointsDataFrame(shp.pol_boundaries.cen, 
                                                 shp.pol_boundaries@data,
                                                 match.ID=F)




#################### For WWF's ecoregions

filepath <- 'data/geo_processed/teow/official/wwf_terr_ecos_dissolved.shp'
shp.terr_eco <- rgdal::readOGR(filepath, use_iconv=TRUE, encoding = "UTF-8")

shp.pol_boundaries.cen <- spatialEco::point.in.poly(shp.pol_boundaries.cen, shp.terr_eco)
data <- merge(shp.pol_boundaries@data,
              shp.pol_boundaries.cen@data[,c('idx', 'REALM_EDIT')],
              by='idx', all.x=T, all.y=F)

data[] <- lapply(data, as.character)

dim(shp.pol_boundaries@data); dim(data)

table(is.na(data$REALM_EDIT))
cty_realms <- unique(data[,c('NAME_0', 'REALM_EDIT')])
cty_realms <- cty_realms[!is.na(cty_realms$REALM_EDIT),][order('NAME_0', 'REALM_EDIT'),]
cty_realms <- cty_realms[!duplicated(cty_realms$NAME_0),]
cty_realms[] <- lapply(cty_realms, as.character)

data[is.na(data$REALM_EDIT) & data$GID_0 == 'AUS',]$REALM_EDIT <- 'AA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'BRA',]$REALM_EDIT <- 'NT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'CAN',]$REALM_EDIT <- 'NA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'ALA',]$REALM_EDIT <- 'PA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'ATF',]$REALM_EDIT <- 'AT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'ATG',]$REALM_EDIT <- 'NT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'BHR',]$REALM_EDIT <- 'PA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'BHS',]$REALM_EDIT <- 'NT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'BMU',]$REALM_EDIT <- 'NA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'CPV',]$REALM_EDIT <- 'AT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'CYM',]$REALM_EDIT <- 'NT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'EGY',]$REALM_EDIT <- 'PA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'GGY',]$REALM_EDIT <- 'PA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'GNQ',]$REALM_EDIT <- 'AT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'GUM',]$REALM_EDIT <- 'OC'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'HKG',]$REALM_EDIT <- 'IM'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'IND',]$REALM_EDIT <- 'IM'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'KEN',]$REALM_EDIT <- 'AT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'MAC',]$REALM_EDIT <- 'IM'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'MNE',]$REALM_EDIT <- 'PA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'MUS',]$REALM_EDIT <- 'AT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'MYT',]$REALM_EDIT <- 'AT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'NLD',]$REALM_EDIT <- 'PA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'NRU',]$REALM_EDIT <- 'OC'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'NZL',]$REALM_EDIT <- 'AA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'PHL',]$REALM_EDIT <- 'IM'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'PLW',]$REALM_EDIT <- 'OC'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'PYF',]$REALM_EDIT <- 'OC'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'SLB',]$REALM_EDIT <- 'AA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'SPM',]$REALM_EDIT <- 'NA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'SYC',]$REALM_EDIT <- 'AT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'TCA',]$REALM_EDIT <- 'NT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'TON',]$REALM_EDIT <- 'OC'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'TKL',]$REALM_EDIT <- 'OC'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'TUV',]$REALM_EDIT <- 'OC'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'UMI',]$REALM_EDIT <- 'OC'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'VGB',]$REALM_EDIT <- 'NT'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'VUT',]$REALM_EDIT <- 'AA'
data[is.na(data$REALM_EDIT) & data$GID_0 == 'WLF',]$REALM_EDIT <- 'OC'
table(is.na(data$REALM_EDIT))

# data$NAME_CONCAT_1.2 <- ifelse(is.na(data$NAME_2), data$NAME_1, paste0(data$NAME_1, ", ", data$NAME_2))

shp.pol_boundaries@data <- data
shp.pol_boundaries.cen@data <- data


rgdal::writeOGR(obj=shp.pol_boundaries, dsn="data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo.shp",
                layer='boundaries', driver="ESRI Shapefile", encoding='UTF-8')

rgdal::writeOGR(obj=shp.pol_boundaries.cen, 
                dsn="data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_cen.shp",
                layer='boundaries', driver="ESRI Shapefile", encoding='UTF-8')


#################### For realms (Holt et al 2013) 

filepath <- 'data/geo/0_manual/biogeo/CMEC_regions_&_realms/CMEC regions & realms/realms.shp'
shp.terr_holt <- rgdal::readOGR(filepath, use_iconv=TRUE, encoding = "UTF-8")
filepath2 <- 'data/geo/0_manual/biogeo/CMEC_regions_&_realms/CMEC regions & realms/newRealms.shp'
shp.terr_holt2 <- rgdal::readOGR(filepath2, use_iconv=TRUE, encoding = "UTF-8")
crs(shp.terr_holt) <- CRS("+init=epsg:4326")
shp.terr_holt@data
shp.terr_holt2data <- shp.terr_holt2@data[,c("fullupgmar", "Realm")]
shp.terr_holt@data <- merge(shp.terr_holt@data, shp.terr_holt2data, 
                            by.x="fullupgmar", by.y="fullupgmar")

shp.pol_boundaries.cen <- spatialEco::point.in.poly(shp.pol_boundaries.cen, shp.terr_holt)
data <- merge(shp.pol_boundaries@data,
              shp.pol_boundaries.cen@data[,c('idx', 'Realm')],
              by='idx', all.x=T, all.y=F)

data[] <- lapply(data, as.character)

dim(shp.pol_boundaries@data); dim(data)

cty_realms <- unique(data[,c('NAME_0', 'Realm')])
cty_realms <- cty_realms[!is.na(cty_realms$Realm),]
cty_realms <- cty_realms[order(cty_realms$NAME_0, cty_realms$Realm),]
cty_realms <- cty_realms[!duplicated(cty_realms$NAME_0),]
cty_realms[] <- lapply(cty_realms, as.character)

table(data[is.na(data$Realm),]$GID_0)
unique(data[is.na(data$Realm),]$GID_0)
length(unique(data[is.na(data$Realm),]$GID_0))

data[is.na(data$Realm) & data$GID_0 == 'ALA',]$Realm <- 'Palearctic'
data[is.na(data$Realm) & data$GID_0 == 'ASM',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'ATF',]$Realm <- 'Antarctica'
data[is.na(data$Realm) & data$GID_0 == 'ATG',]$Realm <- 'Neotropical'
data[is.na(data$Realm) & data$GID_0 == 'AUS',]$Realm <- 'Australian'
data[is.na(data$Realm) & data$GID_0 == 'BES',]$Realm <- 'Neotropical'
data[is.na(data$Realm) & data$GID_0 == 'BHS',]$Realm <- 'Panamanian'
data[is.na(data$Realm) & data$GID_0 == 'BMU',]$Realm <- 'Nearctic'
data[is.na(data$Realm) & data$GID_0 == 'BRA',]$Realm <- 'Neotropical'
data[is.na(data$Realm) & data$GID_0 == 'CAN',]$Realm <- 'Nearctic'
data[is.na(data$Realm) & data$GID_0 == 'CHN',]$Realm <- 'Sino-Japanese'
data[is.na(data$Realm) & data$GID_0 == 'CPV',]$Realm <- 'Afrotropical'
data[is.na(data$Realm) & data$GID_0 == 'CYM',]$Realm <- 'Neotropical'
data[is.na(data$Realm) & data$GID_0 == 'ECU',]$Realm <- 'Neotropical'
data[is.na(data$Realm) & data$GID_0 == 'EGY',]$Realm <- 'Saharo-Arabian'
data[is.na(data$Realm) & data$GID_0 == 'FSM',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'GGY',]$Realm <- 'Palearctic'
data[is.na(data$Realm) & data$GID_0 == 'GRL',]$Realm <- 'Palearctic'
data[is.na(data$Realm) & data$GID_0 == 'HKG',]$Realm <- 'Oriental'
data[is.na(data$Realm) & data$GID_0 == 'IND',]$Realm <- 'Oriental'
data[is.na(data$Realm) & data$GID_0 == 'KWT',]$Realm <- 'Saharo-Arabian'
data[is.na(data$Realm) & data$GID_0 == 'MAC',]$Realm <- 'Oriental'
data[is.na(data$Realm) & data$GID_0 == 'MUS',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'MYS',]$Realm <- 'Oriental'
data[is.na(data$Realm) & data$GID_0 == 'NGA',]$Realm <- 'Afrotropical'
data[is.na(data$Realm) & data$GID_0 == 'NLD',]$Realm <- 'Palearctic'
data[is.na(data$Realm) & data$GID_0 == 'NZL',]$Realm <- 'Australian'
data[is.na(data$Realm) & data$GID_0 == 'PER',]$Realm <- 'Neotropical'
data[is.na(data$Realm) & data$GID_0 == 'PLW',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'PYF',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'SHN',]$Realm <- 'Afrotropical'
data[is.na(data$Realm) & data$GID_0 == 'SLB',]$Realm <- 'Australian'
data[is.na(data$Realm) & data$GID_0 == 'SYC',]$Realm <- 'Afrotropical'
data[is.na(data$Realm) & data$GID_0 == 'TCA',]$Realm <- 'Panamanian'
data[is.na(data$Realm) & data$GID_0 == 'TKL',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'TON',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'TUV',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'UMI',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'VGB',]$Realm <- 'Panamanian'
data[is.na(data$Realm) & data$GID_0 == 'VUT',]$Realm <- 'Oceanina'
data[is.na(data$Realm) & data$GID_0 == 'WLF',]$Realm <- 'Oceanina'

# Old edits using "newRealms.shp"

# data[is.na(data$Realm) & data$GID_0 == 'BDI',]$Realm <- 'Afrotropical'
# data[is.na(data$Realm) & data$GID_0 == 'CPV',]$Realm <- 'Afrotropical'
# data[is.na(data$Realm) & data$GID_0 == 'MWI',]$Realm <- 'Afrotropical'
# data[is.na(data$Realm) & data$GID_0 == 'NGA',]$Realm <- 'Afrotropical'
# data[is.na(data$Realm) & data$GID_0 == 'SHN',]$Realm <- 'Afrotropical'
# data[is.na(data$Realm) & data$GID_0 == 'SYC',]$Realm <- 'Afrotropical'
# data[is.na(data$Realm) & data$GID_0 == 'TZA',]$Realm <- 'Afrotropical'
# data[is.na(data$Realm) & data$GID_0 == 'UGA',]$Realm <- 'Afrotropical'
# data[is.na(data$Realm) & data$GID_0 == 'ATF',]$Realm <- 'Antarctica'
# data[is.na(data$Realm) & data$GID_0 == 'NZL',]$Realm <- 'Australian'
# data[is.na(data$Realm) & data$GID_0 == 'SLB',]$Realm <- 'Australian'
# data[is.na(data$Realm) & data$GID_0 == 'CAN',]$Realm <- 'Nearctic'
# data[is.na(data$Realm) & data$GID_0 == 'USA',]$Realm <- 'Nearctic'
# data[is.na(data$Realm) & data$GID_0 == 'ATG',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'BES',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'BRA',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'COL',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'CYM',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'ECU',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'KNA',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'NIC',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'PER',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'PRI',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'VCT',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'VIR',]$Realm <- 'Neotropical'
# data[is.na(data$Realm) & data$GID_0 == 'ASM',]$Realm <- 'Oceanina'
# data[is.na(data$Realm) & data$GID_0 == 'FJI',]$Realm <- 'Oceanina'
# data[is.na(data$Realm) & data$GID_0 == 'PLW',]$Realm <- 'Oceanina'
# data[is.na(data$Realm) & data$GID_0 == 'TKL',]$Realm <- 'Oceanina'
# data[is.na(data$Realm) & data$GID_0 == 'WLF',]$Realm <- 'Oceanina'
# data[is.na(data$Realm) & data$GID_0 == 'HKG',]$Realm <- 'Oriental'
# data[is.na(data$Realm) & data$GID_0 == 'IND',]$Realm <- 'Oriental'
# data[is.na(data$Realm) & data$GID_0 == 'MYS',]$Realm <- 'Oriental'
# data[is.na(data$Realm) & data$GID_0 == 'PHL',]$Realm <- 'Oriental'
# data[is.na(data$Realm) & data$GID_0 == 'ALA',]$Realm <- 'Palearctic'
# data[is.na(data$Realm) & data$GID_0 == 'GRL',]$Realm <- 'Palearctic'
# data[is.na(data$Realm) & data$GID_0 == 'MLT',]$Realm <- 'Palearctic'
# data[is.na(data$Realm) & data$GID_0 == 'NLD',]$Realm <- 'Palearctic'
# data[is.na(data$Realm) & data$GID_0 == 'BHS',]$Realm <- 'Panamanian'
# data[is.na(data$Realm) & data$GID_0 == 'TCA',]$Realm <- 'Panamanian'
# data[is.na(data$Realm) & data$GID_0 == 'VGB',]$Realm <- 'Panamanian'
# data[is.na(data$Realm) & data$GID_0 == 'EGY',]$Realm <- 'Saharo-Arabian'
# data[is.na(data$Realm) & data$GID_0 == 'KWT',]$Realm <- 'Saharo-Arabian'
# data[is.na(data$Realm) & data$GID_0 == 'XAD',]$Realm <- 'Saharo-Arabian'
# data[is.na(data$Realm) & data$GID_0 == 'CHN',]$Realm <- 'Sino-Japanese'

table(data[is.na(data$Realm),]$GID_0)
unique(data[is.na(data$Realm),]$GID_0)
length(unique(data[is.na(data$Realm),]$GID_0))

# data$NAME_CONCAT_1.2 <- ifelse(is.na(data$NAME_2), data$NAME_1, paste0(data$NAME_1, ", ", data$NAME_2))

shp.pol_boundaries@data <- data
shp.pol_boundaries.cen@data <- data

rgdal::writeOGR(obj=shp.pol_boundaries, dsn="data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt.shp",
                layer='boundaries', driver="ESRI Shapefile", encoding='UTF-8')

rgdal::writeOGR(obj=shp.pol_boundaries.cen, 
                dsn="data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_cen_holt.shp",
                layer='boundaries', driver="ESRI Shapefile", encoding='UTF-8')






# joins

filepath <- 'data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp'
poly <- rgdal::readOGR(filepath, use_iconv=TRUE, encoding = "UTF-8")
filepath <- 'tmp/pt.shp'
pt <- rgdal::readOGR(filepath, use_iconv=TRUE, encoding = "UTF-8")

pt.poly <- spatialEco::point.in.poly(pt, poly)


rgdal::writeOGR(obj=pt.poly, 
                dsn="tmp/pt.shp",
                layer='boundaries', driver="ESRI Shapefile", encoding='UTF-8')

write.csv(data.frame(pt.poly), 'tmp/test.csv')
