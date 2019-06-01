# An exercise on species disribution modelling with Megachile sculpturalis
# R packages to use: SDMTools, dismo, biomod, hSDM

# Setup
setwd('C:/Dev/msc-thesis-code')
# setwd('mnt/c/Dev/msc-thesis-code')


# Load libraries
library(data.table)
library(dismo)
library(maptools)
library(rgdal)
library(sp)
library(ggmap)


# Params
geocode_api = 'AIzaSyAkLYuBhwxR7glHNVuR1Gsszt4BmJ8ScY4'
map_crs = '+proj=longlat +datum=WGS84'
raster_dir = 'data/geo/chelsa/bioclim/'
should_plot = 'no'


# Initialize google api for geocoding
register_google(key = geocode_api)


# Read dataset
df = fread('data/2019-05-27-gbif-data/0018967-190415153152247.csv') # obtained from GBIF
df_cols = names(df)


# Quick cleans
df$institutionCodeShort = gsub(".*\\((.*)\\).*", "\\1", df[,institutionCode])


# Quick checks
dim(df)

df[,.(.N), by=.(hasCoordinate)]
df[,.(.N), by=.(institutionCodeShort, basisOfRecord)][order(basisOfRecord),]
df[,.(.N), by=.(countryCode)]
df[,.(.N), by=.(hasCoordinate, countryCode)][order(countryCode, hasCoordinate)]
# georeferencing may need to be done for Japan, China


# Georeference those without coordinates
georef_cols = c(
    'locality',
    'verbatimLocality'
)
# followed for setting up setting api keys https://stackoverflow.com/questions/52565472/
filter = is.na(df$decimalLatitude) | is.na(df$decimalLongitude)
table(filter)

# Separate tables
df1 = df[!filter]   # no need geocoding
df2 = df[filter]    # to be geocoded
# Remove those with no locality
df2 = df2[!(verbatimLocality == "" & locality == ""),]; print(dim(df2)[1]) 
# Create columns with most information for geocoding
df2$localityCombined = df2$locality
df2[locality=="",]$localityCombined = df2[locality=="", verbatimLocality]
df2$localityMoreDetails = paste0(gsub("\\|", ", ", df2$higherGeography), ", ", df2$localityCombined)
df2$localityMoreDetails = gsub("^, ", "", df2$localityMoreDetails)
# Checks
df2[locality=="", localityMoreDetails]
df2[verbatimLocality=="", localityMoreDetails]
dim(df2[verbatimLocality=="" & locality=="",])
dim(df2[verbatimLocality=="" & locality=="",])
# Geocoding
geocoded = try(geocode(df2$localityMoreDetails)) 
geocoded = as.data.frame(geocoded)
df2$decimalLatitude = geocoded$lat
df2$decimalLongitude = geocoded$lon
# Cleanup 
filter = is.na(df2$decimalLatitude) | is.na(df2$decimalLongitude)
table(filter)
df2 = df2[!filter]
df2$hasGeospatialIssues = TRUE
df2$issue = "GEOREFERENCED W/ GOOGLE API"
df2$locality = df$localityMoreDetails
df2$localityCombined = NULL
df2$localityMoreDetails = NULL
# Recombine both dataframes
df = rbind(df1, df2)
# Last checks
df[hasCoordinate==FALSE, decimalLatitude]
df[hasCoordinate==FALSE, decimalLongitude]

# Remove duplicates
# TODO:
# 1: may be based on exact time/date/locality
# 2: may have same catalogNumber


# Choose preferred columns
necessary_cols = c(
    'decimalLatitude',
    'decimalLongitude',
    'species',
    'year',
    'month',
    'day', 
    'countryCode',
    'institutionCodeShort'
)
modelling_cols = necessary_cols[1:2]
df_subset = df[, ..necessary_cols]

m = df[hasCoordinate == "TRUE", ]
head(m)
names(m)


# Quick plot
data(wrld_simpl)
if should_plot == 'yes':
    plot(wrld_simpl, xlim=c(-100, 150), ylim=c(25, 50), axes=TRUE, col="light yellow")
    box()
    points(m$decimalLongitude, m$decimalLatitude, col='orange', pch=20, cex=0.75)
    points(m$decimalLongitude, m$decimalLatitude, col='red', cex=0.75)
    dev.off()


# Data cleaning

# Check if countryCode and lat/long match up

coordinates(m) = ~decimalLongitude + decimalLatitude
crs(m) = crs(wrld_simpl)
sp_join = over(m, wrld_simpl)

m@data$countryJoin = sp_join$FIPS
country_do_not_match = m@data[m@data$countryCode!=m@data$countryJoin, c(necessary_cols[-1:-3], "countryJoin")]
dim(country_do_not_match)
# mainly JA to JP, KS and KA, GM and DE mapping; US and CA
# looks good overall

# Subsampling

r = raster(m)                # create raster
res(r) = 1                   # set resolution 1 deg
r = extend(r, extent(r)+1)   # expand extent by 1 deg
sam = gridSample(m, r, n=1)  # sample grid
p = rasterToPolygons(r)      # convert raster to polygon

if should_plot == "yes":
    plot(p, border='grey')
    points(m)
    points(sam, cex=1, col='red', pch='x')
    dev.off()


# Prepare presence absence information

# Load background data
files = list.files(raster_dir, pattern="tif", full.names=TRUE)
pred = raster(files[1], pattern='tif', full.names=TRUE)
set.seed(1963)
# Get random points
bg = randomPoints(pred, 500)
if should_plot == 'yes':
    par(mfrow=c(1,2))
    plot(!is.na(pred), legend=FALSE)
    points(bg, cex=0.5)
# Create new extent and generate randomly sampled points based on it
ee = extent(-80, -53, -39, -22)
bg2 = randomPoints(pred, 50, ext=ee)
if should_plot == 'yes':
    plot(!is.na(pred), legend=FALSE)
    plot(ee, add=TRUE, col='red')
    points(bg2, cex=0.5)

# Random circles
# make circles
x = circles(m, d=50000, lonlat=TRUE) # 50km rad circles from presence points
pol = polygons(x)
print(pol)
# subsample points from these polygons [how?]
samp1 = spsample(pol, 250, type='random', iter=25)
# get unique raster cells
cells = cellFromXY(pred, samp1)
print(length(cells)); print(length(unique(cells)))
# get xy coords from raster cells
xy = xyFromCell(pred, cells)
print(dim(xy))

if should_plot == "yes":
    plot(pol, axes=TRUE)
    points(xy, cex=0.75, pch=20, col='blue')
    dev.off()

# get overlay between circles and points
spxy = SpatialPoints(xy, proj4string=CRS(map_crs))
o = over(spxy, geometry(x))
table(is.na(o))
xyInside = xy[!is.na(o), ]
# see separate method on pg 21, vignette for dismo


# Raster preparation

predictors = stack(files)
if should_plot == 'yes':
    plot(predictors)

worldmap = rgdal::readOGR(dsn = "data/geo/gadm/shp_pri/gadm36_0.shp")
crs(worldmap) = map_crs
if should_plot == 'yes':
    plot(predictors, 1)
    plot(worldmap, add=TRUE)
    points(m, col='blue')
    dev.off()


# Extracting values from raster
pres_vals = extract(predictors, m)        # get raster values for species occurrence
set.seed(0)
backgr = randomPoints(predictors, 500)
abs_vals = extract(predictors, backgr)    # get raster values for background
pb = c(rep(1, nrow(abs_vals)), rep(0, nrow(absvals)))
sdm_data = data.frame(cbind(pb, rbind(presvals, absvals))) # creating dataframe
head(sdm_data)
summary(sdm_data)

pairs(sdm_data[,2:5], cex=0.1, fig=TRUE)
