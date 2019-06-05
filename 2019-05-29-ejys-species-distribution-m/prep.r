# An exercise on species disribution modelling with Megachile sculpturalis
# R packages to use: SDMTools, dismo, biomod, hSDM

# 02

# Setup
source('keys.R')
setwd(working_dir)


# Load libraries
library(data.table)
library(dismo)
library(maptools)
library(rgdal)
library(sp)
library(ggmap)
library(dplyr)

# Initialize variables
data(wrld_simpl)


# Params
map_crs = '+proj=longlat +datum=WGS84'
raster_dir = 'data/geo/1_separate/chelsa/bioclim/'
should_plot = 'no'

# clean data where every point would be used, prior to subsetting
data_in = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-clean.csv' 

# dataset to be used for training
data_out_train = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-model-train.csv' 

# dataset to be used for testing
data_out_test = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-model-test.csv' 


# Read dataset
df = fread(data_in, integer64="character")
df_cols = names(df)
if(any(df_cols == "V1")) {
    df$V1 = NULL
}

# Quick checks
necessary_cols = c(
    'decimalLatitude',
    'decimalLongitude',
    'scientificName',
    'year',
    'month',
    'day', 
    'countryCode',
    'institutionCodeShort'
)

# Check dataset length
dim(df)

# Check which have coords
df[,.(.N), by=.(hasCoordinate)] # retaining the original information prior to geocoding
df[is.na(decimalLatitude),] # but all are geocoded

# Check institution codes
df[,.(.N), by=.(institutionCodeShort, basisOfRecord)][order(basisOfRecord),]
df[basisOfRecord == "HUMAN_OBSERVATION",.(.N), by=.(institutionCode, basisOfRecord)][order(basisOfRecord),]
df[institutionCode == "", ..necessary_cols]

# Check issues
# see https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
df[,.(.N), by=.(issue)]
# issues don't seem very serious

# Check sex
df[,.(.N), by=.(sex)]
# many specimens do not have sex

# Check month
df[,.(.N), by=.(month)][order(month)]
# mainly spotted from Jun - Aug
df_map = df[month %in% c("2", "4","5","11", "12"), ..necessary_cols]
# df_map = df[month %in% c("11"), ..necessary_cols]
df_map
mp = ggplot() + borders("world", colour="gray70", fill="gray80", ylim=c(20, 50)) 
mapPoints = mp + 
    geom_point(
        aes(x = decimalLongitude, y = decimalLatitude, colour=as.factor(month)), 
        data = df,
        alpha = .8,
        ) + theme_minimal()
mapPoints + labs(color='Month', x='Longtitude', y='Latitude')
dev.off()

# April records (max temp 20-23 deg C)
df[month=="4"]
# Feb record (max temp -4 to 2 deg C)
df[month=="2"] # seems like outlier
# Nov records (max temp 24 deg C)
df[month=="11"]
# Dec records (max temp 24 deg C)
df[month=="12"]

df[is.na(month), verbatimEventDate]
df[is.na(month), ..necessary_cols] # may not be useful if there is no date

# Check year
df[is.na(year), ..necessary_cols]
df2 = df
df2$decade = paste0(substr(df2$year, 0, 3), "0s")
df2[decade=="NA0s"]$decade = "No year"

df2$addValue = "No georef"
df2[issue=="GEOREFERENCED_GOOGLE_API_WITH_HIGHER_GEOG"]$addValue = "Geo ref (higher geo)"
df2[issue=="GEOREFERENCED_GOOGLE_API"]$addValue = "Geo ref"

df2$addValue = factor(df2$addValue, levels=c("No georef", "Geo ref", "Geo ref (higher geo)"))

tib = df2 %>%
  group_by(decade, addValue) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
ggplot(data=tib, aes(x=decade, y=freq, fill=addValue), fill="black") +
  geom_bar(stat="identity") + theme_minimal() + labs(x="Decade", y="Number of specimens", fill="Georeference status")

year = df2[,.(.N), by=.(decade, addValue)][order(decade)]
year
ggplot(data=year, aes(x=decade, y=N, fill=addValue), fill="black") +
  geom_bar(stat="identity") + theme_minimal() + labs(x="Decade", y="Number of specimens", fill="Georeference status")
dev.off(); year = NULL; df2 = NULL
year = df2[,.(.N), by=.(decade)][order(decade)]
year

# Check country codes
df[,.(.N), by=.(countryCode)]
df[,.(.N), by=.(hasCoordinate, countryCode)][order(countryCode, hasCoordinate)]
# Added records from China, from Discover Life

# For modelling
m = df

# Convert to geodataframe
coordinates(m) = ~decimalLongitude + decimalLatitude
crs(m) = crs(wrld_simpl)

# Spatial checks
sp_join = over(m, wrld_simpl)
m@data$countryJoin = sp_join$FIPS
m@data$countryJoin2 = sp_join$NAME
country_do_not_match = m@data[m@data$countryCode!=m@data$countryJoin,]
dim(country_do_not_match)
summary = as.data.frame(unique(country_do_not_match[,c("countryCode", "countryJoin", "countryJoin2")]))
summary[order(summary$countryCode),]
cols = c(necessary_cols[-1:-3], "countryJoin")
# All is good based on country mapping

# Subsampling

r = raster(m)                # create raster
res(r) = 1                   # set resolution 1 deg
r = extend(r, extent(r)+1)   # expand extent by 1 deg
sam = gridSample(m, r, n=1)  # sample grid
p = rasterToPolygons(r)      # convert raster to polygon

if(should_plot == "yes") {
    plot(p, border='grey')
    points(m)
    points(sam, cex=1, col='red', pch='x')
    dev.off()
}


# Prepare presence absence information

# Load background data
files = list.files(raster_dir, pattern="tif", full.names=TRUE)
pred = raster(files[1], pattern='tif', full.names=TRUE)
set.seed(1963)
# Get random points
bg = randomPoints(pred, 500)
if(should_plot == 'yes'){
    par(mfrow=c(1,2))
    plot(!is.na(pred), legend=FALSE)
    points(bg, cex=0.5)
}

# Create new extent and generate randomly sampled points based on it
ee = extent(-80, -53, -39, -22)
bg2 = randomPoints(pred, 50, ext=ee)
if(should_plot == 'yes'){
    plot(!is.na(pred), legend=FALSE)
    plot(ee, add=TRUE, col='red')
    points(bg2, cex=0.5)
}

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

if(should_plot == "yes"){
    plot(pol, axes=TRUE)
    points(xy, cex=0.75, pch=20, col='blue')
    dev.off()
}

# get overlay between circles and points
spxy = SpatialPoints(xy, proj4string=CRS(map_crs))
o = over(spxy, geometry(x))
table(is.na(o))
xyInside = xy[!is.na(o), ]
# see separate method on pg 21, vignette for dismo


# Raster preparation

predictors = stack(files)
if(should_plot == 'yes'){
    plot(predictors)
}

worldmap = rgdal::readOGR(dsn = "data/geo/1_separate/gadm/shp_pri/gadm36_0.shp")
crs(worldmap) = map_crs
if(should_plot == 'yes'){
    plot(predictors, 1)
    plot(worldmap, add=TRUE)
    points(m, col='blue')
    dev.off()
}


# Extracting values from raster
pres_vals = extract(predictors, m)        # get raster values for species occurrence
set.seed(0)
backgr = randomPoints(predictors, 500)
abs_vals = extract(predictors, backgr)    # get raster values for background
pb = c(rep(1, nrow(abs_vals)), rep(0, nrow(abs_vals)))
sdm_data = data.frame(cbind(pb, rbind(pres_vals, abs_vals))) # creating dataframe
head(sdm_data)
summary(sdm_data)
pairs(sdm_data[,2:5], cex=0.1, fig=TRUE)
