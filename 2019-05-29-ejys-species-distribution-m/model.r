# An exercise on species disribution modelling with Megachile sculpturalis
# R packages to use: SDMTools, dismo, biomod, hSDM

# 04

# Setup
source('keys.R')
setwd(working_dir)


# Libraries
library(maptools)
library(data.table)
library(raster)
library(dismo)
library(ggplot2)
library(randomForest)
library(gbm)


# Params
map_crs = '+proj=longlat +datum=WGS84'
raster_dir = 'data/geo/1_separate/chelsa/bioclim/'
country_shp_dir = 'data/geo/1_separate/gadm/shp_pri'
data_in = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-clean.csv'


# Initialize
data(wrld_simpl) # maptools
modelling_col = paste0('bio',1:19)
latlong_col = c('x', 'y')


# Read data
pres_dd = data.table::fread(data_in, integer64="character")
df_cols = names(pres_dd)
if(any(df_cols == "V1")) {
    pres_dd$V1 = NULL
}


# Subset presence data
native_range = c("JP", "KR", "CN", "TW")
pres_dd_native = pres_dd[countryCode %in% native_range]
pres_dd_native_sp = pres_dd_native


# Get simple map 
native_long = c('japan', 'china', 'taiwan', 'korea')
native = ggplot2::map_data('world2', native_long)
# see https://stackoverflow.com/questions/8751497 on converting map_data to SpatialDF


# Get complex map
native_code = c('JPN', 'CHN', 'TWN', 'KOR', 'MNG', 'PRK', 'RUS')
native_code = paste0(country_shp_dir, "/gadm36_", native_code, "_0.shp")
native_code

shp_master = raster::shapefile(native_code[1])
for (i in 2:length(native_code)) {
    print(paste0("Binding for ", native_code[i], " at index ", i, "."))
    shp_new = raster::shapefile(native_code[i])
    shp_master = raster::bind(shp_master, shp_new)
}

# Get extent
native_extent = raster::extent(min(pres_dd_native$decimalLongitude)-1,
                               max(pres_dd_native$decimalLongitude)+1, 
                               min(pres_dd_native$decimalLatitude)-1,
                               max(pres_dd_native$decimalLatitude)+1)
native_extent_pol = methods::as(native_extent, 'SpatialPolygons')
crs(native_extent_pol) = map_crs


# Crop shp to extent
shp_master_crop = raster::crop(shp_master, native_extent)


# Plot
base = ggplot2::ggplot() + 
    geom_polygon(data = ggplot2::fortify(shp_master_crop), 
                 aes(x=long, y = lat, group = group), 
                 fill = "grey90", color = "grey70") + 
        geom_polygon(data = native_extent_pol, aes(x=long, y = lat, group = group), fill = NA, color = "grey70") 


# Get background values
files = list.files(raster_dir, pattern="tif", full.names=TRUE) # load background data
rast = raster::raster(files[1], pattern='tif', full.names=TRUE)
rast = raster::crop(rast, native_extent)
set.seed(1963); bg_random_points = dismo::randomPoints(rast, nrow(pres_dd_native)) # generate random points


# Making all of them in a dataframe
pres = cbind(1, pres_dd_native$decimalLongitude, pres_dd_native$decimalLatitude, row.names(pres_dd_native))
bg = cbind(0, bg_random_points, NA)
df = data.frame(rbind(pres, bg))
names(df) = c('pres_bg', 'x', 'y', 'id')
sapply(df, class)
df[] = lapply(df, as.character)
df[] = lapply(df, as.numeric)

sp::coordinates(df) = ~ x + y
raster::crs(df) = map_crs


# Getting predictors
files = list.files(raster_dir, pattern="tif", full.names=TRUE)
predictors = raster::stack(files)
predictors = raster::crop(predictors, native_extent)
names(predictors) = paste0("bio", 1:19)
crs(predictors) = map_crs
d = raster::extract(predictors, df)
m = cbind(df, d)
p = as.data.frame(m)


# Test/ train datasets
pres = p[p$pres_bg==1,]
bg = p[p$pres_bg==0,]

set.seed(1000)
pres_gp = kfold(pres, 5)
bg_gp = kfold(bg, 5)

pres_train = pres[pres_gp != 1,]
pres_test = pres[pres_gp == 1,]
bg_train = bg[bg_gp != 1,]
bg_test = bg[bg_gp == 1,]


# Plot
pdf('plots/2019-05-29-ejys-sampling_points.pdf', width=10, height=10)
base + theme_minimal() + 
    geom_point(aes(x = x, y = y), data = as.data.frame(pres_train), color="royalblue4", size=1, shape=3) +
        geom_point(aes(x=x, y=y), data = as.data.frame(pres_test), color="royalblue4", size=1) + 
            geom_point(aes(x=x, y=y), data = as.data.frame(bg_train), color="red3", size=1, shape=3) + 
                geom_point(aes(x=x, y=y), data = as.data.frame(bg_test), color="red3", size=1) +
                    labs(x="Longitude, dd", y="Latitude, dd")
dev.off()


# Bioclim
pred_xy = as.matrix(pres_train[,latlong_col])
mod_cols = c('bio5', 'bio6', 'bio18', 'bio19')
bc = dismo::bioclim(pres_train[, mod_cols]); plot(bc, a=3, b=4, p=0.85)
result = dismo::evaluate(pres_test, bg_test, bc); result
thres = dismo::threshold(result, 'spec_sens')
pb = dismo::predict(predictors, bc, ext=native_extent, progress='')

pdf('plots/2019-05-29-ejys-predict-bioclim.pdf', width=20, height=10)
par(mfrow=c(1,2))
plot(pb, main='Bioclim, raw values')
plot(shp_master_crop, add=TRUE, border='dark grey')
plot(pb > thres, main='presence/absence')
plot(shp_master_crop, add=TRUE, border='dark grey')
points(pres_train[,latlong_col], pch='+')
dev.off()


# Domain
dm = dismo::domain(pres_train[, mod_cols])
result = dismo::evaluate(pres_test, bg_test, dm); result
thres = dismo::threshold(result, 'spec_sens')
pb = dismo::predict(predictors, dm, ext=native_extent, progress='')

pdf('plots/2019-05-29-ejys-predict-domain.pdf', width=20, height=10)
par(mfrow=c(1,2))
plot(pb, main='Domain, raw values')
plot(shp_master_crop, add=TRUE, border='dark grey')
plot(pb > thres, main='presence/absence')
plot(shp_master_crop, add=TRUE, border='dark grey')
points(pres_train[,latlong_col], pch='+')
dev.off()


# Domain
mh = dismo::mahal(pres_train[, mod_cols])
result = dismo::evaluate(pres_test, bg_test, mh); result
thres = dismo::threshold(result, 'spec_sens')
pb = dismo::predict(predictors, mh, ext=native_extent, progress='')

pdf('plots/2019-05-29-ejys-predict-mahal.pdf', width=20, height=10)
par(mfrow=c(1,2))
plot(pb, main='Mahalanobis distance, raw values')
plot(shp_master_crop, add=TRUE, border='dark grey')
plot(pb > thres, main='presence/absence')
plot(shp_master_crop, add=TRUE, border='dark grey')
points(pres_train[,latlong_col], pch='+')
dev.off()


# GLM
train = rbind(pres_train, bg_train)
test = rbind(pres_test, bg_test)
mod1 = glm(pres_bg ~ bio5 + bio6 + bio18 + bio19, family = binomial(link = "logit"), data=train)
summary(mod)
mod2 = glm(pres_bg ~ bio5 + bio6 + bio19, family = binomial(link = "logit"), data=train)
summary(mod2)
mod3 = glm(pres_bg ~ bio6 + bio19, family = binomial(link = "logit"), data=train)
summary(mod3)
result = dismo::evaluate(pres_test, bg_test, mod3)
thres = dismo::threshold(result, 'spec_sens')
pb = dismo:: predict(predictors, mod3, ext=native_extent, progress='')

pdf('plots/2019-05-29-ejys-predict-glm.pdf', width=20, height=10)
par(mfrow=c(1,2))
plot(pb, main='Mahalanobis distance, raw values')
plot(shp_master_crop, add=TRUE, border='dark grey')
plot(pb > thres, main='presence/absence')
plot(shp_master_crop, add=TRUE, border='dark grey')
points(pres_train[,latlong_col], pch='+')
dev.off()


xm = dismo::maxent(predictors, pred_xy)
plot(xm)
response(xm)

e = evaluate(as.matrix(pres_test[,latlong_col]), as.matrix(bg_test[,latlong_col]), xm, predictors)
px = predict(predictors, xm, ext=native_extent, progress='')
par(mfrow=c(1,2))
plot(px, main='Maxent, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr = threshold(e, 'spec_sens')
plot(px > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')


# Convex hulls
hull <- dismo::convHull(pres_train, lonlat=TRUE)
e <- evaluate(hull, p=pres_test, a=backg_test)



# Did not do for random forest and SVM.
# Did not try model combination methods.
# Did not try geoIDW, voronoiHull, indicator kriging (gstat)


