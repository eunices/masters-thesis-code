# An exercise on species disribution modelling with Megachile sculpturalis
# R packages to use: SDMTools, dismo, biomod, hSDM

# 03 - following tutorial

# Setup
source('keys.R')
setwd(working_dir)


# Load libraries

library(car)
library(data.table)
library(dismo)
library(dplyr)
library(ggmap)
library(ggplot2)
library(maptools)
library(rgdal)
library(sp)

# Initialize variables
data(wrld_simpl)


# Params
map_crs = '+proj=longlat +datum=WGS84'
raster_dir = 'data/geo/1_separate/chelsa/bioclim/'
should_plot = 'no'

# clean data where every point would be used, prior to subsetting
data_in = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-clean.csv' 
data_out_glm = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-glm.csv' 


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

native_range = c("JP", "KR", "CN", "TW")
exotic_range = c("US", "IT", "DE", "CA", "SI", "FR", "KR", "CH", "AD", "MG")
table(df$countryCode %in% native_range)


# For modelling, only using native range
m_coords = df[countryCode %in% native_range] # dataframe
m = df[countryCode %in% native_range]        # spatial dataframe
length(m)


# Convert to geodataframe
coordinates(m) = ~decimalLongitude + decimalLatitude
crs(m) = crs(wrld_simpl)
native_extent = extent(min(m$decimalLongitude)-1,
                       max(m$decimalLongitude)+1, 
                       min(m$decimalLatitude)-1,
                       max(m$decimalLatitude)+1)
native_extent_pol = as(native_extent, 'SpatialPolygons')

# Subsampling presence points for test
pres_raster = raster(m)                               # create raster
res(pres_raster) = 1                                  # set resolution 1 deg
pres_raster = extend(pres_raster, native_extent)      # expand extent by 1 deg (following native_extent)
pres_polygon = rasterToPolygons(pres_raster)          # convert raster to polygon

sam = gridSample(m, pres_raster, n=1, chess="black")  # sample grid for points


# Plotting points to check
native = ggplot2::map_data('world2', c('japan', 'china', 'taiwan', 'korea')) 
polygons = ggplot2::fortify(pres_polygon)
base = ggplot() + geom_polygon(data = native, aes(x=long, y = lat, group = group), fill = NA, color = "grey70") + 
    geom_polygon(data = native_extent_pol, aes(x=long, y = lat, group = group), fill = NA, color = "grey70") 
poly = base + 
    geom_polygon(data = polygons, aes(x=long, y = lat, group = group), fill = NA, color = "grey50") + 
        coord_fixed(1.3) + theme_minimal() 
points = poly +
    geom_point(aes(x = decimalLongitude, y = decimalLatitude, color = as.factor(month)), data = m_coords, alpha = .4, size = 2) +
         labs(x="Longitude (dd)", y="Latitude (dd)", color="Month") 
points
points + geom_point(aes(x=decimalLongitude, y=decimalLatitude), data = as.data.frame(sam), color="black", size=2)
# 23 points from 574 points


# Prepare presence absence information
files = list.files(raster_dir, pattern="tif", full.names=TRUE) # Load background data
rast = raster(files[1], pattern='tif', full.names=TRUE)
rast = crop(rast, native_extent)
# raster_polygon = rasterToPolygons(rast) # too slow
set.seed(1963)
bg_random_points = randomPoints(rast, 500) # get random points
# bg = randomPoints(pred, 500, ext=ee) # get random points


# Plotting this information
base + theme_minimal() + 
    geom_point(aes(x = x, y = y), data = as.data.frame(bg_random_points), color="black", size=2) + # sampled background points
        geom_point(aes(x=decimalLongitude, y=decimalLatitude), data = as.data.frame(m), color="blue", size=2) + # all presence points
             geom_point(aes(x=decimalLongitude, y=decimalLatitude), data = as.data.frame(sam), color="red", size=2)


# Random circles
pres_circles = circles(m, d=50000, lonlat=TRUE) # 50km rad circles from presence points
pres_circles_polygon = polygons(pres_circles) # convert circle to polygon
pres_xy_sample = spsample(pres_circles_polygon, 250, type='random', iter=25) # subsample points from these polygons
pres_cells = cellFromXY(rast, pres_xy_sample)  # get unique raster cells
pres_cells = pres_cells[!is.na(pres_cells)]
print(length(cells)); print(length(unique(pres_cells)))
pres_cells_xy = xyFromCell(rast, pres_cells) # get xy coords from raster cells
print(dim(pres_cells_xy)); print(dim(unique(pres_cells_xy)))

# Overlay between circles and points
pres_cells_xy_sp = SpatialPoints(pres_cells_xy, proj4string=CRS(map_crs))
pres_cells_xy_overlay = over(pres_cells_xy_sp, geometry(pres_circles_polygon))
table(is.na(pres_cells_xy_overlay))
pres_cells_xy_inside = pres_cells_xy[!is.na(pres_cells_xy_overlay), ]
# see separate method on pg 21, vignette for dismo


# Plotting this information
base + theme_minimal() + 
    geom_point(aes(x = x, y = y), data = as.data.frame(bg_random_points), color="black", size=2) +  # sampled background points
        geom_point(aes(x=decimalLongitude, y=decimalLatitude), data = as.data.frame(m_coords), color="blue", size=2) + # all presence points
             geom_point(aes(x=x, y=y), data = as.data.frame(pres_cells_xy_inside), colour = "green", fill = NA, size=2, stroke = 0.1) # sampled presence raster cells (based on bioclim raster)


# Raster preparation
# TODO: add more raster layers like topography/aspect, land cover type, etc.
predictors = stack(files)
predictors = crop(predictors, native_extent)
if(should_plot == 'yes'){
    plot(predictors)
}

# worldmap = rgdal::readOGR(dsn = "data/geo/1_separate/gadm/shp_pri/gadm36_0.shp")
# crs(worldmap) = map_crs
if(should_plot == 'yes'){
    plot(predictors, 1)
    plot(wrld_simpl, add=TRUE)
    plot(native_extent_pol, add=TRUE)
    points(m, col='blue')
    dev.off()
}


# Extracting values from raster
pres_vals = extract(predictors, m)        # get raster values for species occurrence
set.seed(0)
bg_points = randomPoints(predictors, 500)
abs_vals = extract(predictors, bg_points)    # get raster values for background
pb = c(rep(1, nrow(abs_vals)), rep(0, nrow(abs_vals)))
sdm_data = data.frame(cbind(pb, rbind(pres_vals, abs_vals))) # creating dataframe
head(sdm_data)

base + theme_minimal() + 
    geom_point(aes(x = x, y = y), data = as.data.frame(bg_points), color="black", size=1) +  # sampled background points
        geom_point(aes(x=decimalLongitude, y=decimalLatitude), data = as.data.frame(m_coords), color="blue", size=1) # all presence points

write.csv(sdm_data, data_out_glm)


#########################################################################################

# Load libraries
library(car)
library(dismo)
library(data.table)

# Params
raster_dir = 'data/geo/1_separate/chelsa/bioclim/'
data_in_raw = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-clean.csv' 
data_in_glm = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-glm.csv' 
map_crs = '+proj=longlat +datum=WGS84'

# Read data
sdm_data = read.csv(data_in_glm)
sdm_data = sdm_data[,-1] # drop index col
names(sdm_data)[2:length(names(sdm_data))] = paste0("bio", 1:19)


# Predictors
df = fread(data_in_raw, integer64="character")
native_range = c("JP", "KR", "CN", "TW")
m = df[countryCode %in% native_range]        # spatial dataframe
coordinates(m) = ~decimalLongitude + decimalLatitude
crs(m) = map_crs
native_extent = extent(min(m$decimalLongitude)-1,
                       max(m$decimalLongitude)+1, 
                       min(m$decimalLatitude)-1,
                       max(m$decimalLatitude)+1)
files = list.files(raster_dir, pattern="tif", full.names=TRUE) # Load background data
predictors = stack(files)
predictors = crop(predictors, native_extent)
names(predictors) = paste0("bio", 1:19)
crs(predictors) = map_crs

# Quick plots
summary(sdm_data)
pairs(sdm_data[,2:8], cex=0.1, fig=TRUE)   # temperature variables
pairs(sdm_data[,13:20], cex=0.1, fig=TRUE) # precipitation variables


# Modelling with GLM
# Model 1 - fully saturated
model1 = glm(pb ~ ., data=sdm_data)
summary(model1)
car::vif(model1)

# Model 2
# model2 = glm(pb ~ bio1 + bio12, data=sdm_data)
model2 = glm(pb ~ bio5 + bio6 + bio18 + bio19, data=sdm_data)
summary(model2)
car::vif(model2)


# Modelling with bioclim
mod_cols = c('bio5', 'bio6', 'bio18', 'bio19')
bc = bioclim(sdm_data[sdm_data$pb=="1", mod_cols])
pairs(bc)
response(bc)

p = predict(predictors, model2)
plot(p)
points(m)
# text(m, m@data$id, cex=0.65, pos=3,col="red")

# Creating train and test datasets

# Test and train only
train_indices = sample(nrow(sdm_data), round(0.75 * nrow(sdm_data)))
sdm_data_train = sdm_data[train_indices,]
sdm_data_train = sdm_data_train[sdm_data_train$pb==1, mod_cols]
sdm_data_test = sdm_data[-train_indices,]
bc = bioclim(sdm_data_train)
e = evaluate(sdm_data_test[sdm_data_test$pb==1,], sdm_data_test[sdm_data_test$pb==0,], bc); e
plot(e, 'ROC')

# Test and train kfold
pres_data = sdm_data[sdm_data $pb==1, mod_cols]
abs_data = sdm_data[sdm_data $pb==0, mod_cols]

k = 5
group = kfold(pres_data, k)
e = list()
for (i in 1:k) {
    print(paste0("k=", i))
    train = pres_data[group != i,]; print(paste0("Train data ", dim(train)[1]))
    test = pres_data[group == i,]; print(paste0("Test data ", dim(test)[1]))
    bc = bioclim(train)
    e[[i]] = evaluate(p=test, a=abs_data, bc)
}

auc = sapply(e, function(x){slot(x, 'auc')}); mean(auc)
thres = sapply( e, function(x){x@t[which.max(x@TPR + x@TNR)]}); mean(thres)

# Test by removing spatial sorting bias
pres = fread(data_in_raw, integer64="character")
pres = pres[,c("decimalLatitude", "decimalLongitude")]
nr = nrow(pres)
s = sample(nr, 0.25 * nr)
pres_train = pres[-s, ]
pres_test = pres[s, ]

files = list.files(raster_dir, pattern="tif", full.names=TRUE) 
rast = raster(files[1], pattern='tif', full.names=TRUE)
rast = crop(rast, native_extent)
set.seed(1963); bg_random_points = randomPoints(rast, 1200)
nr = nrow(abs)
s = sample(nr, 0.25 * nr)
back_train = abs[-s, ]
back_test = abs[s, ]

sb = ssb(pres_test, back_test, pres_train); sb
sb[,1] / sb[,2] 

i = pwdSample(fixed=pres_test, sample=back_test, reference=pres_train, n=1, tr=0.2)
pres_test_pwd = pres_test[!is.na(i[,1]), ]
back_test_pwd = back_test[na.omit(as.vector(i)), ]
# doesn't work
# sb2 = ssb(pres_test_pwd, back_test_pwd, pres_train)
# sb2[1]/ sb2[2]

########################################################################################