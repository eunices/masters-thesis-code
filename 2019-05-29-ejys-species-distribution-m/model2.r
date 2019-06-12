# An exercise on species disribution modelling with Megachile sculpturalis
# Trying biomod2 
# https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/biomod2/inst/doc/Simple_species_modelling.pdf?root=biomod
# Incomplete

# 05  


# Libraries
library(biomod2)


# Params
map_crs = '+proj=longlat +datum=WGS84'
raster_dir = 'data/geo/1_separate/chelsa/bioclim/'
country_shp_dir = 'data/geo/1_separate/gadm/shp_pri'
data_in = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-clean.csv'


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

# Get extent
native_extent = raster::extent(min(pres_dd_native$decimalLongitude)-1,
                               max(pres_dd_native$decimalLongitude)+1, 
                               min(pres_dd_native$decimalLatitude)-1,
                               max(pres_dd_native$decimalLatitude)+1)
native_extent_pol = methods::as(native_extent, 'SpatialPolygons')
crs(native_extent_pol) = map_crs

# Get background values
files = list.files(raster_dir, pattern="tif", full.names=TRUE)
predictors = raster::stack(files)
predictors = raster::crop(predictors, native_extent)
names(predictors) = paste0("bio", 1:19)
crs(predictors) = map_crs


x = as.matrix(pres_dd_native[ ,c("decimalLongitude", "decimalLatitude")])
x = data.frame(cbind(x, 1))
name = 'pres'
names(x) = c('x', 'y', name)

data = biomod2::BIOMOD_FormatingData(resp.var = x[,name],
                                     expl.var = predictors,
                                     resp.xy = x[,c('x', 'y')],
                                     resp.name = name)

# To be continued