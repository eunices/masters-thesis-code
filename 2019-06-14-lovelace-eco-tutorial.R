# Following https://geocompr.robinlovelace.net/eco.html

# Load libraries
library(dplyr)
library(mlr)
library(raster)
library(RQGIS)
library(sf)
library(tree)
library(vegan)


# Load data
data("study_area", "random_points", "comm", "dem", "ndvi", package = "RQGIS")


# Data exploration
summary(study_area)
summary(random_points)
summary(comm)
summary(dem)
summary(ndvi)


comm[35:40, 1:5]

dev.off()
plot(dem, col=terrain.colors(200))
plot(study_area, add=T, col=NA)
plot(random_points, add=T, col="black", cex=1, pch=16)
dev.off()

# Compute catchment area and slope
# RQGIS::find_algorithm()
RQGIS::get_usage("saga:sagawetnessindex")

ep = RQGIS::run_qgis(alg = "saga:sagawetnessindex",
                     DEM = dem,
                     AREA_TYPE = 0, 
                     SLOPE_TYPE = 1, 
                     SLOPE = tempfile(fileext = ".sdat"),
                     AREA = tempfile(fileext = ".sdat"),
                     load_output = TRUE,
                     show_output_paths = FALSE)

ep = stack(c(dem, ndvi, ep))
names(ep) = c("dem", "ndvi", "carea", "cslope")

# too right-skewed, log it
ep$carea = log10(ep$carea)

# extract points
random_points[, names(ep)] = raster::extract(ep, random_points)

# presence-absence matrix
pa = vegan::decostand(x=comm, method="pa")  # 100 rows (sites), 69 columns (species)
dim(comm); dim(pa)

# keep only sites in which at least one species was found
pa = pa[rowSums(pa) != 0, ]  # 84 rows, 69 columns

set.seed(214)
nmds = vegan::metaMDS(comm=pa, k=4, try=500)
nmds$stress

# get elevation
elev = dplyr::filter(random_points, id %in% rownames(pa)) %>% 
  dplyr::pull(dem)

# nmds rotated according to elev (main driving variable that is correlated with humidity)
rotnmds = vegan::MDSrotate(nmds, elev)

# extracting the first two axes
sc = vegan::scores(rotnmds, choices = 1:2)


# plot
plot(y = sc[, 1], x = elev, xlab = "elevation in m", 
     ylab = "First NMDS axis", cex.lab = 0.8, cex.axis = 0.8)


# modelling the gradient
rp = data.frame(id = as.numeric(rownames(sc)), sc = sc[, 1])
rp = inner_join(random_points, rp, by = "id")

tree_mo = tree::tree(sc ~ dem, data = rp)
plot(tree_mo)
text(tree_mo, pretty = 0)


# extract the coordinates into a separate data frame
coords = sf::st_coordinates(rp) %>% 
  as.data.frame() %>%
  rename(x = X, y = Y)

# only keep response and predictors which should be used for the modeling
rp = dplyr::select(rp, -id, -spri) %>%
  st_drop_geometry()

# create task
task = mlr::makeRegrTask(data = rp, target = "sc", coordinates = coords)

# learner
lrn_rf = mlr::makeLearner(cl = "regr.ranger", predict.type = "response")

# spatial partitioning
perf_level = mlr::makeResampleDesc("SpCV", iters = 5)

# specifying random search
ctrl = mlr::makeTuneControlRandom(maxit = 50L)

# specifying the search space
ps = ParamHelpers::makeParamSet(
  makeIntegerParam("mtry", lower = 1, upper = ncol(rp) - 1),
  makeNumericParam("sample.fraction", lower = 0.2, upper = 0.9),
  makeIntegerParam("min.node.size", lower = 1, upper = 10)
)

# hyperparamter tuning
set.seed(2014)
tune = mlr::tuneParams(learner = lrn_rf, 
                       task = task,
                       resampling = perf_level,
                       par.set = ps,
                       control = ctrl, 
                       measures = mlr::rmse)

diff(range(rp$sc))

# learning using the best hyperparameter combination
lrn_rf = mlr::makeLearner(cl = "regr.ranger",
                          predict.type = "response",
                          mtry = tune$x$mtry, 
                          sample.fraction = tune$x$sample.fraction,
                          min.node.size = tune$x$min.node.size)


# doing the same more elegantly using setHyperPars()
# lrn_rf = setHyperPars(makeLearner("regr.ranger", predict.type = "response"),
#                       par.vals = tune$x)
# train model
model_rf = mlr::train(lrn_rf, task)
# convert raster stack into a data frame
new_data = as.data.frame(as.matrix(ep))
# apply the model to the data frame
pred_rf = predict(model_rf, newdata = new_data)
# put the predicted values into a raster
pred = dem
# replace altitudinal values by rf-prediction values
pred[] = pred_rf$data$response

plot(pred, col=hcl.colors(300))
plot(study_area, add=T, color=NA)