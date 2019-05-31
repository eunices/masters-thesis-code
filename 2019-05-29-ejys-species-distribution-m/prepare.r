# An exercise on species disribution modelling with Megachile sculpturalis
# R packages to use: SDMTools, dismo, biomod, hSDM

setwd('C:/Dev/msc-thesis-code')

# Load libraries
library(dismo)
library(data.table)
library(maptools)

# Read dataset
df = fread('data/2019-05-27-gbif-data/0018967-190415153152247.csv')

# Choose preferred columns
necessary_columns = c(
    'decimalLatitude',
    'decimalLongitude',
)

table(df[, 'hasCoordinate'])
m = df[hasCoordinate == "TRUE", ..necessary_columns]
head(m)

# Georeference those without coordinates
# TODO

summary(m)

# Plotting
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-100, 150), ylim=c(-50, 50), axes=TRUE, col="light yellow")
box()
points(m$decimalLongitude, m$decimalLatitude, col='orange', pch=20, cex=0.75)
points(m$decimalLongitude, m$decimalLatitude, col='red', cex=0.75)