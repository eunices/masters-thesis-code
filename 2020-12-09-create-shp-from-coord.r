library(sp)
library(raster)
library(rgdal)

boxes <- list(
    matrix(c(-180, -90, 180, -66.56325), nrow=2),
    matrix(c(-180, -66.56325, 180, -56.04217), nrow=2),
    matrix(c(-180, -56.04217, 180, -45.52108), nrow=2),
    matrix(c(-180, -45.52108, 180, -35 ), nrow=2),
    matrix(c(-180, -35, 180, -23.43674), nrow=2),
    matrix(c(-180, -23.43674, 180, 0), nrow=2),

    matrix(c(-180, 0, 180, 23.43674), nrow=2),
    matrix(c(-180, 23.43674, 180, 35), nrow=2),
    matrix(c(-180, 35, 180, 45.52108), nrow=2),
    matrix(c(-180, 45.52108, 180, 56.04217), nrow=2),
    matrix(c(-180, 56.04217, 180, 66.56325), nrow=2),
    matrix(c(-180, 66.56325, 180, 90), nrow=2)
)

boxes <- lapply(boxes, function(x) {
    x <- as(raster::extent(x), "SpatialPolygons")
    proj4string(x) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    x
})

boxes <- raster::bind(boxes)


names <- c("Tropics", "Subtropics", "Temperate 1", "Temperate 2", "Temperate 3", "Polar")
unique <- c(
    paste0("S ", rev(names)),
    paste0("N ", names)
)
boxes <- SpatialPolygonsDataFrame(
    boxes, 
    data.frame(
        id=1:length(boxes), 
        name = unique,
        type = c(rev(names), names),
        NS = c(rep("S", length(names)), rep("N", length(names))))
)

file <- "2020-12-09-latitude-boxes.gpkg"
writeOGR(boxes, layer = 'poly', dsn = file, driver="GPKG")