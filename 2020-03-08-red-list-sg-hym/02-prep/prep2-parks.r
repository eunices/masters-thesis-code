# Assign habitat to park
# based on proximity to nature reserves (periphery)
# and proportion of tree cover (>60% tree cover == non-urban park)

source('2020-03-08-red-list-sg-hym/init.r')

# Libraries
library(sf)
library(data.table)

library(rgdal)
library(raster)

library(rgeos)

# Files
# UNIQUE_ is a unique identifier used to join all datasets here! it's not exactly a running number...
parks = paste0(folder_new_parks, "parks-all-non-islands-edit.gpkg") # any manual edits should go here
parks2a = paste0(folder_new_parks, "parks-all-non-islands-green.gpkg") # automated to calculate area and greenery
parks2b = paste0(folder_new_parks, "parks-all-non-islands-green-large.gpkg") # automated to calculate area and greenery
parks3a = paste0(folder_new_parks, "parks-all-final.gpkg") # automated to calculate area and greenery

natres = paste0(folder_final, "parks-nature-reserves.gpkg")
raster = paste0(folder_rast, "Figure1_svy21.tif")

# Read files
p = st_read(parks)
p = st_set_crs(p, svy21)
nr = st_read(natres)
r = raster(raster)
# r = projectRaster(r, crs=svy21_proj4) # takes too long

##################################################################################################
##################################################################################################
##################################################################################################
# Part 1 - get parks that are near to nature reserve

# nr_100buf = st_buffer(nr, 200)
# parks_near_res = st_intersects(p, nr_100buf, sparse=F)
# table(parks_near_res[,1])
# p[parks_near_res[,1],]
# plot(nr_100buf[,1], add=T)
# plot(nr[,1], add=T)
# plot(p[,1])
# plot(nr[,1])
# extent(nr)
# extent(p)
# couldn't get files to align, did it on QGIS

##################################################################################################
##################################################################################################
##################################################################################################
# Part 2 - intersect and calculate prop of vegetation

##################################################################################################
# Clip raster to each individual polygon, calculate % of land cover, append to data frame

unique_ids = p$UNIQUE_
table(duplicated(unique_ids))
data = fread(paste0(folder_geo, "parks-classify-combined.csv"))
ids = as.integer(unique(data$UNIQUE_))
missing_ids = unique_ids[!(unique_ids %in% ids)]
p[is.na(p$UNIQUE_),]

extract_values = function(pol, r, timeout=T) {
  if (timeout) setTimeLimit(elapse=60*10, trans=T)
  cropped = crop(r, pol, byid=TRUE)
  ext = extract(cropped, pol, method='simple')
  df = data.table(UNIQUE_=pol$UNIQUE_, TYPE=unlist(ext))[, .N, by=c("UNIQUE_", "TYPE")]
  return(df)
}

df_all = data.table(UNIQUE_=as.character(), TYPE=as.character(), N=as.integer())
beginCluster()
for (i in missing_ids) {
  print(paste0(Sys.time(), " -- Polygon ", i, " -- ", paste0(p[p$UNIQUE_==as.character(i),]$NAME)))
  pol = p[p$UNIQUE_==as.character(i),]
  df = extract_values(pol, r, timeout=F)
  df_all = rbind(df_all, df)
}
endCluster()

# Timeout version
# err = c()
# beginCluster()
# for (i in 386:dim(p)[1]) {
#   print(paste0(Sys.time(), " -- Polygon ", i, " -- ", paste0(p[p$UNIQUE_==as.character(i),]$NAME)))
#   tryCatch({
#     pol = p[p$UNIQUE_==as.character(i),]
#     df = extract_values(pol, r)
#     df_all = rbind(df_all, df)

#   }, error=function(cond) {
#     print(paste0("Time out for polygon ", i))
#     message(cond)
#     err = c(err, i)
#   })
# }
# endCluster()

df_write = rbind(fread(paste0(folder_geo, "parks-classify-combined.csv")), df_all, fill=T)
write.csv(df_write, paste0(folder_geo, "parks-classify-combined.csv"), row.names=F)

##################################################################################################
# Tabulate and merge back
data = fread(paste0(folder_geo, "parks-classify-combined.csv"))

# idxes[!(idxes %in% as.numeric(unique(data$ID)))] # check
sum = dcast(data, UNIQUE_ ~ TYPE, fun.aggregate=mean, value.var="N")
sum[is.na(sum)] = 0

# vegetation with canopy [6,13], vegetation without canopy [11,16]
# freshwater swamp [16], mangrove [31]
# bare ground [4], buildings [1], impervious [2]
# water bodies [22], water courses [20], freshwater marsh [19]

terrestrial_cols = names(sum)[which(!(names(sum) %in% c("22", "20", "19", "UNIQUE_", "total")))]
sum$total_terrestrial = rowSums(sum[, ..terrestrial_cols], na.rm=T)
sum$total = rowSums(sum[, 2:dim(sum)[2]], na.rm=T)
sum$veg_canopy = sum$`6` + sum$`13`
sum$veg_no_canopy = sum$`11` + sum$`16`
sum$veg_canopy_unmanaged = sum$`6`

sum$prop_veg_canopy_unmanaged = sum$`6` / sum$total_terrestrial * 100
sum$prop_veg_canopy = sum$veg_canopy / sum$total_terrestrial * 100
sum$prop_veg_no_canopy = sum$veg_no_canopy / sum$total_terrestrial * 100
sum$prop_veg = sum$prop_veg_canopy + sum$prop_veg_no_canopy

sum$prop_veg_no_canopy_of_veg = sum$veg_no_canopy / (sum$veg_no_canopy + sum$veg_canopy) * 100
sum$prop_veg_unmanaged_canopy_of_veg = sum$`6` / (sum$veg_no_canopy + sum$veg_canopy) * 100
sum$mangrove = sum$`31`

# Files
p = st_read(parks)

# calculate area
p$AREA_SQ_M = st_area(p)

cols = c("UNIQUE_", "prop_veg_canopy_unmanaged", "prop_veg_canopy", "prop_veg_no_canopy", "prop_veg",
         "prop_veg_no_canopy_of_veg", "mangrove", "veg_canopy_unmanaged")#, "total")

p2 = merge(p, sum[, ..cols], by="UNIQUE_", all.x=T, all.y=F)
# hist(as.numeric(p2$AREA_SQ_M) - (p2$total*5)) # sanity check for area

# > 40% vegetation cover
# size > 1ha
st_write(p2, parks2a, delete_dsn=T)

##################################################################################################
# Assign habitat type
# this script may go later into the package itself

p = st_read(parks2a)
p$habitat = as.character(p$TYPE)
unique(p$habitat)

# Mature secondary/ primary/ near
# already added manually as "FRINGING FOREST"

SIZE_THRESHOLD = 10000
UNMANAGED_TREES_THRESHOLD = 10000/5 # 1 ha / pixel size
VEGETATION_THRESHOLD = 80

isLarge = p$AREA_SQ_M > SIZE_THRESHOLD
isMangrove = p$mangrove>1
isForestCover = p$veg_canopy_unmanaged >= UNMANAGED_TREES_THRESHOLD &
  p$prop_veg > VEGETATION_THRESHOLD

# Mangrove
p[isLarge & isMangrove,]$habitat = "MANGROVE"

# Urban/semi-urban
p[isLarge & !isMangrove & !isForestCover,]$habitat = "URBAN/SEMI-URBAN"

# Young secondary
p[isLarge & !isMangrove & isForestCover,]$habitat = "YOUNG SECONDARY FOREST"

# Small parks
p[!isLarge,]$habitat = "SMALL GREEN SPACE"

table(isLarge)
table(p$habitat)

p[isLarge & !isMangrove & isForestCover,]$NAME

st_write(p, paste0(folder_final, "parks-all.gpkg"), delete_dsn=T)



large_p = p[isLarge,]
st_write(large_p, parks2b, delete_dsn=T)
large_pdf = large_p
st_geometry(large_pdf) = NULL
large_pdf = data.table(large_pdf)

isMangrove = large_pdf$mangrove>1
isForestCover = large_pdf$veg_canopy_unmanaged >= UNMANAGED_TREES_THRESHOLD &
  large_pdf$prop_veg > VEGETATION_THRESHOLD

large_pdf[isMangrove, c("NAME", "mangrove")]
large_pdf[!isMangrove & !isForestCover, c("NAME", "prop_veg")]
t = large_pdf[!isMangrove & isForestCover,
              c("NAME", "veg_canopy_unmanaged", "AREA_SQ_M")][order(-veg_canopy_unmanaged)]
dim(t)
write.csv(t, paste0(folder_geo, "parks-vet.csv"), row.names=F)

# note: east coast keeps popping up (prop of unmanaged of veg, raw unmanaged, prop of canopy of veg)

# TODO: try training this random forest by labelling 80% of the parks
# TODO: learn gdal
# TODO: polygonize stuff
# steps: covert to raster, polygonize, remove small bits, delete holes [try to buffer/expand and merge polygons later]
# remove areas that are parks? [may not be necessary]