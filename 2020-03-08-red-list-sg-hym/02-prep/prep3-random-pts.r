
source('2020-03-08-red-list-sg-hym/init.r')

set.seed(2020)

# Libraries
library(sf)
library(data.table)

# TODO: a script to generate random points in different areas

# Files
file_rand_natres_common = "random-pts-within-nat-res-common.gpkg"
file_rand_natres_rare = "random-pts-within-nat-res-rare.gpkg"
file_rand_parks_common = "random-pts-within-parks-common.gpkg"
file_rand_parks_rare = "random-pts-within-parks-rare.gpkg"
file_rand_islands_rare = "random-pts-islands-rare.gpkg"
file_rand_islands_common = "random-pts-islands-common.gpkg"

filepath_rand_natres_common = paste0(folder_rand, file_rand_natres_common)
filepath_rand_natres_rare = paste0(folder_rand, file_rand_natres_rare)
filepath_rand_parks_common = paste0(folder_rand, file_rand_parks_common)
filepath_rand_parks_rare = paste0(folder_rand, file_rand_parks_rare)
filepath_rand_islands_rare = paste0(folder_rand, file_rand_islands_rare)
filepath_rand_islands_common = paste0(folder_rand, file_rand_islands_common)

# Read files
v_rand_natres_common = st_read(filepath_rand_natres_common)
v_rand_natres_rare = st_read(filepath_rand_natres_rare)
v_rand_parks_common = st_read(filepath_rand_parks_common)
v_rand_parks_rare = st_read(filepath_rand_parks_rare)
v_rand_islands_rare = st_read(filepath_rand_islands_rare)
v_rand_islands_common = st_read(filepath_rand_islands_common)

li_names = c(file_rand_natres_common, 
             file_rand_natres_rare,
             file_rand_parks_common,
             file_rand_parks_rare,
             file_rand_islands_rare,
             file_rand_islands_common)

li = list(v_rand_natres_common,
          v_rand_natres_rare,
          v_rand_parks_common,
          v_rand_parks_rare,
          v_rand_islands_rare,
          v_rand_islands_common)

li_df = list()
for (i in 1:length(li)) {
  vec = li[[i]]
  vec = st_transform(vec, wgs84)
  coords = st_coordinates(vec)
  li_df[[i]] = coords
  write.csv(coords, paste0(folder_rand_test, gsub("\\.gpkg", "\\.csv", li_names[i])), row.names=F)
}

li_df[[5]]
dat = data.frame(species=as.character(), X=as.numeric(), Y=as.numeric())
for (i in 1:length(li)) {
  df = data.frame(species=paste0("sp ", i), li_df[[i]])
  if (i==5) print(df)
  dat = rbind(dat, df)
}


dat = data.table(dat)
date_lower = as.Date("1758-01-01")
date_cut_off = as.Date("1959-12-31")
date_upper = as.Date("2020-01-01")

dat$collection_date = date_upper

file_rand_natres_common = "random-pts-within-nat-res-common.gpkg"
file_rand_natres_rare = "random-pts-within-nat-res-rare.gpkg"
file_rand_parks_common = "random-pts-within-parks-common.gpkg"
file_rand_parks_rare = "random-pts-within-parks-rare.gpkg"
file_rand_islands_rare = "random-pts-islands-rare.gpkg"
file_rand_islands_common = "random-pts-islands-common.gpkg"
dat[species == "sp 1",]$collection_date = sample(seq(date_lower, date_upper, by="day"), 
                                                 dim(dat[species == "sp 1",])[1], replace=T) # should be "Endangered"
dat[species == "sp 2",]$collection_date = sample(seq(date_lower, date_cut_off, by="day"), 
                                                 dim(dat[species == "sp 2",])[1], replace=T) # should be "Manual - either Data Deficient or Nationally Extinct "
dat[species == "sp 3",]$collection_date = sample(seq(date_lower, date_upper, by="day"), 
                                                 dim(dat[species == "sp 3",])[1], replace=T) # should be "Least Concern"
dat[species == "sp 4",]$collection_date = sample(seq(date_lower, date_upper, by="day"), 
                                                 dim(dat[species == "sp 4",])[1], replace=T) # should be "Near Threatened"
dat[species == "sp 5",]$collection_date = sample(seq(date_lower, date_upper, by="day"), 
                                                 dim(dat[species == "sp 5",])[1], replace=T) # should be "Data deficient"
dat[species == "sp 6",]$collection_date = sample(seq(date_lower, date_upper, by="day"), 
                                                 dim(dat[species == "sp 6",])[1], replace=T) # should be "Vulnerable"


dat$type = ""
dat$type = sample(c("worker", "reproductive"), dim(dat)[1], c(0.99, 0.01), replace=T)

write.csv(dat, paste0(folder_rand_test, "test-points.csv"), row.names=F)