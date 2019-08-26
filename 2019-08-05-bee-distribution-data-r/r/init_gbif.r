# Load libraries
library(data.table)


# Parameters
data_in <- 'data/2019-05-27-ejys-gbif-data/0018996-190415153152247.csv' 


# Read dataset
df_gbif <- fread(data_in, integer64="character", encoding='UTF-8')
df_gbif$idx <- 1:dim(df_gbif)[1]

print(paste0(Sys.time(), " --- df_gbif read in memory"))

