# Load libraries
library(data.table)

data_dir <- 'data/2018-08-05-amnh-bees/' 
files <- list.files(data_dir, full.names=T)

# Read dataset
df_amnh <- fread(files[1], encoding='UTF-8')

for (i in 2:length(files)) {
    print(paste0("Reading ", files[i]))
    df <- fread(files[i], fill=T)
    df_amnh <- rbind(df_amnh, df)
}

df_amnh$idx <- 1:dim(df_amnh)[1]
df_amnh$binomial.nomenclature <- paste0(df$Genus, " ", df_amnh$species)

print(paste0(Sys.time(), " --- df_amnh read in memory"))
