# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating distribution dataset by other groups, 
# derived from the country distribution.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

source('2019-06-19-jsa-type/df/functions.R')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by country
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by country"))



# Read data
df <- read_escaped_data(paste0(dir_data_raw, basefile, " filtered_4.3-clean-coll.csv"))
df$idx <- as.integer(df$idx)

df_mapper2 <- read_escaped_data(paste0(dir_data_raw, basefile, " filtered_5-species-cty1.csv"))
df_mapper2 <- df_mapper2[, c("idx", "A.3")]
df_mapper2$idx <- as.integer(df_mapper2$idx)




# Merge to lookup
df_mapper2 <- merge(df_mapper2, 
                    lookup.cty[,c("A.3", "Latitude_type", "Latitude_type2", 
                                  "continent", "biogeo_wwf", "ecoregions2017_biome")], 
                    by.x="A.3", by.y="A.3", all.x=T, all.y=F)



# Subset relevant columns
df_merge <- df[,c("idx", "duplicated.row", "date.n", "full.name.of.describer")]
df_mapper2 <- merge(df_mapper2, df_merge, by.x="idx", by.y="idx", all.x=T, all.y=F)
df_mapper2 <- df_mapper2[date.n <=2018][duplicated.row=="FALSE"][order(as.numeric(idx))]




# Write data
output_filepath <- paste0(dir_data_raw, basefile, " filtered_5-species-cty2-cty.csv")
write.csv(df_mapper2, output_filepath, na='', row.names=F, fileEncoding="UTF-8")





# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by continent
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by continent"))



# Summarise data
group <- df_mapper2[, list(no_cty_in_trop = length(unique(A.3)),
                           cty_in_trop = paste(unique(A.3), collapse=', ')), 
                    by=c("idx", "biogeo_wwf", "full.name.of.describer",
                         "date.n", "duplicated.row")][
                                     order(idx)]




# Write data
output_filepath <- paste0(dir_data_raw, basefile, " filtered_5-species-cty3-continent.csv")
write.csv(group, output_filepath, na='', row.names=F, fileEncoding="UTF-8")




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - appending WWF's ecoregions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- appending WWF's ecoregions [using countries]"))


# Summarise data 
#### Using WWF's ####
group <- df_mapper2[, list(no_cty_in_trop = length(unique(A.3)),
                           cty_in_trop = paste(unique(A.3), collapse=', ')), 
                    by=c("idx", "biogeo_wwf", 
                         "full.name.of.describer", "date.n", "duplicated.row")][
                    order(idx)]



# Write data
output_filepath <- paste0(dir_data_raw, basefile, " filtered_5-species-cty4-biogeo.csv")
write.csv(group, output_filepath, na='', row.names=F, fileEncoding="UTF-8")




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by tropical/not
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by tropical/not [using countries]"))


# Summarise data 
# By type1 of trop/sub tropical
group <- df_mapper2[, list(no_cty_in_trop = length(unique(A.3)),
                           cty_in_trop = paste(unique(A.3), collapse=', ')), 
                    by=c("idx", "Latitude_type", 
                            "full.name.of.describer", "date.n", "duplicated.row")][
                                     order(idx)]



# Write data
output_filepath <- paste0(dir_data_raw, basefile, " filtered_5-species-cty6-trop-type1.csv")
write.csv(group, output_filepath, na='', row.names=F, fileEncoding="UTF-8")




# Summarise data
# By type2 of trop/sub tropical
group <- df_mapper2[, list(no_cty_in_trop = length(unique(A.3)),
                           cty_in_trop = paste(unique(A.3), collapse=', ')), 
                    by=c("idx", "Latitude_type2", 
                         "full.name.of.describer", "date.n", "duplicated.row")][
                                     order(idx)]




# Write data
output_filepath <- paste0(dir_data_raw, basefile, " filtered_5-species-cty7-trop-type2.csv")
write.csv(group, output_filepath, na='', row.names=F, fileEncoding="UTF-8")


