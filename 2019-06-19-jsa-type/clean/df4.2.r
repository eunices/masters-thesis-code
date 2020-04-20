# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating distribution dataset by other groups, 
# derived from the country distribution.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by country
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by country"))

filepath_input <- paste0(dir_data, basefile, " filtered_4.3-clean-coll.csv")
df <- fread(filepath_input, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df$idx <- as.integer(df$idx)

filepath_input <- paste0(dir_data, basefile, " filtered_5-species-cty1.csv")
df_mapper2 <- fread(filepath_input, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df_mapper2 <- df_mapper2[, c("idx", "A.3")]
df_mapper2$idx <- as.integer(df_mapper2$idx)

df_mapper2 <- merge(df_mapper2, 
                    lookup.cty[,c("A.3", "Latitude_type", "Latitude_type2", 
                                  "continent", "biogeo_wwf", "ecoregions2017_biome")], 
                    by.x="A.3", by.y="A.3", all.x=T, all.y=F)

df_merge <- df[,c("idx", "duplicated.row", "date.n", "full.name.of.describer")]
df_mapper2 <- merge(df_mapper2, df_merge, by.x="idx", by.y="idx", all.x=T, all.y=F)
df_mapper2 <- df_mapper2[date.n <=2018][duplicated.row=="FALSE"][order(as.numeric(idx))]

output_filepath <- paste0(dir_data, basefile, " filtered_5-species-cty2-cty.csv")
write.csv(df_mapper2, output_filepath, na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by continent
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by continent"))

group <- df_mapper2[, list(no_cty_in_trop = length(unique(A.3)),
                           cty_in_trop = paste(unique(A.3), collapse=', ')), 
                    by=c("idx", "biogeo_wwf", 
                         "full.name.of.describer", "date.n", "duplicated.row")][
                                     order(idx)]

output_filepath <- paste0(dir_data, basefile, " filtered_5-species-cty3-continent.csv")
write.csv(group, output_filepath, na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - appending WWF's ecoregions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- appending WWF's ecoregions [using countries]"))

#### Using WWF's ####
group <- df_mapper2[, list(no_cty_in_trop = length(unique(A.3)),
                           cty_in_trop = paste(unique(A.3), collapse=', ')), 
                    by=c("idx", "biogeo_wwf", 
                         "full.name.of.describer", "date.n", "duplicated.row")][
                    order(idx)]

output_filepath <- paste0(dir_data, basefile, " filtered_5-species-cty4-biogeo.csv")
write.csv(group, output_filepath, na='', row.names=F, fileEncoding="UTF-8")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - by tropical/not
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- by tropical/not [using countries]"))

# By type1 of trop/sub tropical
group <- df_mapper2[, list(no_cty_in_trop = length(unique(A.3)),
                           cty_in_trop = paste(unique(A.3), collapse=', ')), 
                    by=c("idx", "Latitude_type", 
                            "full.name.of.describer", "date.n", "duplicated.row")][
                                     order(idx)]

output_filepath <- paste0(dir_data, basefile, " filtered_5-species-cty6-trop-type1.csv")
write.csv(group, output_filepath, na='', row.names=F, fileEncoding="UTF-8")

# By type2 of trop/sub tropical
group <- df_mapper2[, list(no_cty_in_trop = length(unique(A.3)),
                           cty_in_trop = paste(unique(A.3), collapse=', ')), 
                    by=c("idx", "Latitude_type2", 
                         "full.name.of.describer", "date.n", "duplicated.row")][
                                     order(idx)]

output_filepath <- paste0(dir_data, basefile, " filtered_5-species-cty7-trop-type2.csv")
write.csv(group, output_filepath, na='', row.names=F, fileEncoding="UTF-8")
