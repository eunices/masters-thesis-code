# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
dir_script <- '2019-06-19-jsa-type/'
dir_script_ch1 <- '2019-06-19-jsa-type-ch1/'

source(paste0(dir_script, "subset.r"))

# Ancillary datasets
lp_pop <- fread("data/2020-05-05-population-growth/WorldPopulationAnnual12000years_interpolated_HYDEandUNto2015.csv")
names(lp_pop) <- c("year", "pop")

# Read datasets
print(paste0("Read df"))
df <- get_df1(write=F)
df <- df[date.n<=2018]
df$date.decade <- paste0(substr(df$date.n, 1, 3), "0s")


print(paste0("Read df_country"))
df_country <- get_dis(write=F)
table(df_country$duplicated.row)
table(is.na(df_country$A.3))
dim(df_country); df_country <- df_country[!is.na(A.3)]; dim(df_country)
# df_country$duplicated.row <- NULL
df_country <- merge(df_country, df[, c("idx", "date.n")], all.x=T, all.y=F)[date.n<=2018]


print(paste0("Read df_publications  / df_publications_N"))
df_publications <- get_pub(write=F)
df_publications <- df_publications[date.n<=2018]
df_publications$date.decade <- paste0(substr(df_publications$date.n, 1, 3), "0s")

df_publications_N <- data.table(df_publications %>% separate_rows(idxes, sep="; "))
df_publications_N$idxes <- as.numeric(df_publications_N$idxes)
df_publications_N$type <- ""
df_publications_N[idxes <= 20669]$type <- "n_valid"
df_publications_N[idxes > 20669 & idxes <= 32285]$type <- "n_synonym"
df_publications_N[idxes > 32285 & idxes <= 33198]$type <- "n_subspecies"
df_publications_N[idxes > 33198]$type <- "n_var"
df_publications_N <- df_publications_N[, list(N_species=.N), 
                      by=c("date.n", "paper.authors", "journal", "title", 
                           "volume", "issue", "page.numbers.publication", "type")]
df_publications_N <- dcast(df_publications_N, 
                  date.n + paper.authors + journal + title + volume + issue + page.numbers.publication  ~ type, value.var="N_species", fun.aggregate=sum)
df_publications_N$n_species <- df_publications_N$n_valid + df_publications_N$n_synonym
df_publications_N$date.decade <- paste0(substr(df_publications_N$date.n, 1, 3), "0s")


print(paste0("Read df_continent"))
df_continent <- fread(paste0(dir_data, basefile, " filtered_5-species-cty3-continent.csv"),
                      integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
table(df_continent$duplicated.row)
table(is.na(df_continent$countries))
df_continent$duplicated.row <- NULL
df_continent <- df_continent[date.n<=2018]


print(paste0("Read df_trop_type1"))
df_trop_type1 <- fread(paste0(dir_data, basefile, " filtered_5-species-cty6-trop-type1.csv"),
                       integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
table(is.na(df_trop_type1$Latitude_type))
df_trop_type1 <- df_trop_type1[date.n<=2018]


print(paste0("Read df_trop_type2"))
df_trop_type2 <- fread(paste0(dir_data, basefile, " filtered_5-species-cty7-trop-type2.csv"),
                       integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
table(is.na(df_trop_type2$Latitude_type))
df_trop_type2 <- df_trop_type2[date.n<=2018]


print(paste0("Read taxonomic_effort / taxonomic_effort_long"))
taxonomic_effort <- fread(paste0(dir_data, basefile, " describers_6.0-active-by-year.csv"),
                          integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
taxonomic_effort$species_per_real_taxonomist <- 
    taxonomic_effort$N_species_described / taxonomic_effort$N_real_describers
taxonomic_effort$species_per_real_taxonomist_weighted <- 
    taxonomic_effort$N_species_described / taxonomic_effort$N_weighted_real_describers
taxonomic_effort <- taxonomic_effort[years<=2018]

taxonomic_effort_long <- data.table(taxonomic_effort %>% gather(type, N, N_describers:N_synonyms))

print(paste0("Read df_describers / df_describers_year "))
df_describers <- get_des(write=F)
df_describers_template <- data.frame(date.n=integer(), full.name.of.describer=character())

for (i in 1:length(df_describers$idx_auth)) {
    row <- df_describers[idx_auth==i]

    # only include authors which are "real taxonomist"
    if (!(row$spp_N_1st_auth_s == 0)) {
        d <- expand.grid(date.n=row$min:row$max_corrected, 
                         full.name.of.describer=row$full.name.of.describer.n)
        df_describers_template <- rbind(df_describers_template, d)
    }
}

df_describers_year <- df %>% separate_rows(full.name.of.describer, sep="; ")
df_describers_year <- df_describers_year[, c("idx", "full.name.of.describer", "date.n")]
df_describers_year <- df_describers_year[, list(.N), by=c("full.name.of.describer", "date.n")]

df_describers_year <- merge(df_describers_template, df_describers_year, all.x=T, all.y=F, 
                            by=c("full.name.of.describer", "date.n"))
df_describers_year <- data.table(df_describers_year)
df_describers_year[is.na(N)]$N <- 0
df_describers_year$date.decade <- paste0(substr(df_describers_year$date.n, 1, 3), "0s")
df_describers_year <- df_describers_year[date.n<=2018]

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read shp files
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Read shpfiles"))

if(!exists("shp_gadm_0")) {
    print(paste0("Read shp_gadm_0"))
    shp_gadm_0 <- sf::st_read('data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp')
}

# if(!exists("shp_biogeo")) {
#     print(paste0("Read shp_biogeo"))
#     shp_biogeo <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo2.shp')
# }

# if(!exists("shp_biogeo_holt")) {
#     print(paste0("Read shp_biogeo_holt"))
#     shp_biogeo_holt <- sf::st_read('data/geo_processed/gadm/gadm36_boundaries_utf8_biogeo_holt2.shp')
# }

# if(!exists("shp_continents")) {
#     print(paste0("Read shp_continents"))
#     shp_continents <- sf::st_read('data/geo_processed/gadm/gadm36_0_utf8_continents.shp')
# }
