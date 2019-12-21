# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - no of taxonomist active per year
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': number of taxonomist active per year"))

filepath <- paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv")
describer_data <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
describer_data[, names(describer_data) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes
describer_data <- describer_data[years_active > 0]

filepath <- paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv")
describer_date <- fread(filepath, na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describer_date[, names(describer_date) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

filepath <- paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_1-clean.csv")
synonyms <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
synonyms[, names(synonyms) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

###########################
# For taxonomic_effort1
###########################
# Subset dataset for taxonomic_effort1
describers <- describer_data[, c("idx_auth", "full.name.of.describer.n", 
                                 "min", "max_corrected", "ns_species_per_year_active")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max_corrected)
describers$years <- seq
describers <- data.table(describers %>% unnest(years))
taxonomic_effort1 <- 
    describers[, list(N_describers = length(idx_auth), 
                      N_weighted_describers = sum(as.numeric(ns_species_per_year_active))), 
               by=years][order(as.numeric(years))][
                   ,c("years", "N_describers", "N_weighted_describers")]

# Check, since cut-off year is 2018, to exclude Silas Bossert
# describer_data[full.name.of.describer.n =="Silas Bossert"]
# describers[!is.finite(as.numeric(describers$ns_species_per_year_active))]

# Merge to template dataset
min_year <- min(taxonomic_effort1$years)
max_year <- max(taxonomic_effort1$years)
taxonomic_effort1 <- merge(data.frame(years=min_year:max_year), taxonomic_effort1, 
                           by="years", all.x=T, all.y=F)

###########################
# For taxonomic_effort2
###########################
# Subset dataset for taxonomic_effort2
# Exclude these authors: no first author publications at all
to_exclude <- describer_data[spp_N_1st_auth_s == 0]$idx_auth 

# N number of describers (N and weighted)
describers <- describer_data[!(idx_auth %in% to_exclude), 
                             c("idx_auth", "full.name.of.describer.n", 
                               "min", "max_corrected", "ns_species_per_year_active")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max_corrected)
describers$years <- seq
describers <- data.table(describers %>% unnest(years))
taxonomic_effort2 <- 
    describers[, list(N_real_describers = length(idx_auth), 
                      N_weighted_real_describers = sum(as.numeric(ns_species_per_year_active))), 
               by=years][order(as.numeric(years))][
                   , c("years", "N_real_describers", "N_weighted_real_describers")]

############################
# Merge taxonomic_effort1&2
###########################
taxonomic_effort <- merge(taxonomic_effort1, taxonomic_effort2, by="years", all.x=T, all.y=F)

# TODO: plot time series of N=1 species described, N=2 etc.

############################
# Count species
###########################
# N species
described_species_by_year <- 
    describer_date[idxes %in% 1:20669, 
                   list(N_species_described=length(unique(idxes))), 
                   by="date.n"][,c("date.n", "N_species_described")]
per_year1 <- merge(taxonomic_effort, described_species_by_year,
                   by.x="years", by.y="date.n", all.x=T, all.y=F)

# N synonyms
described_species_by_year <- 
    synonyms[, 
             list(N_synonyms=length(unique(idx))), 
             by="date.n"][,c("date.n", "N_synonyms")]
per_year2 <- merge(per_year1, described_species_by_year,
                   by.x="years", by.y="date.n", all.x=T, all.y=F)

per_year2[is.na(per_year2)] <- 0
per_year2 <- data.table(per_year2)

filepath <- paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_6.0-active-by-year.csv") 
write.csv(per_year2[years<=2018], filepath, na='', row.names=F, fileEncoding="UTF-8")