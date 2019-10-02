# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - no of taxonomist active per year
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': number of taxonomist active per year"))

describer_date <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describer_date[, names(describer_date) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

describer_data <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
describer_data[, names(describer_data) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

synonyms <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_1-clean.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
synonyms[, names(synonyms) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# Get dataset
describers <- describer_data[,c("idx_auth", "full.name.of.describer.n", "min", "max_corrected")]

# N number of describers
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max_corrected)
describers$years <- seq
describers <- describers %>% unnest(years)
describers[,N_describers := length(idx_auth), by=years]
describers_active_by_year <- unique(describers[,c("years", "N_describers")])[order(as.numeric(years))]

# Weighted 
describers <- describer_data[,c("idx_auth", "full.name.of.describer.n",
                                "min", "max_corrected",
                                "ns_species_per_year_active")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max_corrected)
describers$years <- seq
describers <- describers %>% unnest(years)
describers[,N_weighted_describers := sum(as.numeric(ns_species_per_year_active)), by=years]
describers_weighted_by_year <- unique(describers[,c("years", "N_weighted_describers")])[order(as.numeric(years))]

taxonomic_effort1 <- merge(describers_active_by_year, describers_weighted_by_year, by="years", all.x=T, all.y=T)
min_year <- min(taxonomic_effort1$years)
max_year <- max(taxonomic_effort1$years)
taxonomic_effort1 <- merge(data.frame(years=min_year:max_year), taxonomic_effort1, by="years", all.x=T, all.y=F)

# Exclude these
# no first author publications at all
to_exclude <- describer_data[spp_N_1st_auth_s == 0]$idx_auth 

# # to exclude those synonyms only and no first auth pub
# to_exclude <- describer_data[spp_N_1st_auth_s == 0 |
#                              ns_spp_N == 0]$idx_auth 

# N number of describers
describers <- describer_data[!(idx_auth %in% to_exclude), 
                             c("idx_auth", "full.name.of.describer.n", 
                               "min", "max_corrected")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max_corrected)
describers$years <- seq
describers <- describers %>% unnest(years)
describers[,N_real_describers := length(idx_auth), by=years]
describers_active_by_year <- unique(describers[,c("years", "N_real_describers")])[order(as.numeric(years))]

# Weighted 
describers <- describer_data[!(idx_auth %in% to_exclude),
                             c("idx_auth", "full.name.of.describer.n", 
                               "min", "max_corrected",
                               "ns_species_per_year_active")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max_corrected)
describers$years <- seq
describers <- describers %>% unnest(years)
describers[,  N_weighted_real_describers := sum(as.numeric(ns_species_per_year_active)), by="years"]
describers_weighted_by_year <- unique(describers[,c("years", "N_weighted_real_describers")])[order(as.numeric(years))]

taxonomic_effort2 <- merge(describers_active_by_year, describers_weighted_by_year, by="years", all.x=T, all.y=T)
taxonomic_effort <- merge(taxonomic_effort1, taxonomic_effort2, by="years", all.x=T, all.y=F)

# number of species
described_species_by_year <- describer_date[]
described_species_by_year[,N_species_described:=length(unique(idxes)),by="date.n"]
described_species_by_year <- unique(
        described_species_by_year[,c("date.n", "N_species_described")])

described_per_year_final2 <- merge(taxonomic_effort, described_species_by_year,
                                   by.x="years", by.y="date.n", all.x=T, all.y=F)

# number of synonyms
described_species_by_year <- describer_date[]
described_species_by_year[,N_species_described:=length(unique(idxes)),by="date.n"]
described_species_by_year <- unique(
        described_species_by_year[,c("date.n", "N_species_described")])

described_per_year_final2 <- merge(taxonomic_effort, described_species_by_year,
                                   by.x="years", by.y="date.n", all.x=T, all.y=F)

# synonyms
described_species_by_year <- synonyms[]
described_species_by_year[,N_synonyms:=length(unique(idx)),by="date.n"]
described_species_by_year <- unique(
        described_species_by_year[,c("date.n", "N_synonyms")])

described_per_year_final3 <- merge(described_per_year_final2, described_species_by_year,
                                   by.x="years", by.y="date.n", all.x=T, all.y=F)

described_per_year_final3[is.na(described_per_year_final3)] <- 0
described_per_year_final3 <- data.table(described_per_year_final3)

write.csv(described_per_year_final3[years<=2018], paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_6.0-active-by-year.csv"), na='', row.names=F, fileEncoding="UTF-8")
