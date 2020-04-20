# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating dataset for taxonomic effort.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - no of taxonomist active per year
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': number of taxonomist active per year"))

filepath <- paste0(dir_data, basefile, " describers_5.0-describers-final.csv")
describer_data <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
describer_data[, names(describer_data) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes
describer_data <- describer_data[years_active > 0]

filepath <- paste0(dir_data, basefile, " describers_4.0-denormalised2.csv")
describer_date <- fread(filepath, na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describer_date[, names(describer_date) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

filepath <- paste0(dir_data, basefile, " oth_1-clean.csv")
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

###########################
# For taxonomic_effort3
###########################
N_species <- 10
describers <- describer_data[!(idx_auth %in% to_exclude) & as.numeric(spp_N) <=N_species, 
                             c("idx_auth", "full.name.of.describer.n", 
                               "min", "max_corrected", "ns_species_per_year_active",
                               "spp_N")]
describers <- dcast(describers, 
      idx_auth + full.name.of.describer.n +  min + 
        max_corrected ~ as.numeric(spp_N), value.var="ns_species_per_year_active")

# Figure out how to loop this # TODO:
describers[!is.na(`2`), ]$`3` <- describers[!is.na(`2`), ]$`2`
describers[!is.na(`3`), ]$`4` <- describers[!is.na(`3`), ]$`3`
describers[!is.na(`4`), ]$`5` <- describers[!is.na(`4`), ]$`4`
describers[!is.na(`5`), ]$`6` <- describers[!is.na(`5`), ]$`5`
describers[!is.na(`6`), ]$`7` <- describers[!is.na(`6`), ]$`6`
describers[!is.na(`7`), ]$`8` <- describers[!is.na(`7`), ]$`7`
describers[!is.na(`8`), ]$`9` <- describers[!is.na(`8`), ]$`8`
describers[!is.na(`9`), ]$`10` <- describers[!is.na(`9`), ]$`9`

seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max_corrected)
describers$years <- seq
describers <- data.table(describers %>% unnest(years))
taxonomic_effort3 <- 
    describers[, list(N_real_describers.1 = length(which(!is.na(`1`))), 
                      N_real_describers.2 = length(which(!is.na(`2`))), 
                      N_real_describers.3 = length(which(!is.na(`3`))), 
                      N_real_describers.4 = length(which(!is.na(`4`))), 
                      N_real_describers.5 = length(which(!is.na(`5`))), 
                      N_real_describers.6 = length(which(!is.na(`6`))), 
                      N_real_describers.7 = length(which(!is.na(`7`))), 
                      N_real_describers.8 = length(which(!is.na(`8`))), 
                      N_real_describers.9 = length(which(!is.na(`9`))), 
                      N_real_describers.10 = length(which(!is.na(`10`)))
    ), by=years][order(as.numeric(years))]


############################
# Merge taxonomic_effort
###########################
taxonomic_effort <- merge(taxonomic_effort1, taxonomic_effort2, by="years", all.x=T, all.y=F)
taxonomic_effort <- merge(taxonomic_effort, taxonomic_effort3, by="years", all.x=T, all.y=F)

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

filepath <- paste0(dir_data, basefile, " describers_6.0-active-by-year.csv") 
write.csv(per_year2[years<=2018], filepath, na='', row.names=F, fileEncoding="UTF-8")