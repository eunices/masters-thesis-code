# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
dir_script <- '2019-06-19-jsa-type/'
source(paste0(dir_script, "subset.r"))

####################################################################################################
# Datasets

# Lookup datasets
lp_pop <- get_lp_pop()

# Other datasets
print(paste0("Read valid species"))
df <- get_df1(write=F)
df <- df[date.n<=2018]
df$date.decade <- paste0(substr(df$date.n, 1, 3), "0s")


print(paste0("Read synonyms"))
df2 <- get_df2(write=F)[ ,c("idx", "status", "date.n")]
df2 <- df[date.n<=2018]
df2$date.decade <- paste0(substr(df2$date.n, 1, 3), "0s")


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


print(paste0("Read taxonomic_effort / taxonomic_effort_long"))
taxonomic_effort <- get_n_active_describers_by_year()
taxonomic_effort$species_per_real_taxonomist <- 
    taxonomic_effort$N_species_described / taxonomic_effort$N_real_describers
taxonomic_effort$species_per_real_taxonomist_weighted <- 
    taxonomic_effort$N_species_described / taxonomic_effort$N_weighted_real_describers
taxonomic_effort <- taxonomic_effort[years<=2018]

taxonomic_effort$N_real_describers_roll <- 
    rollmean(taxonomic_effort$N_real_describers, 10, 
             fill = list(NA, NULL, NA))
taxonomic_effort$N_weighted_real_describers_roll <- 
    rollmean(taxonomic_effort$N_weighted_real_describers, 10, 
             fill = list(NA, NULL, NA))
taxonomic_effort$species_per_real_taxonomist_roll <-
    rollmean(taxonomic_effort$species_per_real_taxonomist, 10, 
             fill = list(NA, NULL, NA))    
taxonomic_effort$species_per_real_taxonomist_weighted_roll <-
    rollmean(taxonomic_effort$species_per_real_taxonomist_weighted, 10, 
             fill = list(NA, NULL, NA))


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