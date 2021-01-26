# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
dir_script <- '2020-08-31-jsa-type-v2/'
source(paste0(dir_script, "subset.r"))

################################################################################

# Parameters
year_end <- 2019
theme <- theme_minimal(base_size=7)

dir_output <- "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\"
dir_plot <- paste0(dir_output, "_figures\\_ch1\\")
dir_data_ch1 <- paste0(dir_output, "_data\\_ch1\\")

################################################################################

# Datasets

# Lookup datasets
lp_pop <- get_lp_pop()

# Other datasets
print(paste0("Read valid species"))
df <- get_df()
df <- df[date <= year_end]
df$date.decade <- paste0(substr(df$date, 1, 3), "0s")

df_all <- df # all 
df_rec <- df[status %in% c("Valid species", "Synonym")] # all valid species/syn
df <- df[status %in% "Valid species"] # all valid species

print(paste0("Read df_publications  / df_publications_N"))
df_publications <- get_pub()
df_publications <- df_publications[date <= year_end]
df_publications$date.decade <- paste0(substr(df_publications$date, 1, 3), "0s")

df_publications_N <- data.table(df_publications %>% separate_rows(idxes, sep=", "))
df_publications_N$idxes <- as.numeric(df_publications_N$idxes)

lp_status <- df_all[, c("idx", "status")]
df_publications_N$type <- lp_status[match(df_publications_N$idxes, idx)]$status
df_publications_N$type <- 
    paste0("n_", gsub(" ", "_", tolower(df_publications_N$type)))

df_publications_N <- df_publications_N[, list(N_species=.N), 
    by=c(
        "date",  "journal", "title", 
        "volume", "issue", "type"
)]

df_publications_N <- dcast(
    df_publications_N, 
    date + journal + title + volume + issue ~ type, 
    value.var="N_species", 
    fun.aggregate = sum
)

df_publications_N$n_species <- 
    df_publications_N$n_valid_species + df_publications_N$n_synonym

df_publications_N$date.decade <- 
    paste0(substr(df_publications_N$date, 1, 3), "0s")


print(paste0("Read taxonomic_effort / taxonomic_effort_long"))
taxonomic_effort <- get_n_active_describers_by_year()

taxonomic_effort$species_per_real_taxonomist <- 
    taxonomic_effort$N_species_described / taxonomic_effort$N_real_describers

taxonomic_effort$species_per_real_taxonomist_weighted <- 
    taxonomic_effort$N_species_described / taxonomic_effort$N_weighted_real_describers

taxonomic_effort <- taxonomic_effort[years <= year_end]

taxonomic_effort$N_real_describers_roll <- 
    rollmean(taxonomic_effort$N_real_describers, 10, fill = list(NA, NULL, NA))

taxonomic_effort$N_weighted_real_describers_roll <- 
    rollmean(
        taxonomic_effort$N_weighted_real_describers, 10, 
        fill = list(NA, NULL, NA)
    )

taxonomic_effort$species_per_real_taxonomist_roll <-
    rollmean(
        taxonomic_effort$species_per_real_taxonomist, 10, 
        fill = list(NA, NULL, NA)
    )

taxonomic_effort$species_per_real_taxonomist_weighted_roll <-
    rollmean(
        taxonomic_effort$species_per_real_taxonomist_weighted, 10, 
        fill = list(NA, NULL, NA)
    )

taxonomic_effort_long <- data.table(
    taxonomic_effort %>% gather(type, N, N_describers:N_synonyms)
)


print(paste0("Read df_describers / df_describers_year "))
df_describers <- get_des()
df_describers[, 11:48] <- lapply(df_describers[,11:48], as.numeric)
df_describers$dod.describer <- as.integer(df_describers$dod.describer)
df_describers$dob.describer <- as.integer(df_describers$dob.describer)

df_describers_template <- data.frame(
    date=integer(), full.name.of.describer=character()
)

for (i in 1:dim(df_describers)[1]) {
    row <- df_describers[i, ]

    # only include authors which are "real taxonomist"
    if (!(row$spp_N_1st_auth_s == 0)) {

        d <- expand.grid(
            date = row$min:row$max_corrected, 
            full.name.of.describer = row$full.name.of.describer
        )

        df_describers_template <- rbind(df_describers_template, d)
    }
}

df_describers_year <- df %>% separate_rows(full.name.of.describer, sep="; ")

df_describers_year <- data.table(df_describers_year[
    , c("idx", "full.name.of.describer", "date")
])

df_describers_year <- df_describers_year[, .N,
    by=c("full.name.of.describer", "date")
]

df_describers_year <- merge(
    df_describers_template, df_describers_year,
    all.x=T, all.y=F, 
    by=c("full.name.of.describer", "date")
)

df_describers_year <- data.table(df_describers_year)

df_describers_year[is.na(N)]$N <- 0

df_describers_year$date.decade <- paste0(
    substr(df_describers_year$date, 1, 3), "0s"
)

df_describers_year <- df_describers_year[date <= year_end]


