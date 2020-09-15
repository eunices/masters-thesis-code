# Purpose: calculate taxonomic effort over time

source('2020-08-31-jsa-type-v2/init/init.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
df <- read_escaped_data_v2(file)

file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_2.csv")
df_des <- read_escaped_data_v2(file)


##########################################
# Part 1: all valid species
des <- df_des[!is.na(max_corrected), 
              c("full.name.of.describer", 
                "min", "max_corrected", "ns_species_per_year_active")
    ]

# Expand dataset by min and max years
seq <- mapply(
    function(a, b) seq(a, b), 
    a = des$min, 
    b = des$max_corrected
)

des$years <- seq
des <- data.table(des %>% unnest(years))
des$years <- as.integer(des$years)

# Calculate number of describers, and weighted number of describers
des$ns_species_per_year_active <- as.numeric(des$ns_species_per_year_active)

taxonomic_effort1 <- des[, list(
    N_describers = length(unique(full.name.of.describer)), 
    N_weighted_describers = sum(ns_species_per_year_active)
    ), 
by = years][order(years)]

# Merge to template dataset
min_year <- min(taxonomic_effort1$years)
max_year <- cutoff
template <- data.frame(years=min_year:max_year)
taxonomic_effort1 <- merge(
    template, taxonomic_effort1, 
    by = "years", all.x = TRUE, all.y = FALSE
)




##########################################
# Part 2: Exclude authors with no first author publications at all
to_exclude <- df_des[spp_N_1st_auth_s == 0]$full.name.of.describer

des <- df_des[!(full.name.of.describer %in% to_exclude) & 
              !is.na(max_corrected), 
              c("full.name.of.describer", "min", "max_corrected", 
              "ns_species_per_year_active")]

# Expand dataset by min and max years
seq <- mapply(
    function(a, b) seq(a, b),
    a = des$min, 
    b = des$max_corrected
)

des$years <- seq
des <- data.table(des %>% unnest(years))
des$years <- as.integer(des$years)

# Summarise
des$ns_species_per_year_active <- as.numeric(des$ns_species_per_year_active)

taxonomic_effort2 <- 
    des[, list(
        N_real_describers = length(unique(full.name.of.describer)), 
        N_weighted_real_describers = sum(ns_species_per_year_active)
    ), 
    by = years][order(years)]



##########################################
# Part 3: by number of species described
N_species_thres <- 10


# Exclude authors that have no first author publications
des <- df_des[!(full.name.of.describer %in% to_exclude) & 
              as.integer(spp_N) <= N_species_thres, 
              c("full.name.of.describer", "min", "max_corrected",
                "ns_species_per_year_active", "spp_N")]

# Reshape data by number of species described
des <- data.table(dcast(
    des, 
    full.name.of.describer + min + max_corrected ~ spp_N,
    value.var = "ns_species_per_year_active"
))

# Sum the number of authors by number of species described
start_col <- which(names(des) == 1)
for (i in start_col:(start_col + 9)) {
    des[[i]] <- as.numeric(des[[i]])
}

# Replace NA with 0 in numeric cols
nms <- names(des)[sapply(des, is.numeric)]
des[, (nms):=lapply(.SD, function(i) replace(i, is.na(i), 0)), .SDcols=nms]

des$`2` <- des$`1` + des$`2`
des$`3` <- des$`2` + des$`3`
des$`4` <- des$`3` + des$`4`
des$`5` <- des$`4` + des$`5`
des$`6` <- des$`5` + des$`6`
des$`7` <- des$`6` + des$`7`
des$`8` <- des$`7` + des$`8`
des$`9` <- des$`8` + des$`9`
des$`10` <- des$`9` + des$`10`

# Expand data by min/max for each author
seq <- mapply(
    function(a, b) seq(a, b), 
    a = des$min, 
    b = des$max_corrected
)

des$years <- seq
des <- data.table(des %>% unnest(years))

# Calculate authors that only authored 1,2,3,4,5.. publications

taxonomic_effort3 <- 
    des[, list(
        N_real_describers.1 = length(which(`1` > 0)), 
        N_real_describers.2 = length(which(`2` > 0)), 
        N_real_describers.3 = length(which(`3` > 0)), 
        N_real_describers.4 = length(which(`4` > 0)), 
        N_real_describers.5 = length(which(`5` > 0)), 
        N_real_describers.6 = length(which(`6` > 0)), 
        N_real_describers.7 = length(which(`7` > 0)), 
        N_real_describers.8 = length(which(`8` > 0)), 
        N_real_describers.9 = length(which(`9` > 0)), 
        N_real_describers.10 = length(which(`10` > 0))
    ), 
    by = years][order(as.numeric(years))]



# Combine part 1, 2, 3
taxonomic_effort <- merge(
    taxonomic_effort1, 
    taxonomic_effort2, 
    by = "years", 
    all.x = TRUE, all.y = FALSE
)

taxonomic_effort <- merge(
    taxonomic_effort,
    taxonomic_effort3,
    by = "years", 
    all.x = TRUE, all.y = FALSE
)


############################
# Count species

# N species
described_species_by_year <- 
    df[status == "Valid species" & 
       duplicated == FALSE, 
       list(N_species_described = length(unique(idx))), 
       by = "date"][, c("date", "N_species_described")]

taxonomic_effort <- merge(
    taxonomic_effort, described_species_by_year,
    by.x = "years", by.y = "date", 
    all.x = TRUE, all.y = FALSE
)


# N synonyms
described_syn_by_year <- 
    df[status == "Synonyms" & 
       duplicated == FALSE, 
       list(N_synonyms = length(unique(idx))), 
            by = "date"][, c("date", "N_synonyms")]



# Combine data
taxonomic_effort <- merge(
    taxonomic_effort, described_syn_by_year,
    by.x = "years", by.y = "date", 
    all.x = T, all.y = F
)

taxonomic_effort[is.na(taxonomic_effort)] <- 0
taxonomic_effort <- data.table(taxonomic_effort)

rm(df)

file <- paste0(v2_dir_data_raw, v2_basefile, "-describer-effort.csv")
fwrite(taxonomic_effort, file)