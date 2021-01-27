# Purpose: calculate author metrics

source('2020-08-31-jsa-type-v2/00-init/main.r')
print(paste0(Sys.time(), " ----- df01.r"))

# Read describers data ---------------------------------------------------------
file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_1.csv")
df_des <- read_escaped_data_v2(file)

# Read data --------------------------------------------------------------------
file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
df <- read_escaped_data_v2(file)

file <- paste0(v2_dir_data_raw_clean, "lp-surname.csv")
lp_surname <- read_escaped_data_v2(file)
lp_surname <- lp_surname[
    , c("full.name.of.describer", "last.name", "last.name.no.initials")
]

# Merge surname ----------------------------------------------------------------

df_des <- merge(
    df_des, lp_surname, by="full.name.of.describer", all.T=T, all.y=F
)

# Calculate author metrics -----------------------------------------------------

# Separate df by author for metrics 
cols <- c(
    "file", "idx", "duplicated", "genus", "species",
    "author", "status", "full.name.of.describer", "date"
)

df_m <- df[duplicated == FALSE, ..cols]
df_m <- data.table(separate_rows(df_m, full.name.of.describer, sep = "; "))
df_m[, order := seq_len(.N), by=idx]
df_m <- df_m[order(idx, order)]
df_m[, len := .N, by="idx"]
df_m$order_c <- as.character(df_m$order)
df_m[len == order & len == 2,]$order_c <- "S"
df_m[len == order & len > 2,]$order_c <- "L"

# Valid species (prefixed by "ns_", only VALID SPECIES)
df_m_valid <- df_m[status == "Valid species" & date <= cutoff, list(
    ns_max = max(date, na.rm=T), 
    ns_min = min(date, na.rm=T), 
    ns_spp_N = length(unique(idx)), 
    ns_spp_N_1st_auth = sum(order_c=="1"),
    ns_spp_N_1st_auth_s = sum(order_c %in% c("1", "S")),
    ns_spp_N_last_auth = sum(order_c == "L")),
    by = "full.name.of.describer"]

# All species (prefixed by nothing; ALL SPECIES, including subspecies)
df_m_all <- df_m[date <= cutoff, list(
    max = max(date, na.rm=T), 
    min = min(date, na.rm=T),
    spp_N = length(unique(idx)), 
    spp_N_1st_auth = sum(order_c == "1"), 
    spp_N_1st_auth_s = sum(order_c %in% c("1", "S")), 
    spp_N_last_auth = sum(order_c == "L")),
    by = "full.name.of.describer"]


# Synonyms (prefixed by "syn_"; only SYNONYMS)
df_m_s <- df_m[status == "Synonym" & date <= cutoff, list(
    syn_max = max(date, na.rm=T), 
    syn_min = min(date, na.rm=T), 
    syn_spp_N = length(unique(idx)), 
    syn_spp_N_1st_auth = sum(order_c == "1"),
    syn_spp_N_1st_auth_s = sum(order_c %in% c("1", "S")),
    syn_spp_N_last_auth = sum(order_c == "L")), 
    by = "full.name.of.describer"]

# Combine 
df_des <- merge(
    df_des, df_m_valid,
    by = "full.name.of.describer",
    all.x = T, all.y = F
)

df_des <- merge(
    df_des, df_m_all,
    by = "full.name.of.describer",
    all.x = T, all.y = F
)

df_des <- merge(
    df_des, df_m_s,
    by = "full.name.of.describer",
    all.x = T, all.y = F
)


# Change NA to 0
columns <- unlist(lapply(df_des, class))
integer_cols <- columns[columns=='integer']
exclude_columns <- c("dob.describer", "dod.describer", "max")
integer_cols <- integer_cols[!names(integer_cols) %in% exclude_columns]
for (col in names(integer_cols)) df_des[is.na(get(col)), (col) := 0]


# Making max corrected
df_des$max_corrected <- as.integer(df_des$max)
df_des[alive=="Y"]$max_corrected <- cutoff

df_des$ns_max_corrected <- as.integer(df_des$ns_max)
df_des[alive=="Y"]$ns_max_corrected <- cutoff


# Other metrics
df_des$min <- as.integer(df_des$min)
df_des$dod.describer <- as.integer(df_des$dod.describer)
df_des$dob.describer <- as.integer(df_des$dob.describer)

df_des$years_active <- df_des$max_corrected - df_des$min +1
df_des$years_alive <- df_des$dod.describer - df_des$dob.describer + 1
df_des$years_diff <- df_des$years_alive - df_des$years_active

df_des$ns_species_per_year_active <- round(
    df_des$ns_spp_N / df_des$years_active, 2
)

df_des$ns_species_per_year_alive <- round(
    df_des$ns_spp_N / df_des$years_alive, 2
)

# First author defined as first, or first and second for two authors
df_des$prop_species_as_1st_author_s <- round(
    df_des$spp_N_1st_auth_s / df_des$spp_N, 2
)

# Proportion of synonyms
df_des$prop_species_syn <- round(
    df_des$syn_spp_N / (df_des$ns_spp_N + df_des$syn_spp_N), 2
)

# Note: last part of df1.4 omitted as publication != authored by describer, 
# but added later in Jan 2021 as it is needed for calculation in chapter 1,
# used idx / full.name.of.describer to link publication to describer


# Publication metrics

df_sp_per_pub <- df[
    duplicated == FALSE &
    status %in% c("Valid species", "Synonym"), 
    list(
        idxes = paste0(idx, collapse = ", "), 
        N = .N, 
        N_valid = sum(status == "Valid species"),
        N_synonym = sum(status == "Synonym")
    ),
    by = c(ppcol_n)
]

df_sp_per_pub <- data.table(separate_rows(df_sp_per_pub, "idxes", sep = ", "))
df_sp_per_pub$idxes <- as.integer(df_sp_per_pub$idxes)

df_sp_per_pub <- merge(
    df_sp_per_pub, df[, c("idx", "full.name.of.describer")],
    by.x = "idxes", by.y = "idx", all.x = T, all.y =F
)

df_sp_per_pub <- unique(data.table(separate_rows(
    df_sp_per_pub, "full.name.of.describer", sep = "; "
)))

df_sp_per_pub$idxes <- NULL

df_sp_per_pub <- unique(df_sp_per_pub)

author_ss <- df_sp_per_pub[, 
    list(
        spp_per_pub_mean=mean(N),
        spp_per_pub_sd=sd(N),
        n_pubs=.N
    ),
    by="full.name.of.describer"]

author_ss[is.na(author_ss)] <- 0


# Count mean/SD number of species described per publication 
# ONLY FOR <=20 years and <=5 years before death

df_des_with_dod <- 
    df_des[!is.na(dod.describer), c("full.name.of.describer", "dod.describer")]

df_sp_per_pub2 <- merge(
    df_sp_per_pub, df_des_with_dod,
    by='full.name.of.describer', all.x=T, all.y=T
) # filter only those with dod

# remove those authors without dates
df_sp_per_pub2 <- df_sp_per_pub2[!is.na(date)] 

df_sp_per_pub2$published_years_before_death <- 
    df_sp_per_pub2$dod.describer - df_sp_per_pub2$date

df_sp_per_pub2[published_years_before_death <0]$published_years_before_death <-
    NA 

df_sp_per_pub2$pub_yr_cat <- ""
df_sp_per_pub2[published_years_before_death <= 20]$pub_yr_cat <- "20y"
df_sp_per_pub2[published_years_before_death <= 5]$pub_yr_cat <- 
    paste0(df_sp_per_pub2[published_years_before_death <= 5]$pub_yr_cat, "; 5y")

df_sp_per_pub2 <- data.table(
    separate_rows(df_sp_per_pub2, "pub_yr_cat", sep = "; ")
)

author_ss2 <- df_sp_per_pub2[
    pub_yr_cat != "", 
    list(
        spp_per_pub_mean=mean(N),
        spp_per_pub_sd=sd(N),
        n_spp=sum(N),
        n_pubs=.N
    ),
    by=c("full.name.of.describer", "pub_yr_cat")
]

authors_ss2 <- dcast(
    author_ss2, full.name.of.describer  ~ pub_yr_cat,
    value.var=c("spp_per_pub_mean", "spp_per_pub_sd", "n_spp", "n_pubs")
)

authors_ss2 <- merge(
    data.frame(full.name.of.describer = df_des_with_dod$full.name.of.describer),
    authors_ss2,
    all.x=T, all.y=F, by = "full.name.of.describer"
)

authors_ss2[is.na(authors_ss2)] <- 0

# Merge info back


df_des <- merge(
    df_des, author_ss, 
    all.x = T, all.y = F,
    by = "full.name.of.describer"
)

df_des <- merge(
    df_des, authors_ss2, 
    all.x = T, all.y = F,
    by = "full.name.of.describer"
)

# Remove those with 0 species dscribed

print(paste0(
        "These authors with 0 species described: ", 
        paste0(df_des[spp_N == 0]$full.name.of.describer, collapse = ", ")
))

df_des <- df_des[spp_N != 0]

# Write data -------------------------------------------------------------------
file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_2.csv")
fwrite(df_des, file)


# df[idx %in% c(28735, 34671), c("duplicated", ..bcol)]
# df[genus == "Bombus" & species == "perezi", c("duplicated", ..bcol)]
