# Purpose: calculate author metrics

source('2020-08-31-jsa-type-v2/00-init/main.r')

# Read describers data
file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_1.csv")
df_des <- read_escaped_data_v2(file)

# Read data
file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
df <- read_escaped_data_v2(file)

# Separate df by author for metrics
cols <- c("file", "idx", "duplicated", "genus", "species",
          "author", "status", "full.name.of.describer", "date")
df_m <- df[duplicated == FALSE, ..cols]
df_m <- data.table(separate_rows(df_m, full.name.of.describer, sep = "; "))
df_m[, order := seq_len(.N), by=idx]
df_m <- df_m[order(idx, order)]
df_m[, len := .N, by="idx"]

df_m$order_c <- as.character(df_m$order)
df_m[len == order & len == 2,]$order_c <- "S"
df_m[len == order & len > 2,]$order_c <- "L"


# Valid species
df_m_valid <- df_m[status == "Valid species" & date <= cutoff, list(
    ns_max = max(date, na.rm=T), 
    ns_min = min(date, na.rm=T), 
    ns_spp_N = length(unique(idx)), 
    ns_spp_N_1st_auth = sum(order_c=="1"),
    ns_spp_N_1st_auth_s = sum(order_c %in% c("1", "S")),
    ns_spp_N_last_auth = sum(order_c == "L")),
    by = "full.name.of.describer"]

# All species
df_m_all <- df_m[date <= cutoff, list(
    max = max(date, na.rm=T), 
    min = min(date, na.rm=T),
    spp_N = length(unique(idx)), 
    spp_N_1st_auth = sum(order_c == "1"), 
    spp_N_1st_auth_s = sum(order_c %in% c("1", "S")), 
    spp_N_last_auth = sum(order_c == "L")),
    by = "full.name.of.describer"]


# Synonyms
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
numeric_cols <- columns[columns=='integer']
for (col in names(numeric_cols)) df_des[is.na(get(col)), (col) := 0]


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
    df_des$syn_spp_N / df_des$spp_N, 2
)


# Note: last part of df1.4 omitted as publication != authored by describer


file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_2.csv")
fwrite(df_des, file)