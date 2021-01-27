
# source("2020-08-31-jsa-type-v2-ch1/02-oth.r")

# Information about code:
# This code corresponds to exploratory data analyses for my MSc thesis.
# They are pertaining to EDA for the authors (relevant for Chapter 1).
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2020-08-31-jsa-type-v2/subset.r')

source('2020-08-31-jsa-type-v2-ch1/libraries.r')
source('2020-08-31-jsa-type-v2-ch1/params.r')



# TODO: 
# Where are most of the describers? 
# How many species in author lifetime? How many species per year?
# How many authors per species over time?
# What are the trends in species distribution (N number of species in publications)?
# What is the description rate across countries?
# Where are the specimens/ repositories/ publishers?

# How many type repository in each genus?

# Calculate effort it takes to describe all species?


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - basic summary statistics for taxonomist analysis
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- basic summary statistics for species dataset"))

df <- get_df()
authors <- df[
    status %in% c("Valid species", "Synonym"), 
    c("full.name.of.describer", "idx")
    ] %>% 
    separate_rows(full.name.of.describer, sep="; ")

authors <- data.table(authors)[, .N, by="idx"]

ggplot(authors, aes(x=N)) + geom_histogram(binwidth=1) +
    labs(x="\nN authors", y="Frequency\n") + theme
table(authors$N)
prop.table(table(authors$N))

print(paste0(Sys.time(), " --- basic summary statistics for taxonomist dataset"))

df_describers <- get_des()

# How long are taxonomists active for? 
ggplot(df_describers, aes(years_active)) + 
    geom_histogram(binwidth=1) +
        theme_minimal()

summary(df_describers$years_active)
# Median = 3
# Min = 1, Max = 83

# How many species are described?
ggplot(df_describers, aes(spp_N)) + 
    geom_histogram(binwidth=100) +
        theme_minimal()

summary(df_describers$spp_N)
# Median = 4
# Min = 1, Max = 3136 (by a far margin)

# Who were the greatest taxonomists? Are the greatest taxonomists still alive? None are alive sadly
df_describers[
    order(-spp_N)
][1:10][,c("full.name.of.describer", "dod.describer")]

df_describers[
    order(-spp_N_1st_auth_s)
][1:10][,c("full.name.of.describer", "dod.describer")]


# Were the greatest taxonomists always first authors? 

## Last authors
df_describers$prop_spp_last_auth <- 
    df_describers$spp_N_last_auth / df_describers$spp_N

cor(
    df_describers$spp_N, 
    df_describers$prop_spp_last_auth)^2

cor(
    df_describers[spp_N<500]$spp_N, 
    df_describers[spp_N<500]$prop_spp_last_auth)^2

ggplot(
    df_describers[spp_N<500], 
    aes(spp_N, prop_spp_last_auth)
) + geom_point() + theme_minimal()

ggplot(
    df_describers, 
    aes(spp_N, prop_spp_last_auth)
) + geom_point() + theme_minimal()

## Not as first authors
df_describers$prop_spp_not_1st <- 
    1- df_describers$prop_species_as_1st_author_s

cor(
    df_describers$spp_N, 
    df_describers$prop_spp_not_1st)^2

cor(df_describers[spp_N<500]$spp_N, 
    df_describers[spp_N<500]$prop_spp_not_1st)^2

ggplot(
    df_describers[spp_N<500], 
    aes(spp_N, prop_spp_not_1st)
) + geom_point() + theme_minimal()

ggplot(
    df_describers, 
    aes(spp_N, prop_spp_not_1st)
) + geom_point() + theme_minimal()
# Does not appear to be a correlation when a continuous variable

# But probably there are some who are always last authors for all
df_describers[order(-prop_spp_last_auth)][
    ,c("full.name.of.describer", "prop_spp_last_auth")]

# Also as seen here
# Hist of prop_spp_last_auth
ggplot(df_describers, aes(prop_spp_last_auth)) + 
    geom_histogram(binwidth=0.1) + theme_minimal()
# INTERESTING. The second peak

ggplot(df_describers, aes(prop_spp_not_1st)) + 
    geom_histogram(binwidth=0.1) + theme_minimal()
# Similar to the previous plot

# when were these people alive
summary(as.numeric(df_describers[prop_spp_last_auth<0.9]$dob.describer))
summary(as.numeric(df_describers[prop_spp_last_auth>=.9]$dob.describer))
# Not very reliable due to missing data

# when were these people active
summary(as.numeric(df_describers[prop_spp_last_auth<0.9]$max))
summary(as.numeric(df_describers[prop_spp_last_auth>=.9]$max))
# Seem that those with higher proportion of species described as last author are more recent

# Where are most of the taxonomists
des_a <- df_describers[spp_N_1st_auth_s>=1]

des_a1 <- data.table(df_describers[,
    c("full.name.of.describer", "residence.country.describer")
] %>% separate_rows(residence.country.describer, sep= "; "))

des_a1 <- des_a1[, .N, by="full.name.of.describer"]

table(des_a1$N)
prop.table(table(des_a1$N))

# Write datasets
df_describers_loc <- df_describers[, c(
        "full.name.of.describer", "residence.country.describer"
)]

df_describers_loc <- data.table(separate_rows(
    df_describers_loc, residence.country.describer, sep = "; "
))

df_describers_loc <- df_describers2[!duplicated(full.name.of.describer)]

dataset_write <- merge(
    des_a1[N>=2], 
    df_describers_loc,
    by="full.name.of.describer", all.x=T, all.y=F
)


file <- paste0(
    dir_table_ch1, "oth-2019-10-28-taxonomist-country-res-morethanone.csv"
)

write.csv(dataset_write, file)

file <- paste0(
    dir_table_ch1, "oth-2019-10-28-taxonomist-country-res-summary.csv"
)

write.csv(
    df_describers[, .N,by=c("residence.country.describer")][order(-N)],
    file
)  
