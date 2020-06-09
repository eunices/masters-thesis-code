# Information about code:
# This code corresponds to exploratory data analyses for my MSc thesis.
# They are pertaining to EDA for the authors (relevant for Chapter 1).
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2019-06-19-jsa-type/subset.r')

# Parameters
theme <- theme_minimal()


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

df <- get_df1(write=F)
authors <- df[, c("full.name.of.describer", "idx")] %>% 
    separate_rows(full.name.of.describer, sep="; ")
authors <- data.table(authors)[, .N, by="idx"]
ggplot(authors, aes(x=N)) + geom_histogram(binwidth=1) +
    labs(x="\nN authors", y="Frequency\n") + theme
table(authors$N)
prop.table(table(authors$N))

print(paste0(Sys.time(), " --- basic summary statistics for taxonomist dataset"))

df_describers <- get_des(write=F)
df_describers <- df_describers[full.name.of.describer.n != "Silas Bossert"]

# How long are taxonomists active for? 
ggplot(df_describers, aes(years_active)) + 
    geom_histogram(binwidth=1) +
        theme_minimal()
summary(df_describers$years_active)
# Median = 3
# Min = 1, Max = 83

# How many species are described?
ggplot(df_describers, aes(num_species_described)) + 
    geom_histogram(binwidth=100) +
        theme_minimal()
summary(df_describers$num_species_described)
# Median = 4
# Min = 1, Max = 3136 (by a far margin)

# Who were the greatest taxonomists? Are the greatest taxonomists still alive? None are alive sadly
df_describers[order(-num_species_described)][1:10][,c("full.name.of.describer.n", "dod.describer.n")]
df_describers[order(-num_first_auth_species_described
)][1:10][,c("full.name.of.describer.n", "dod.describer.n")]

# Hist of number of describers per species
ggplot(df_species_describers_sum, aes(N_authors)) + 
    geom_histogram(binwidth=1)
table(df_species_describers_sum$N_authors)

# Were the greatest taxonomists always first authors? 
## Last authors
cor(df_describers$num_species_described, df_describers$prop_species_described_as_last_author)^2
cor(df_describers[num_species_described<500]$num_species_described, 
    df_describers[num_species_described<500]$prop_species_described_as_last_author)^2

ggplot(df_describers[num_species_described<500], aes(num_species_described, prop_species_described_as_last_author)) +
    geom_point() + theme_minimal()
ggplot(df_describers, aes(num_species_described, prop_species_described_as_last_author)) +
    geom_point() + theme_minimal()

## Not as first authors
cor(df_describers$num_species_described, df_describers$prop_species_described_not_as_first_author)^2
cor(df_describers[num_species_described<500]$num_species_described, 
    df_describers[num_species_described<500]$prop_species_described_not_as_first_author)^2

ggplot(df_describers[num_species_described<500], 
       aes(num_species_described, prop_species_described_not_as_first_author)) +
    geom_point() + theme_minimal()
ggplot(df_describers, aes(num_species_described, prop_species_described_not_as_first_author)) +
    geom_point() + theme_minimal()
# Does not appear to be a correlation when a continuous variable

# But probably there are some who are always last authors for all
df_describers[order(-prop_species_described_as_last_author)][
    ,c("full.name.of.describer.n", "prop_species_described_as_last_author")]

# Also as seen here
# Hist of prop_species_described_as_last_author
ggplot(df_describers, aes(prop_species_described_as_last_author)) + 
    geom_histogram(binwidth=0.1) + theme_minimal()
# INTERESTING. The second peak

ggplot(df_describers, aes(prop_species_described_not_as_first_author)) + 
    geom_histogram(binwidth=0.1) + theme_minimal()
# Similar to the previous plot

# when were these people alive
summary(as.numeric(df_describers[prop_species_described_as_last_author<0.9]$dob.describer.n))
summary(as.numeric(df_describers[prop_species_described_as_last_author>=.9]$dob.describer.n))
# Not very reliable due to missing data

# when were these people active
summary(as.numeric(df_describers[prop_species_described_as_last_author<0.9]$max))
summary(as.numeric(df_describers[prop_species_described_as_last_author>=.9]$max))
# Seem that those with higher proportion of species described as last author are more recent

# Where are most of the taxonomists
des_a <- df_describers[spp_N_1st_auth_s>=1]

des_a1 <- df_describers[, c("full.name.of.describer.n", "residence.country.describer.full")] %>% 
    separate_rows(residence.country.describer.full, sep= "; ")
des_a1 <- des_a1[, .N, by="full.name.of.describer.n"]
table(des_a1$N)
prop.table(table(des_a1$N))

# Write datasets
dataset_write = merge(des_a1[N>=2], 
                      df_describers[, c("full.name.of.describer.n", 
                                        "residence.country.describer.full"),
                      by="full.name.of.describer.n", all.x=T, all.y=F]


filename_write = paste0(dir_data, "eda0_sum/2019-10-28-taxonomist-country-res-morethanone.csv")
write.csv(dataset_write, filename_write)

filename_write = paste0(dir_data, "eda0_sum/2019-10-28-taxonomist-country-res-summary.csv")
write.csv(df_describers[, .N,by=c("residence.country.describer.first")][order(-N)],
          filename_write)  