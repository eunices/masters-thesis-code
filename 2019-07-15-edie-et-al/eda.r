source('2019-07-15-edie-et-al/init.r')

# Libraries
library(ggplot2)
library(grid); library(gridExtra)

# Analyses
# Cumulative bee species across years

## All bees
summary_year <- df_country[,.(.N), by="date.n"][order(date.n)]
template_year <- data.frame(date.n=min(summary_year$date.n):max(summary_year$date.n))
summary_year <- merge(template_year, summary_year, by="date.n", all.x=T, all.y=F)
summary_year[is.na(summary_year$N),]$N <- 0
summary_year$N_cumsum <- cumsum(summary_year$N)
plot_year <- ggplot(data = summary_year, aes(x=date.n, y=N_cumsum)) 
plot_year + geom_point() + geom_line() + 
    xlab("Year") + ylab("Cumulative number of species")  + theme_minimal() +
        ggtitle("Cumulative number of bee species time series (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))

## By family
df_country2 <- merge(df_country, df[,c("idx", "family")], by="idx", all.x=T, all.y=F)
summary_year_fam <- df_country2[,.(.N), by=c("date.n", "family")]
template_year_fam <- expand.grid(date.n=min(summary_year_fam$date.n):max(summary_year_fam$date.n),
                                 family=unique(df_country2$family))
summary_year_fam <- merge(template_year_fam, summary_year_fam,
                          by=c("date.n", "family"), all.x=T, all.y=F)
summary_year_fam[is.na(summary_year_fam$N),]$N <- 0
summary_year_fam <- data.table(summary_year_fam)
summary_year_fam[, N_cumsum := cumsum(N), by="family"]

plot_year_fam <- ggplot(data = summary_year_fam, aes(x=date.n, y=N_cumsum, fill=family)) 
plot_year_fam + geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme_minimal() +
        facet_wrap(. ~ family, ncol=3, scales = "free_y") +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))
plot_year_fam + geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme_minimal() +
        facet_wrap(. ~ family, ncol=3) +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))

# Map by country
summary_country <- df_country[,.(.N), by="A.3"][order(A.3)]
shp_gadm_0_bee <- merge(shp_gadm_0, summary_country, by.x="GID_0", by.y="A.3")
map_country <- ggplot(data = shp_gadm_0_bee) 
map_country + geom_sf(aes(fill=N)) + theme_minimal() +
    ggtitle("Map of observed species richness by country") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))

# Taxonomic effort
plot_tax_effort1 <- ggplot(data=taxonomic_effort, aes(x=years, y=N_describers)) +
    geom_point() + geom_line() + theme_minimal() +
       xlab("Year") + ylab("Number of \nactive taxonomists") + 
                ggtitle("Number of active taxonomists in each year")

plot_tax_effort2 <- ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_describers)) +
    geom_point() + geom_line() + theme_minimal() +
       xlab("Year") + ylab("Number of active taxonomists, \nweighted by mean number of species described") + 
                ggtitle("Number of active taxonomists in each year")

grid.arrange(plot_tax_effort1, plot_tax_effort2, ncol=1)

# Not weighted
rsq <- round(cor(taxonomic_effort$N_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort3 <- ggplot(data=taxonomic_effort, aes(x=N_describers, y=N_species_described)) +
    geom_point() + theme_minimal() +
       xlab("Number of active taxonomists") + ylab("Number of species described") + 
                ggtitle(paste0("Number of active taxonomists in each year (Rsq = ", rsq, ")"))

# Weighted
rsq <- round(cor(taxonomic_effort$N_weighted_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort4 <- ggplot(data=taxonomic_effort, aes(x=N_weighted_describers, y=N_species_described)) +
    geom_point() + theme_minimal() +
       xlab("Number of active taxonomists") + ylab("Number of species described") + 
                ggtitle(paste0("Number of active taxonomists in each year, \n weighted by mean no. of species described per taxonomist (Rsq = ", rsq, ")"))

grid.arrange(plot_tax_effort3, plot_tax_effort4, nrow=1)


#################################
# Not related to main analysis
#################################

# Dirth of bee taxonomist

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
ggplot(df_species_describers, aes(N_authors)) + 
    geom_histogram(binwidth=1)
table(df_species_describers$N_authors)

# Were the greatest taxonomists always first authors? 
## Last authors
cor(df_describers$num_species_described, df_describers$prop_species_described_as_last_author)^2
cor(df_describers[num_species_described<500]$num_species_described, df_describers[num_species_described<500]$prop_species_described_as_last_author)^2

ggplot(df_describers[num_species_described<500], aes(num_species_described, prop_species_described_as_last_author)) +
    geom_point() + theme_minimal()
ggplot(df_describers, aes(num_species_described, prop_species_described_as_last_author)) +
    geom_point() + theme_minimal()

## Not as first authors
cor(df_describers$num_species_described, df_describers$prop_species_described_not_as_first_author)^2
cor(df_describers[num_species_described<500]$num_species_described, df_describers[num_species_described<500]$prop_species_described_not_as_first_author)^2

ggplot(df_describers[num_species_described<500], aes(num_species_described, prop_species_described_not_as_first_author)) +
    geom_point() + theme_minimal()
ggplot(df_describers, aes(num_species_described, prop_species_described_not_as_first_author)) +
    geom_point() + theme_minimal()
# Does not appear to be a correlation when a continuous variable

# But probably there are some who are always last authors for all
df_describers[order(-prop_species_described_as_last_author)][,c("full.name.of.describer.n", "prop_species_described_as_last_author")]

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
