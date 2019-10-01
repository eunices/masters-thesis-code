source('2019-07-15-edie-et-al/init.r')

# Libraries
#############
library(tidyr)
library(ggplot2)
library(grid); library(gridExtra)

# Analyses
#############


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - TEST
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- TEST"))

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
plot_year <- ggplot(data = summary_year, aes(x=date.n, y=N)) 
plot_year + geom_point() + geom_line() + 
    xlab("Year") + ylab("Number of species described in a year")  + theme_minimal() +
        ggtitle("Number of species described time series (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold")) +  geom_smooth()


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
plot_year_fam <- ggplot(data = summary_year_fam, aes(x=date.n, y=N, c=family)) 
plot_year_fam + geom_point() + geom_line() + 
    xlab("Year") + ylab("Number of species described in a year")  + theme_minimal() +
        facet_wrap(. ~ family, ncol=3, scales="free_y") +
        ggtitle("Number of species described time series (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))  +  geom_smooth()



## By tropics etc
get_first_type <- function(x) {
    strsplit(x, "/")[[1]][1]
}
df_country2 <- df_country
df_country2$Latitude_type3 <- sapply(df_country2$Latitude_type, get_first_type)

summary_year_trop <- df_country2[,.(.N), by=c("date.n", "Latitude_type3")]
template_year_trop <- expand.grid(
        date.n=min(summary_year_fam$date.n):max(summary_year_fam$date.n),
        Latitude_type3=unique(df_country2$Latitude_type3))
summary_year_trop <- merge(template_year_trop, summary_year_trop,
                          by=c("date.n", "Latitude_type3"), all.x=T, all.y=F)
summary_year_trop[is.na(summary_year_trop$N),]$N <- 0
summary_year_trop <- data.table(summary_year_trop)
summary_year_trop[, N_cumsum := cumsum(N), by="Latitude_type3"]

plot_year_fam <- ggplot(data = summary_year_trop, aes(x=date.n, y=N_cumsum, col=Latitude_type3)) 
plot_year_fam + geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme_minimal() +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))

plot_year_fam <- ggplot(data = summary_year_trop, aes(x=date.n, y=N, col=Latitude_type3)) 
plot_year_fam + 
    # geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme_minimal() +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold")) + geom_smooth()

## By family and tropics/ not 
df_country2 <- merge(df_country, df[,c("idx", "family")], by="idx", all.x=T, all.y=F)
df_country2$Latitude_type3 <- sapply(df_country2$Latitude_type, get_first_type)
summary_year_fam_trop <- df_country2[,.(.N), by=c("date.n", "family", "Latitude_type3")]
template_year_fam_trop <- expand.grid(
    date.n=min(summary_year_fam$date.n):max(summary_year_fam$date.n),
    family=unique(df_country2$family), Latitude_type3=unique(df_country2$Latitude_type3))
summary_year_fam_trop <- merge(template_year_fam_trop, summary_year_fam_trop,
                          by=c("date.n", "family", "Latitude_type3"), all.x=T, all.y=F)
summary_year_fam_trop[is.na(summary_year_fam_trop$N),]$N <- 0
summary_year_fam_trop <- data.table(summary_year_fam_trop)
summary_year_fam_trop[, N_cumsum := cumsum(N), by=c("family", "Latitude_type3")]

plot_year_fam_trop <- ggplot(data = summary_year_fam_trop, aes(x=date.n, y=N_cumsum, 
                          col=Latitude_type3)) 
plot_year_fam_trop + geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme_minimal() +
        facet_wrap(. ~ family, ncol=3, scales = "free_y") +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))
plot_year_fam_trop <- ggplot(data = summary_year_fam_trop, aes(x=date.n, y=N, 
                          col=Latitude_type3)) 
plot_year_fam_trop + 
    # geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme_minimal() +
        facet_wrap(. ~ family, ncol=3, scales = "free_y") +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold")) + geom_smooth()

# # Map by country
# summary_country <- df_country[,.(.N), by="A.3"][order(A.3)]
# shp_gadm_0_bee <- merge(shp_gadm_0, summary_country, by.x="GID_0", by.y="A.3")
# map_country <- ggplot(data = shp_gadm_0_bee) 
# map_country + geom_sf(aes(fill=N)) + theme_minimal() +
#     ggtitle("Map of observed species richness by country") + 
#              theme(plot.title = element_text(lineheight=.8, face="bold"))

# Taxonomic effort
plot_tax_effort1a <- ggplot(data=taxonomic_effort, aes(x=years, y=N_describers)) +
    geom_point() + geom_line() + theme_minimal() +
       xlab("Year") + ylab("Number of \nactive taxonomists") + 
                ggtitle("Number of active taxonomists in each year")

plot_tax_effort2a <- ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_describers)) +
    geom_point() + geom_line() + theme_minimal() +
       xlab("Year") + ylab("Number of active taxonomists, \nweighted by mean number of species described") + 
                ggtitle("Number of active taxonomists in each year")

grid.arrange(plot_tax_effort1a, plot_tax_effort2a, ncol=1)

# Taxonomic effort (exclude authors w no first author)
plot_tax_effort1b <- ggplot(data=taxonomic_effort, aes(x=years, y=N_real_describers)) +
    geom_point() + geom_line() + theme_minimal() +
       xlab("Year") + ylab("Number of \nactive taxonomists") + 
                ggtitle("Number of active taxonomists in each year")

plot_tax_effort2b <- ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_real_describers)) +
    geom_point() + geom_line() + theme_minimal() +
       xlab("Year") + ylab("Number of active taxonomists, \nweighted by mean number of species described") + 
                ggtitle("Number of active taxonomists in each year")

grid.arrange(plot_tax_effort1b, plot_tax_effort2b, ncol=1)

# Time series of all taxonomists and real taxonomists on the same plot
types <- c("N_describers", "N_real_describers")
plot_tax_effort3 <- ggplot(data=taxonomic_effort_long[type %in% types], aes(x=years, y=N, col=type)) +
    geom_point() + geom_line() + theme_minimal() +
       xlab("Year") + ylab("Number of \nactive taxonomists") + 
                ggtitle("Number of active taxonomists in each year")
plot_tax_effort3

# Number of authors by century/decade
df_species_describers_sum$century <- as.numeric(sub("(\\d{2}).*", "\\1", df_species_describers_sum$date.n))
df_species_describers_sum$decade <- as.numeric(sub("(\\d{3}).*", "\\1", df_species_describers_sum$date.n))
ggplot(df_species_describers_sum, aes(x=as.character(century), y=N_authors)) + 
  geom_boxplot() + theme_minimal()
df_species_describers_sum[,list(min=min(N_authors),
                                quartile_1st=quantile(N_authors, 0.25),
                                median=median(as.numeric(N_authors)),
                                quartile_2nd=quantile(N_authors, 0.75),
                                max=max(N_authors)), by="century"][order(century)]

# Not weighted
rsq <- round(cor(taxonomic_effort$N_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort3a <- ggplot(data=taxonomic_effort, aes(x=N_describers, y=N_species_described)) +
    geom_point() + theme_minimal() +
       xlab("Number of active taxonomists") + ylab("Number of species described") + 
                ggtitle(paste0("Number of active taxonomists in each year (Rsq = ", rsq, ")"))

# Weighted
rsq <- round(cor(taxonomic_effort$N_weighted_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort4a <- ggplot(data=taxonomic_effort, aes(x=N_weighted_describers, y=N_species_described)) +
    geom_point() + theme_minimal() +
       xlab("Number of active taxonomists") + ylab("Number of species described") + 
                ggtitle(paste0("Number of active taxonomists in each year, \nweighted by mean no. of species described per taxonomist (Rsq = ", rsq, ")"))

grid.arrange(plot_tax_effort3a, plot_tax_effort4a, nrow=1)

# Not weighted
rsq <- round(cor(taxonomic_effort$N_real_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort3b <- ggplot(data=taxonomic_effort, aes(x=N_real_describers, y=N_species_described)) +
    geom_point() + theme_minimal() +
       xlab("Number of active taxonomists") + ylab("Number of species described") + 
                ggtitle(paste0("Number of active taxonomists in each year (Rsq = ", rsq, ")"))

# Weighted
rsq <- round(cor(taxonomic_effort$N_weighted_real_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort4b <- ggplot(data=taxonomic_effort, aes(x=N_weighted_real_describers, y=N_species_described)) +
    geom_point() + theme_minimal() +
       xlab("Number of active taxonomists") + ylab("Number of species described") + 
                ggtitle(paste0("Number of active taxonomists in each year, \nweighted by mean no. of species described per taxonomist (Rsq = ", rsq, ")"))

grid.arrange(plot_tax_effort3b, plot_tax_effort4b, nrow=1)

