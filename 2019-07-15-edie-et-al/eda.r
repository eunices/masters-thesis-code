source('2019-07-15-edie-et-al/init_e.r')

# Analyses
#############
theme <- theme_minimal()

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Time series
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Time series"))

# Individual plots

# Cumulative bee species across years
## All bees
df_year <- unique(df_country[,c("idx", "date.n")])
summary_year <- df_year[,.(.N), by="date.n"][order(date.n)]
template_year <- data.frame(date.n=min(summary_year$date.n):max(summary_year$date.n))
summary_year <- merge(template_year, summary_year, by="date.n", all.x=T, all.y=F)
summary_year[is.na(summary_year$N),]$N <- 0
summary_year$N_cumsum <- cumsum(summary_year$N)
plot_year_cumsum <- ggplot(data = summary_year, aes(x=date.n, y=N_cumsum)) + geom_point() + geom_line() + 
    xlab("Year") + ylab("Cumulative number of species")  + theme +
        ggtitle("Cumulative number of bee species time series (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))
plot_year <- ggplot(data = summary_year, aes(x=date.n, y=N)) + geom_point() + geom_line() + 
    xlab("Year") + ylab("Number of species described in a year")  + theme +
        ggtitle("Number of species described time series (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold")) +  geom_smooth()

## By family
df_country2 <- merge(df_country, df[,c("idx", "family")], by="idx", all.x=T, all.y=F)
df_family <- unique(df_country2[,c("idx", "date.n", "family")])
summary_year_fam <- df_family[,.(.N), by=c("date.n", "family")]
template_year_fam <- expand.grid(date.n=min(summary_year_fam$date.n):max(summary_year_fam$date.n),
                                 family=unique(df_country2$family))
summary_year_fam <- merge(template_year_fam, summary_year_fam,
                          by=c("date.n", "family"), all.x=T, all.y=F)
summary_year_fam[is.na(summary_year_fam$N),]$N <- 0
summary_year_fam <- data.table(summary_year_fam)
summary_year_fam[, N_cumsum := cumsum(N), by="family"]

plot_year_fam_cumsum <- ggplot(data = summary_year_fam, aes(x=date.n, y=N_cumsum)) 
plot_year_fam_cumsum1 <- plot_year_fam_cumsum + geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme +
        facet_wrap(. ~ family, ncol=3, scales = "free_y") +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))
plot_year_fam_cumsum2 <- plot_year_fam_cumsum + geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme +
        facet_wrap(. ~ family, ncol=3) +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold"))
plot_year_fam <- ggplot(data = summary_year_fam, aes(x=date.n, y=N)) + 
    geom_point() + geom_line() + 
        xlab("Year") + ylab("Number of species described in a year")  + theme +
            facet_wrap(. ~ family, ncol=3, scales="free_y") +
            ggtitle("Number of species described time series (1758-2018)") + 
                theme(plot.title = element_text(lineheight=.8, face="bold"))  +  geom_smooth()

## By tropics etc
get_first_type <- function(x) strsplit(x, "/")[[1]][1]
df_country2 <- df_country
df_country2$Latitude_type3 <- sapply(df_country2$Latitude_type, get_first_type)
df_trop <- unique(df_country2[,c("idx", "date.n", "Latitude_type3")])
summary_year_trop <- df_trop[,.(.N), by=c("date.n", "Latitude_type3")]
template_year_trop <- expand.grid(
        date.n=min(summary_year_fam$date.n):max(summary_year_fam$date.n),
        Latitude_type3=unique(df_country2$Latitude_type3))
summary_year_trop <- merge(template_year_trop, summary_year_trop,
                          by=c("date.n", "Latitude_type3"), all.x=T, all.y=F)
summary_year_trop[is.na(summary_year_trop$N),]$N <- 0
summary_year_trop <- data.table(summary_year_trop)
summary_year_trop[, N_cumsum := cumsum(N), by="Latitude_type3"]

plot_year_trop_cumsum <- 
    ggplot(data = summary_year_trop, 
           aes(x=date.n, y=N_cumsum, col=Latitude_type3)) +
    geom_point() + geom_line() +
        xlab("Year") + ylab("Cumulative number of species") + theme +
            ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
                theme(plot.title = element_text(lineheight=.8, face="bold"))
plot_year_trop <- ggplot(data = summary_year_trop, aes(x=date.n, y=N, col=Latitude_type3)) +
    # geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold")) + geom_smooth()

## By family and tropics/ not 
df_country2 <- merge(df_country, df[,c("idx", "family")], by="idx", all.x=T, all.y=F)
df_country2$Latitude_type3 <- sapply(df_country2$Latitude_type, get_first_type)
df_trop <- unique(df_country2[,c("idx", "date.n", "family", "Latitude_type3")])
summary_year_fam_trop <- df_trop[, .(.N), by=c("date.n", "family", "Latitude_type3")]
template_year_fam_trop <- expand.grid(
    date.n=min(summary_year_fam$date.n):max(summary_year_fam$date.n),
    family=unique(df_country2$family), Latitude_type3=unique(df_country2$Latitude_type3))
summary_year_fam_trop <- merge(template_year_fam_trop, summary_year_fam_trop,
                          by=c("date.n", "family", "Latitude_type3"), all.x=T, all.y=F)
summary_year_fam_trop[is.na(summary_year_fam_trop$N),]$N <- 0
summary_year_fam_trop <- data.table(summary_year_fam_trop)
summary_year_fam_trop[, N_cumsum := cumsum(N), by=c("family", "Latitude_type3")]

plot_year_fam_trop_cumsum <- 
    ggplot(data = summary_year_fam_trop, 
           aes(x=date.n, y=N_cumsum, col=Latitude_type3)) + geom_point() + geom_line() +
        xlab("Year") + ylab("Cumulative number of species") + theme +
            facet_wrap(. ~ family, ncol=3) +
            ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
                theme(plot.title = element_text(lineheight=.8, face="bold"))
plot_year_fam_trop <- ggplot(data = summary_year_fam_trop, aes(x=date.n, y=N, 
                          col=Latitude_type3)) +
    # geom_point() + geom_line() +
    xlab("Year") + ylab("Cumulative number of species") + theme +
        facet_wrap(. ~ family, ncol=3, scales = "free_y") +
        ggtitle("Cumulative number of bee species time series for each family (1758-2018)") + 
             theme(plot.title = element_text(lineheight=.8, face="bold")) + geom_smooth()

# Combined plots
plot_year_cumsum
plot_year

plot_year_fam_cumsum1
plot_year_fam_cumsum2
plot_year_fam

plot_year_trop_cumsum
plot_year_trop

plot_year_fam_trop_cumsum
plot_year_fam_trop

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Taxonomic effort - taxonomists
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Taxonomic effort - taxonomists"))

# Individual plots

# Taxonomic effort
plot_tax_effort1a <- 
    ggplot(data=taxonomic_effort, aes(x=years, y=N_describers)) +
        geom_point() + geom_line() + theme +
        xlab("Year") + ylab("Number of \nactive taxonomists") + 
                    ggtitle("Number of active taxonomists in each year")
plot_tax_effort2a <- 
    ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_describers)) +
        geom_point() + geom_line() + theme +
        xlab("Year") + ylab("Number of active taxonomists, \nweighted by mean number of species described") + 
                    ggtitle("Number of active taxonomists in each year")

# Taxonomic effort (exclude authors w no first author)
plot_tax_effort1b <- 
    ggplot(data=taxonomic_effort, aes(x=years, y=N_real_describers)) +
        geom_point() + geom_line() + theme +
        xlab("Year") + ylab("Number of \nactive taxonomists") + 
                    ggtitle("Number of active taxonomists in each year")
plot_tax_effort2b <- 
    ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_real_describers)) +
        geom_point() + geom_line() + theme +
        xlab("Year") + ylab("Number of active taxonomists, \nweighted by mean number of species described") + 
                    ggtitle("Number of active taxonomists in each year")

# Time series of all taxonomists and real taxonomists on the same plot
types <- c("N_describers", "N_real_describers")
plot_tax_effort3 <- ggplot(data=taxonomic_effort_long[type %in% types], aes(x=years, y=N, col=type)) +
    geom_point() + geom_line() + theme +
       xlab("Year") + ylab("Number of \nactive taxonomists") + 
                ggtitle("Number of active taxonomists in each year")

# Number of authors by century/decade
df_species_describers_sum$century <- as.numeric(sub("(\\d{2}).*", "\\1", df_species_describers_sum$date.n))
df_species_describers_sum$decade <- as.numeric(sub("(\\d{3}).*", "\\1", df_species_describers_sum$date.n))
ggplot(df_species_describers_sum, aes(x=as.character(century), y=N_authors)) + 
  geom_boxplot() + theme
df_species_describers_sum[,list(min=min(N_authors),
                                quartile_1st=quantile(N_authors, 0.25),
                                median=median(as.numeric(N_authors)),
                                quartile_2nd=quantile(N_authors, 0.75),
                                max=max(N_authors)), by="century"][order(century)]

# Not weighted
rsq <- round(cor(taxonomic_effort$N_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort3a <- 
    ggplot(data=taxonomic_effort, aes(x=N_describers, y=N_species_described)) +
        geom_point() + theme +
        xlab("Number of active taxonomists") + ylab("Number of species described") + 
                    ggtitle(paste0("Number of active taxonomists in each year (Rsq = ", rsq, ")"))

# Weighted
rsq <- round(cor(taxonomic_effort$N_weighted_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort4a <- 
    ggplot(data=taxonomic_effort, aes(x=N_weighted_describers, y=N_species_described)) +
        geom_point() + theme +
        xlab("Number of active taxonomists") + ylab("Number of species described") + 
                    ggtitle(paste0("Number of active taxonomists in each year, \nweighted by mean no. of species described per taxonomist (Rsq = ", rsq, ")"))

# Not weighted
rsq <- round(cor(taxonomic_effort$N_real_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort3b <- 
    ggplot(data=taxonomic_effort, aes(x=N_real_describers, y=N_species_described)) +
        geom_point() + theme +
        xlab("Number of active taxonomists") + ylab("Number of species described") + 
                    ggtitle(paste0("Number of active taxonomists in each year (Rsq = ", rsq, ")"))

# Weighted
rsq <- round(cor(taxonomic_effort$N_weighted_real_describers, taxonomic_effort$N_species_described)^2,2)
plot_tax_effort4b <- 
    ggplot(data=taxonomic_effort, aes(x=N_weighted_real_describers, y=N_species_described)) +
        geom_point() + theme +
        xlab("Number of active taxonomists") + ylab("Number of species described") + 
                    ggtitle(paste0("Number of active taxonomists in each year, \nweighted by mean no. of species described per taxonomist (Rsq = ", rsq, ")"))

# Combined plots
grid.arrange(plot_tax_effort1a, plot_tax_effort2a, ncol=1)
grid.arrange(plot_tax_effort1b, plot_tax_effort2b, ncol=1)
plot_tax_effort3
grid.arrange(plot_tax_effort3a, plot_tax_effort4a, nrow=1)
grid.arrange(plot_tax_effort3b, plot_tax_effort4b, nrow=1)


# Tabulate statistics of years active
tax <- df_describers[spp_N_1st_auth_s>=1]

# Active based on max_corrected
yrs_active1 <- tax$max_corrected - tax$min + 1
tax[yrs_active1 <=0]
yrs_active1[yrs_active1<=0] <- 0
summary(yrs_active1)
# tax[yrs_active1>30]$full.name.of.describer

# Active based on max_correct and with date of death
yrs_active1b <- tax[!is.na(dod.describer.n),]
yrs_active1b <- yrs_active1b$max_corrected - yrs_active1b$min + 1
summary(yrs_active1b)

hist_active_yrs <- ggplot(data.frame(yrs=yrs_active1b)) +
    geom_histogram(data.frame(yrs=yrs_active1), 
                   mapping=aes(x=yrs, y=..count../sum(..count..) * 100),
                   binwidth=10, fill='black') + 
    geom_histogram(aes(x=yrs, y=..count../sum(..count..) * 100), binwidth=10, 
                   fill='darkorange', alpha=0.6) +
    scale_x_continuous(breaks= seq(0, max(yrs_active1), 10)) +
    geom_vline(xintercept=9, color='grey', size=1) +
    geom_vline(xintercept=summary(yrs_active1)[5], color='grey', size=1, linetype="dashed") +
    geom_vline(xintercept=summary(yrs_active1b)[5], color='darkorange', size=1, linetype="dashed") +
    xlab("\nNumber of active years") + ylab("Proportion of taxonomists (%)\n") + 
    theme

# Active based on max publication dates
yrs_active2 <- tax$max - tax$min

# Based on sum of authors that described a species
yrs_active3 <- df[, c("date.n", "full.name.of.describer")] %>% 
    separate_rows(full.name.of.describer, sep="; ")
yrs_active3 <- unique(yrs_active3)
yrs_active3 <- yrs_active3[, .N, by='date.n'][order(date.n)]
plot_tax_effort5 <- 
    ggplot(data=yrs_active3, aes(x=date.n, y=N)) +
        geom_line() + theme +
        xlab("Number of taxonomists") + ylab("Year") + 
                    ggtitle(paste0("Number of taxonomists in each year"))

# Mean number of species/year 
hist_mean_sp_per_auth <- ggplot(df_describers) +
    geom_histogram(mapping=aes(x=ns_species_per_year_active, y=..count../sum(..count..) * 100),
                   binwidth=5) + theme

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Taxonomic effort - publications
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Taxonomic effort - publications"))

# Individual plots

## Mean number of species per publication by decade
median_N_spp_per_pub_decade <- df_publications_N[, list(mean_N_spp=median(n_species)), 
                                               by="date.decade"]


calc_median <- function(x){
  return(c(y = -5, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

plot_tax_pub_decade <- 
    ggplot(data=df_publications_N, aes(x=date.decade, y=n_species)) +
        geom_violin(width=3, fill="bisque") + 
        geom_boxplot(width=0.2, outlier.size=NULL, fill="white") +
        geom_jitter(shape=16, position=position_jitter(0.2), 
                    size=0.1, alpha=0.2, color='grey10') +
        # geom_line(data=median_N_spp_per_pub_decade, 
        #           mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
        #           size=1.3, color="red", alpha=0.9, linetype='dashed') +
        geom_smooth(data=median_N_spp_per_pub_decade[date.decade != "1750s"], 
                    mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
                    size=1.3) +
        scale_y_continuous(limit=c(-5, 40)) +
        stat_summary(fun.data = calc_median, geom = "text", fun.y = median,
                     position = position_dodge(width = 0.75)) +
        xlab("\nDecade") + ylab("Number of species described \nper publication\n") +
        theme

# Combined plots
plot_tax_pub_decade
plot_tax_pub_yr

