# Set up
source('2020-08-31-jsa-type-ch1/init.r')

# Parameters
theme = theme_minimal()
dir_plot = "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\_figures\\_ch2\\"


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 1 time series of species description
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Fig. 1 time series of species description"))

species_per_year <- df[,.(.N), by=.(date.n)][order(date.n)]
template_year <- data.frame(date.n=min(species_per_year$date.n):max(species_per_year$date.n))
species_per_year <- merge(template_year, species_per_year, by="date.n", all.x=T, all.y=F)
species_per_year[is.na(species_per_year$N),]$N <- 0
species_per_year$N_cumsum <- cumsum(species_per_year$N)

species_per_year2 <- melt(species_per_year, "date.n", stringsAsFactors=F)
species_per_year2$variable <- factor(species_per_year2$variable, c("N_cumsum", "N"))
pt_sum <- data.table(species_per_year2)[, list(max=max(value)), by=c("variable")]
pts <- data.frame(date.n=rep(c(1914, 1919, 1939, 1945), 2), 
                  variable=c(rep("N_cumsum", 4), rep("N", 4)),
                  value=c(rep(pt_sum[variable=="N_cumsum",]$max, 4), 
                  rep(pt_sum[variable=="N",]$max, 4)))

labs <- c(`N` = "N species",
          `N_cumsum` = "Cumulative N species")
p1 <- ggplot(species_per_year2, aes(x=date.n, y=value)) + 
    facet_wrap(.~variable, nrow=2, scales = "free_y", labeller= as_labeller(labs)) +
    geom_ribbon(pts[c(1,2,5,6),], mapping=aes(x=date.n, ymin=0, ymax=value), fill="red", alpha=0.2) +
    geom_ribbon(pts[c(1,2,5,6),], mapping=aes(x=date.n, ymin=0, ymax=value), fill="red", alpha=0.2) +
    geom_ribbon(pts[c(3,4,7,8),], mapping=aes(x=date.n, ymin=0, ymax=value), fill="red", alpha=0.2) +
    geom_ribbon(pts[c(3,4,7,8),], mapping=aes(x=date.n, ymin=0, ymax=value), fill="red", alpha=0.2) +
    geom_line(size=1) + geom_smooth() +
        xlab("") + ylab("") +
            theme

ggsave(paste0(dir_plot, 'fig-1.png'), p1, units="cm", width=21, height=10, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig 4. Species richness and area graph
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Species richness and area graph"))
area <- read.csv('data/lookup/2019-12-20-richness-area.csv')

p4 = ggplot(area) + 
    geom_point(aes(x=log(area), y=log(richness))) +
    stat_smooth(area, mapping=aes(x=log(area), y=log(richness)), method='lm', formula = y~x) +
    xlab("log(Area (million sq km))") + ylab("log(N species discovered)") +
    scale_y_continuous(limits=c(2, 10)) + 
    theme

ggsave(paste0(dir_plot, 'fig-4.png'), p4, units="cm", width=10, height=10, dpi=300)

summary(lm(log(richness) ~ log(area), area))



######## EXTRA EDA ########

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


