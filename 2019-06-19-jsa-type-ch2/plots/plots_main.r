# Set up
source('2019-06-19-jsa-type-ch1/init.r')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Species richness and area graph
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Species richness and area graph"))
area <- read.csv('data/lookup/2019-12-20-richness-area.csv')

ggplot(area) + 
    geom_point(aes(x=log(area), y=log(richness))) +
    stat_smooth(area, mapping=aes(x=log(area), y=log(richness)), method='lm', formula = y~x) +
    xlab("log(Area (million sq km))") + ylab("log(N species discovered)") +
    scale_y_continuous(limits=c(2, 10)) + 
    theme

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


