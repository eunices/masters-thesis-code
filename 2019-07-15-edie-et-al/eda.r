source('2019-07-15-edie-et-al/init.r')

# Libraries
library(ggplot2)


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
