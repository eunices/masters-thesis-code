print(paste0(Sys.time(), " --- Fig. 1, S2"))

################################################################################

# Datasets

# Species
species_per_year <- df[,.(.N), by=.(date)][order(date)]

template_year <- data.frame(
    date=min(species_per_year$date):max(species_per_year$date)
)

species_per_year <- merge(
    template_year, species_per_year, 
    by="date", all.x=T, all.y=F
)

species_per_year[is.na(species_per_year$N),]$N <- 0
species_per_year$N_cumsum <- cumsum(species_per_year$N)

species_per_year$N_roll <- rollmean(
    species_per_year$N, 10, fill = list(NA, NULL, NA)
)


# Publications
publications_per_year <- df_publications[,.(.N), by=.(date)][order(date)]
rng <- range(publications_per_year$date)

publications_per_year <- data.table(merge(
        data.frame(date=seq(rng[1], rng[2], 1)), publications_per_year, 
        by='date', all.x=T, all.y=F
))

publications_per_year[is.na(N)]$N <- 0

publications_per_year$N_roll <- rollmean(
    publications_per_year$N, 10, fill = list(NA, NULL, NA)
)

# World War I 1914-1919; World War II 1939-1945 
pts <- data.frame(date=as.integer(c(1914, 1919, 1939, 1945)))


################################################################################

# Unsaved plots

# Species

# Per year

p0 <- ggplot(species_per_year, aes(x=date, y=N)) + 
    xlab("\nYear") + ylab("Number of species") + 
    theme +
    ggtitle("Number of species described by year") +
    annotate("rect", xmin=pts[1,], xmax=pts[2,], ymin=0, ymax=Inf, fill="red", alpha=0.2) +
    annotate("rect", xmin=pts[3,], xmax=pts[4,], ymin=0, ymax=Inf, fill="red", alpha=0.2) +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, aes(y=N_roll)) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks100, minor_breaks=ybreaks20)


# Per decade

species_per_decade <- df[,.(.N), by=.(date.decade)]
p2 <- ggplot(species_per_decade, aes(x=date.decade, y=N)) + 
    geom_bar(stat="identity") + 
    xlab("\nYear") + ylab("Number of species") + 
    theme +
    ggtitle("Number of species described  by decade")

# These are not saved.


################################################################################

# Saved figures


# Publications

# Per year

p3 <- ggplot(publications_per_year, aes(x=date, y=N)) + 
    xlab("\nYear") + ylab("Number of \npublications\n") + 
    theme +
    # ggtitle("Number of publications by year") +
    # ggtitle("A") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, aes(y=N_roll)) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    annotate("rect", xmin=pts[1,], xmax=pts[2,], ymin=0, ymax=Inf, fill="red", alpha=0.2) +
    annotate("rect", xmin=pts[3,], xmax=pts[4,], ymin=0, ymax=Inf, fill="red", alpha=0.2)


# Per year

p8 <- ggplot(taxonomic_effort, aes(x=years, y=N_real_describers)) + 
    xlab("\nYear") + ylab("Number of \nPTEs\n") + 
    theme +
    # ggtitle("Number of PTEs by year") + 
    # ggtitle("B") + 
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, aes(y=N_real_describers_roll)) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    annotate("rect", xmin=pts[1,], xmax=pts[2,], ymin=0, ymax=Inf, fill="red", alpha=0.2) +
    annotate("rect", xmin=pts[3,], xmax=pts[4,], ymin=0, ymax=Inf, fill="red", alpha=0.2)


## Species per author across years
p15 <- ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_real_describers)) +
    xlab("\nYear") + ylab("Number of \nPTEs (wted)\n") + theme +
    # ggtitle("Number of PTEs (wted) by year") +
    # ggtitle("C") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, aes(y=N_weighted_real_describers_roll)) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) + 
    annotate("rect", xmin=pts[1,], xmax=pts[2,], ymin=0, ymax=Inf, fill="red", alpha=0.2) +
    annotate("rect", xmin=pts[3,], xmax=pts[4,], ymin=0, ymax=Inf, fill="red", alpha=0.2)
              

ggsave(paste0(dir_plot, 'fig-1a.png'), p3, units="cm", width=16, height=4, dpi=300)
ggsave(paste0(dir_plot, 'fig-1b.png'), p8, units="cm", width=16, height=4, dpi=300)
ggsave(paste0(dir_plot, 'fig-1c.png'), p15, units="cm", width=16, height=4, dpi=300)


################################################################################

# Trend analysis

# Resources:
# http://r-statistics.co/Time-Series-Analysis-With-R.html

# publications_per_year$N
# taxonomic_effort$N_real_describers
# taxonomic_effort$N_weighted_real_describers

# Create time series data

ts_data_pub <- ts(publications_per_year$N_roll, frequency = 1, start = c(1758))

ts_data_des <- ts(
    taxonomic_effort$N_real_describers_roll, frequency = 1, start = c(1758)
)
ts_data_des_real <- ts(
    taxonomic_effort$N_weighted_real_describers_roll, 
    frequency = 1, start = c(1758)
)

# # Time series decomposition
# ts_data <- ts_data_pub[!is.na(ts_data_pub)]

# # Autocorrelation
# acf(ts_data)
# pacf(ts_data)

# # Decomopose
# ts_decomposed <- decompose(ts_data, type="multiplicative") 

# # Detrend
# tr_model <- lm(ts_data ~ c(1:length(ts_data)))
# plot(resid(tr_model), type="l")  

# # De-seasonalize
# ts_stl <- stl(ts_data, "periodic")  # decompose the TS
# ts_sa <- seasadj(ts_stl)            # de-seasonalize


# # Stationary test
# adf.test(ts_data)    # p>.05 means not stationery
# kpss.test(ts_data)   # p>.05 means not stationery

# # Differencing
# nsdiffs(ts_data)  # number for seasonal differencing needed
# ndiffs(ts_data)   # number for differencing

# ts_data_diff <- diff(ts_data, differences= 1)


# # Decomposition with stl
# stl(ts_data)

################################################################################

# Test for trend
res <- MannKendall(ts_data_pub)
print("--------------------------")
print("Trend test: publications")
print(summary(res))

res <- MannKendall(ts_data_des)
print("--------------------------")
print("Trend test: real describers")
print(summary(res))

res <- MannKendall(ts_data_des_real)
print("--------------------------")
print("Trend test: describers with productivities")
print(summary(res))

# res <- MannKendall(ts_data_diff)
# print("--------------------------")
# print("Trend test: diff of publications")
# print(summary(res))
