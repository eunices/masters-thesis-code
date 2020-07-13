print(paste0(Sys.time(), " --- Fig. 1, S2"))

# Individual plots

species_per_year <- df[,.(.N), by=.(date.n)][order(date.n)]
template_year <- data.frame(date.n=min(species_per_year$date.n):max(species_per_year$date.n))
species_per_year <- merge(template_year, species_per_year, by="date.n", all.x=T, all.y=F)
species_per_year[is.na(species_per_year$N),]$N <- 0
species_per_year$N_cumsum <- cumsum(species_per_year$N)

# World War I 1914-1919; World War II 1939-1945 
pts <- data.frame(date.n=as.integer(c(1914, 1919, 
                                      1939, 1945)))


# Species

# Per year

p0 <- ggplot(species_per_year, aes(x=date.n, y=N)) + 
    xlab("") + ylab("Number of species") + 
    theme +
    ggtitle("Number of species described by year") +
    annotate("rect", xmin=pts[1,], xmax=pts[2,], ymin=0, ymax=Inf, fill="red", alpha=0.2) +
    annotate("rect", xmin=pts[3,], xmax=pts[4,], ymin=0, ymax=Inf, fill="red", alpha=0.2) +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(species_per_year$N, 10, fill = list(NA, NULL, NA))) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks100, minor_breaks=ybreaks20)


# Per decade

species_per_decade <- df[,.(.N), by=.(date.decade)]
p2 <- ggplot(species_per_decade, aes(x=date.decade, y=N)) + 
    geom_bar(stat="identity") + 
        xlab("") + ylab("Number of species") + 
            theme +
                ggtitle("Number of species described  by decade")

# These are not plotted.




# These are plotted.

# Publications

# Per year

publications_per_year <- df_publications[,.(.N), by=.(date.n)][order(date.n)]
p3 <- ggplot(publications_per_year, aes(x=date.n, y=N)) + 
    xlab("") + ylab("Number of publications\n") + 
    theme +
    # ggtitle("Number of publications by year") +
    # ggtitle("A") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(publications_per_year$N, 10, fill = list(NA, NULL, NA))) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10)


# Per year

p8 <- ggplot(taxonomic_effort, aes(x=years, y=N_real_describers)) + 
    xlab("") + ylab("Number of PTEs\n") + 
    theme +
    # ggtitle("Number of PTEs by year") + 
    # ggtitle("B") + 
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(taxonomic_effort$N_real_describers, 10, 
              fill = list(NA, NULL, NA))) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10)


## Species per author across years
p15 <- ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_real_describers)) +
    xlab("Year") + ylab("Number of PTEs (wted)\n") + theme +
    # ggtitle("Number of PTEs (wted) by year") +
    # ggtitle("C") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(taxonomic_effort$N_weighted_real_describers, 10, 
              fill = list(NA, NULL, NA))) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10)
              

ggsave(paste0(dir_plot, 'fig-1a.png'), p3, units="cm", width=16, height=4, dpi=300)
ggsave(paste0(dir_plot, 'fig-1b.png'), p8, units="cm", width=16, height=4, dpi=300)
ggsave(paste0(dir_plot, 'fig-1c.png'), p15, units="cm", width=16, height=4, dpi=300)


