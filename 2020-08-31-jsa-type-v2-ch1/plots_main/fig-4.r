print(paste0(Sys.time(), " --- Catch per effort graph"))

################################################################################

# Plots

species_and_pub_per_year <- df_publications_N[, 
    list(
        species_per_publication=mean(n_species),
        N_publications=length(n_species),
        N_species=sum(n_species)
    ), by="date"][order(date)]

rng <- range(species_and_pub_per_year$date)

species_and_pub_per_year <- data.table(merge(
    data.frame(date=seq(rng[1], rng[2])), species_and_pub_per_year,
    by="date", all.x=T, all.y=F
))

species_and_pub_per_year[is.na(species_and_pub_per_year)] <- 0

species_and_pub_per_year$species_per_publication_roll <-
    rollmean(species_and_pub_per_year$species_per_publication, 10, 
             fill = list(NA, NULL, NA))

p5 <- ggplot(species_and_pub_per_year, aes(x=date, y=species_per_publication)) + 
    xlab("") + ylab("Number of species/ publication") + 
    theme +
    ggtitle("") +
    # ggtitle("Number of species/ publication by year") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_smooth(fill=NA, color='black', size=1.5) +
    geom_line(size=1, aes(y=species_per_publication_roll), color='grey50') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks20, minor_breaks=ybreaks5)

bp_year = 1910; y=4.1
# y = taxonomic_effort[which(years==bp_year)]$species_per_real_taxonomist
p13 <- ggplot(data=taxonomic_effort, aes(x=years, y=species_per_real_taxonomist)) +
    xlab("Year") + ylab("Number of species/ PTE") + 
    ggtitle("") +
    # ggtitle("Number of species described/ PTE by year") + 
    theme + 
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_smooth(fill=NA, color='black', size=1.5) +
    geom_line(size=1, aes(y=species_per_real_taxonomist_roll), color='grey50') +
    annotate(
        geom='curve', x=bp_year+30, y=round(y+4,0), xend=bp_year, yend=y,
        curvature=.1, arrow=arrow(length=unit(1, 'mm')), size=1, color='red'
    ) + 
    annotate(
        geom='text', hjust='left', x=bp_year+33, y=round(y+4,0)+.2, 
        label='Break point', size=4, color='red'
    ) + 
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks2, minor_breaks=ybreaks1, limits=c(0,12))

p16 <- ggplot(
        data=taxonomic_effort, 
        aes(x=years, y=species_per_real_taxonomist_weighted)
    ) +
    xlab("Year") + ylab("Number of species \ndescribed/ PTE \n(wted)") + theme +
    # ggtitle("Number of species described/ PTE (wted) by year") + 
    ggtitle("") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_smooth(fill=NA, color='black', size=1.5) +
    geom_line(
        size=1, aes(y=species_per_real_taxonomist_weighted_roll), color='grey50'
    ) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks2, minor_breaks=ybreaks1, limits=c(0,12))


ggsave(paste0(dir_plot, 'fig-4a.png'), p5, units="cm", width=18, height=5, dpi=300)
ggsave(paste0(dir_plot, 'fig-4b.png'), p13, units="cm", width=18, height=5, dpi=300)
ggsave(paste0(dir_plot, 'fig-4c.png'), p16, units="cm", width=18, height=5, dpi=300)


################################################################################
# Trend analysis

# species_and_pub_per_year$species_per_publication
# taxonomic_effort$species_per_real_taxonomist

ts_data_sp_per_pub <- ts(
    species_and_pub_per_year$species_per_publication,
    frequency = 1, start = c(1758)
)

ts_data_species_per_tax <- ts(
    taxonomic_effort$species_per_real_taxonomist,
    frequency = 1, start = c(1758)
)

ts_data <- ts_data_species_per_tax

# adf.test(ts_data)
# kpss.test(ts_data)
# nsdiffs(ts_data)

idx_1910 <- which(species_and_pub_per_year$date == 1910)

res <- MannKendall(ts_data_sp_per_pub)
print("--------------------------")
print("Trend test: mean species per publication")
summary(res)

res <- MannKendall(ts_data_species_per_tax[1:idx_1910])
print("--------------------------")
print("Trend test: mean species per describer before breakpoint")
summary(res)

res <- MannKendall(
    ts_data_species_per_tax[idx_1910:length(ts_data_species_per_tax)]
)
print("--------------------------")
print("Trend test: mean species per describer after breakpoint")
summary(res)

################################################################################

# Breakpoint analysis

# Resource: https://rpubs.com/MarkusLoew/12164

plot(taxonomic_effort$years, taxonomic_effort$species_per_real_taxonomist_roll)
mod <- lm(species_per_real_taxonomist~years, data = taxonomic_effort)
seg <- segmented(mod, seg.Z = ~ years, psi = list(years=1910))
summary(seg)
seg$psi

# https://otexts.com/fpp2/moving-averages.html
# https://otexts.com/fpp2/stationarity.html