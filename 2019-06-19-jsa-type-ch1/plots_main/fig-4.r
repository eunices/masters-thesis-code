print(paste0(Sys.time(), " --- Catch per effort graph"))




species_and_pub_per_year <- df_publications_N[, list(species_per_publication=mean(n_species),
                                            N_publications=length(n_species),
                                            N_species=sum(n_species)), by="date.n"][order(date.n)]

p5 <- ggplot(species_and_pub_per_year, aes(x=date.n, y=species_per_publication)) + 
    xlab("") + ylab("Number of species/ publication") + 
    theme +
    ggtitle("") +
    # ggtitle("Number of species/ publication by year") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_smooth(fill=NA, color='black', size=1.5) +
    geom_line(size=1, y=rollmean(species_and_pub_per_year$species_per_publication, 10, 
              fill = list(NA, NULL, NA)), color='grey50') +
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
    geom_line(size=1, y=rollmean(taxonomic_effort$species_per_real_taxonomist, 10, 
              fill = list(NA, NULL, NA)), color='grey50') +
    annotate(geom='curve', x=bp_year+30, y=round(y+4,0), xend=bp_year, yend=y,
             curvature=.1, arrow=arrow(length=unit(1, 'mm')), size=1, color='red') + 
    annotate(geom='text', hjust='left', x=bp_year+33, y=round(y+4,0)+.2, 
             label='Break point', size=4, color='red') + 
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks2, minor_breaks=ybreaks1, limits=c(0,12))




p16 <- ggplot(data=taxonomic_effort, aes(x=years, y=species_per_real_taxonomist_weighted)) +
    xlab("Year") + ylab("Number of species \ndescribed/ PTE \n(wted)") + theme +
    # ggtitle("Number of species described/ PTE (wted) by year") + 
    ggtitle("") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_smooth(fill=NA, color='black', size=1.5) +
    geom_line(size=1, y=rollmean(taxonomic_effort$species_per_real_taxonomist_weighted, 10, 
              fill = list(NA, NULL, NA)), color='grey50') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks2, minor_breaks=ybreaks1, limits=c(0,12))





ggsave(paste0(dir_plot, 'fig-5a.png'), p5, units="cm", width=18, height=5, dpi=300)
ggsave(paste0(dir_plot, 'fig-5b.png'), p13, units="cm", width=18, height=5, dpi=300)
ggsave(paste0(dir_plot, 'fig-5c.png'), p16, units="cm", width=18, height=5, dpi=300)