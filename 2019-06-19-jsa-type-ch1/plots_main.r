# Set up
source('2019-06-19-jsa-type-ch1/init.r')

# Libraries
library(zoo)
library(ggrepel)

# Analyses
#############

# Parameters
theme <- theme_minimal(base_size=7)
year_end <- 2018
dir_plot <- "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\_figures\\_ch1\\"

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 1, S2
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Fig. 1, S2"))

# Individual plots

## Number of species described
# Per year
species_per_year <- df[,.(.N), by=.(date.n)][order(date.n)]
template_year <- data.frame(date.n=min(species_per_year$date.n):max(species_per_year$date.n))
species_per_year <- merge(template_year, species_per_year, by="date.n", all.x=T, all.y=F)
species_per_year[is.na(species_per_year$N),]$N <- 0
species_per_year$N_cumsum <- cumsum(species_per_year$N)

pts <- data.frame(date.n=as.integer(c(1914, 1919, 
                                      1939, 1945)))
# World War I 1914-1919; World War II 1939-1945 

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


## Number of publications

# Per year
publications_per_year <- df_publications[,.(.N), by=.(date.n)][order(date.n)]
p3 <- ggplot(publications_per_year, aes(x=date.n, y=N)) + 
    xlab("") + ylab("Number of publications") + 
    theme +
    ggtitle("Number of publications by year") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(publications_per_year$N, 10, fill = list(NA, NULL, NA))) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10)

# Per decade
publications_per_decade <- df_publications[,.(.N), by=.(date.decade)]
p4 <- ggplot(publications_per_decade, aes(x=date.decade, y=N)) + 
    geom_bar(stat="identity") + 
        xlab("") + ylab("Number of publications") + 
            theme +
                ggtitle("Number of publications by decade") 

## Species per publication
species_and_pub_per_year <- df_publications_N[, list(species_per_publication=mean(n_species),
                                            N_publications=length(n_species),
                                            N_species=sum(n_species)), by="date.n"][order(date.n)]
p5 <- ggplot(species_and_pub_per_year, aes(x=date.n, y=species_per_publication)) + 
    xlab("") + ylab("Number of species/ publication") + 
    theme +
    ggtitle("Number of species/ publication by year") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_smooth(fill=NA, color='black', size=1.5) +
    geom_line(size=1, y=rollmean(species_and_pub_per_year$species_per_publication, 10, 
              fill = list(NA, NULL, NA)), color='grey50') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks20, minor_breaks=ybreaks5)

## Correlation between species and publications per year
c <- cor.test(species_and_pub_per_year$N_publications, species_and_pub_per_year$N_species, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between publications \nand species per year (R^2=", corr, ")")
p6 <- ggplot(species_and_pub_per_year, aes(x=N_publications, y=N_species)) + 
    geom_point(alpha=0.5, color='grey') + 
    xlab("Number of publications") + ylab("Number of species") +
    theme  +
    ggtitle(title) + 
    geom_smooth(method='lm',formula=y~x, color='black', fill=NA)

## Histogram of number of species in each publication
sum_p7 <- summary(df_publications_N$n_species); sum_p7
p7 <- ggplot(df_publications_N, aes(x=n_species)) + 
    geom_histogram(binwidth=1) +
    xlab("Species per publication") + ylab("Frequency") + 
    theme +
    ggtitle("Number of species/publication \n(V+S)")
p7v <- p7 + scale_x_continuous(lim = c(0, sum_p7["3rd Qu."])) # for visualisation purposes
p7d <- ggplot_build(p7)$data[[1]]

## Species per publication across years 
mean_N_spp_per_pub <- df_publications_N[, list(mean_N_spp=mean(n_species)), 
                                               by="date.n"][order(date.n)]
p11 <- ggplot(data=mean_N_spp_per_pub, aes(x=date.n, y=mean_N_spp)) +
    xlab("Year") + ylab("Number of species described") + 
    ggtitle("Number of species described/ publication by year") +
    scale_y_continuous(lim = c(0, 40)) + 
    theme + 
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(mean_N_spp_per_pub$mean_N_spp, 10, 
              fill = list(NA, NULL, NA))) 
summary(df_publications_N$n_species)

# Number of describers 
# Per year
p8 <- ggplot(taxonomic_effort, aes(x=years, y=N_real_describers)) + 
    xlab("") + ylab("Number of PTEs") + 
    theme +
    ggtitle("Number of PTEs by year") + 
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(taxonomic_effort$N_real_describers, 10, 
              fill = list(NA, NULL, NA))) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10)

taxonomic_effort_per_person <- merge(taxonomic_effort, lp_pop, by.x="years", by.y="year",
                                     all.x=T, all.y=F) 
# https://www.worldometers.info/world-population/
taxonomic_effort_per_person[years=="2016"]$pop <- 7464022049	
taxonomic_effort_per_person[years=="2017"]$pop <- 7547858925
taxonomic_effort_per_person[years=="2018"]$pop <- 7631091040

taxonomic_effort_per_person$tax_per_mil_person <- taxonomic_effort_per_person$N_real_describers /
    (taxonomic_effort_per_person$pop /10^9)

yscale = .5
p8b <- ggplot(taxonomic_effort_per_person, aes(x=years, y=tax_per_mil_person)) + 
    xlab("") + ylab("Number of PTEs \nper billion persons (black)") + 
    theme +
    ggtitle("Number of PTEs per billion persons by year") + 
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(taxonomic_effort_per_person$tax_per_mil_person, 10, 
              fill = list(NA, NULL, NA))) +
    geom_line(aes(y=pop/(10^9)/yscale), size=1, color='red') + 
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(sec.axis = sec_axis(~ .*yscale, name="World population,\nbillions (red)"))

max(taxonomic_effort_per_person$tax_per_mil_person)
taxonomic_effort_per_person[years %in% 2015:2018]$tax_per_mil_person

## Correlation between species and PTEs per year
c <- cor.test(taxonomic_effort$N_real_describers, taxonomic_effort$N_species_described, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between PTEs \nand species per year (R^2=", corr, ")")
p9 <- ggplot(taxonomic_effort, aes(x=N_real_describers, y=N_species_described)) + 
    geom_point(alpha=0.5, color='grey') + 
    xlab("Number of PTEs") + ylab("Number of species") +
    theme  +
    ggtitle(title) + 
    geom_smooth(method='lm',formula=y~x, color='black', fill=NA)

## Histogram of number of species by each PTE
# sum_p10 <- summary(df_describers$ns_species_per_year_active); sum_p10
# p10 <- ggplot(df_describers, aes(x=spp_N)) + 
#     geom_histogram(binwidth=1) +
#         xlab("Number of species per PTE") + ylab("Frequency") + 
#              theme +
#                 ggtitle("Number of species/PTE \n(V+S)") # in their lifetime
# p10v <- p10 + scale_x_continuous(lim = c(0, sum_p10["3rd Qu."])) # for visualisation purposes
# p10d <- ggplot_build(p10)$data[[1]]
sum_p10 <- summary(df_describers_year$N); sum_p10
p10 <- ggplot(df_describers_year, aes(x=N)) + 
    geom_histogram(binwidth=1) +
    xlab("Number of species per PTE") + ylab("Frequency") + 
    theme +
    ggtitle("Number of species/PTE \n(V+S)") # in their lifetime
p10v <- p10 + scale_x_continuous(lim = c(-1, 10)) # for visualisation purposes
p10d <- ggplot_build(p10)$data[[1]]

bp_year = 1910; y=4.1
# y = taxonomic_effort[which(years==bp_year)]$species_per_real_taxonomist
p13 <- ggplot(data=taxonomic_effort, aes(x=years, y=species_per_real_taxonomist)) +
    xlab("Year") + ylab("Number of species/ PTE") + 
    ggtitle("Number of species described/ PTE by year") + theme + 
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_smooth(fill=NA, color='black', size=1.5) +
    geom_line(size=1, y=rollmean(taxonomic_effort$species_per_real_taxonomist, 10, 
              fill = list(NA, NULL, NA)), color='grey50') +
    annotate(geom='curve', x=bp_year+30, y=round(y+4,0), xend=bp_year, yend=y,
             curvature=.1, arrow=arrow(length=unit(1, 'mm')), size=1, color='red') + 
    annotate(geom='text', hjust='left', x=bp_year+33, y=round(y+4,0)+.2, label='Break point', size=4, color='red') + 
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks2, minor_breaks=ybreaks1, limits=c(0,12))

# Weighted PTEs

## Correlation between species and PTEs per year
c <- cor.test(taxonomic_effort[is.finite(N_weighted_real_describers)]$N_weighted_real_describers, 
              taxonomic_effort[is.finite(N_weighted_real_describers)]$N_species_described, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between PTEs (wted) \nand species per year (R^2=", corr, ")")
p14 <- ggplot(taxonomic_effort, aes(x=N_weighted_real_describers, y=N_species_described)) + 
    geom_point(alpha=0.5, color='grey') + 
    xlab("Number of PTEs (wted)") + ylab("Number of species") +
    theme  +
    ggtitle(title) + 
    geom_smooth(method='lm',formula=y~x, color='black', fill=NA)

## Species per author across years
p15 <- ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_real_describers)) +
    xlab("Year") + ylab("Number of PTEs (wted)") + theme +
    ggtitle("Number of PTEs (wted) by year") +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(taxonomic_effort$N_weighted_real_describers, 10, 
              fill = list(NA, NULL, NA))) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10)
              

p16 <- ggplot(data=taxonomic_effort, aes(x=years, y=species_per_real_taxonomist_weighted)) +
    xlab("Year") + ylab("Number of species \ndescribed/ PTE \n(wted)") + theme +
    ggtitle("Number of species described/ PTE (wted) by year") + 
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_smooth(fill=NA, color='black', size=1.5) +
    geom_line(size=1, y=rollmean(taxonomic_effort$species_per_real_taxonomist_weighted, 10, 
              fill = list(NA, NULL, NA)), color='grey50') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks2, minor_breaks=ybreaks1, limits=c(0,12))

sum_p17 <- summary(taxonomic_effort$N_weighted_real_describers); sum_p17
p17 <- ggplot(taxonomic_effort, aes(x=N_weighted_real_describers)) + 
    geom_histogram(binwidth=1) +
        xlab("Number of species per PTE (wted)") + ylab("Frequency") + 
             theme +
                ggtitle("Number of species/PTE (wted) \n(V+S)") # in their lifetime
p17v <- p17 + scale_x_continuous(lim = c(0, sum_p17["3rd Qu."])) # for visualisation purposes
p17d <- ggplot_build(p17)$data[[1]]

cols <- c("years", "N_real_describers", "N_weighted_real_describers", "N_species_described")
pub_auth_yr <- merge(species_and_pub_per_year[, c("date.n", "N_publications")],
                     taxonomic_effort[, ..cols], 
                     by.x="date.n", by.y="years", all.x=T, all.y=T)
pub_auth_yr[is.na(pub_auth_yr)] <- 0
pub_auth_yr <- pub_auth_yr[date.n<=2018]
# pub_auth_yr[!is.finite(N_weighted_real_describers)]$N_weighted_real_describers <- 195.57


c <- cor.test(pub_auth_yr[is.finite(N_weighted_real_describers)]$N_real_describers, 
              pub_auth_yr[is.finite(N_weighted_real_describers)]$N_publications, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between PTEs \nand pub. per year (R^2=", 
                corr, ")")
p18 <- ggplot(pub_auth_yr, aes(x=N_real_describers, y=N_publications)) + 
    geom_point(alpha=0.5, color='grey') + 
    xlab("Number of PTEs") + ylab("Number of publications") +
    theme  +
    ggtitle(title) + 
    geom_smooth(method='lm',formula=y~x, color='black', fill=NA)

c <- cor.test(pub_auth_yr[is.finite(N_weighted_real_describers)]$N_weighted_real_describers, 
              pub_auth_yr[is.finite(N_weighted_real_describers)]$N_publications, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between PTEs (wted) \nand pub. per year (R^2=", 
                corr, ")")
p19 <- ggplot(pub_auth_yr, aes(x=N_weighted_real_describers, y=N_publications)) + 
    geom_point(alpha=0.5, color='grey') + 
    xlab("Number of PTEs (wted)") + ylab("Number of publications") +
    theme  +
    ggtitle(title) + 
    geom_smooth(method='lm', formula=y~x, color='black', fill=NA)

########### PLOTS ###########

# FIGURE: Edie et al's Fig S2
## Original plot
gr <- grid.arrange(p0, p3, p5, p6, ncol=2, nrow=2)
dev.off()

## Improved plot
grid.arrange(p0, p3, p8, ncol=1) # time series
grid.arrange(p7v, p10v) # histograms
grid.arrange(p6, p9) # correlation


# FIGURE: MS Fig 1: taxonomic effort over time
## Draft 1
grid.arrange(p3, p8, p6, p9, p15, p14, 
            widths=c(2, 1, 1),
            layout_matrix=rbind(c(1, 3),
                                c(2, 4),
                                c(5, 6)))

## Draft 2
grid.arrange(p3, p8, p6, p9, p15, p14, p7v, p10v, p17v,
            widths=c(2, 1, 1),
            layout_matrix=rbind(c(1, 3, 7),
                                c(2, 4, 8),
                                c(5, 6, 9)))

## Draft 3
p <- ggplot() + theme
grid.arrange(p3, p8, p6, p9, p15, p14, p18, p19, p,
            widths=c(2, 1, 1),
            layout_matrix=rbind(c(1, 3, 9),
                                c(2, 4, 7),
                                c(5, 6, 8)))

## Draft 4
p <- ggplot() + theme
gr <- grid.arrange(p3, p8, p6, p9, p15, p14, p18, p19, p, p0,
                   widths=c(2, 1, 1),
                   layout_matrix=rbind(c(10, 9, 9),
                                       c(1, 3, 9),
                                       c(2, 4, 7),
                                       c(5, 6, 8)))
ggsave(paste0(dir_plot, 'fig-1.png'), gr, units="cm", width=21, height=18, dpi=300)

# dataset for p0

# dataset for p3
# dataset for p8
# dataset for p15

# dataset for p

## Draft 5
# p <- ggplot() + theme
# gr <- grid.arrange(p3, p8, p6, p9, p15, p14, p18, p19, p, p0, p8b,
#                    widths=c(2, 1, 1),
#                    layout_matrix=rbind(c(10, 9, 9),
#                                        c(1, 3, 9),
#                                        c(2, 4, 7),
#                                        c(5, 6, 8),
#                                        c(11, 9, 9)))
# ggsave(paste0(dir_plot, 'fig-1.png'), gr, units="cm", width=21, height=21, dpi=300)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Taxonomic effort - publications (boxplot + violin plot)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Taxonomic effort - publications (boxplot + violin plot)"))

# Individual plots

## Mean number of species per publication by decade
median_N_spp_per_pub_decade <- df_publications_N[, list(median_N_spp=median(n_species)), 
                                               by="date.decade"]

calc_median <- function(x){
  return(c(y = -5, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

plot_tax_pub_decade <- 
    ggplot(data=df_publications_N, aes(x=date.decade, y=n_species)) +
        geom_violin(width=3, fill="grey", alpha=.7, color="grey40") + 
        # geom_boxplot(width=0.2, outlier.size=NULL, fill="white") +
        geom_jitter(shape=16, position=position_jitter(0.2), 
                    size=0.2, alpha=0.2, color='grey10') +
        # geom_line(data=median_N_spp_per_pub_decade, 
        #           mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
        #           size=1.3, color="red", alpha=0.9, linetype='dashed') +
        geom_point(data=median_N_spp_per_pub_decade[date.decade != "1750s"], 
                  mapping=aes(x=date.decade, y=median_N_spp, group=1),
                  size=2, color='black') +
        geom_line(data=median_N_spp_per_pub_decade[date.decade != "1750s"], 
                  mapping=aes(x=date.decade, y=median_N_spp, group=1),
                  size=1, color='black') +
        scale_y_continuous(limit=c(-5, 40)) +
        # stat_summary(fun.data=calc_median, geom="text", fun=median,
        #              position=position_dodge(width = 0.75), size=3) +
        xlab("\nDecade") + ylab("Number of species described \nper publication\n") +
        theme

## Species per author across years # TODO: N_species/N_author
median_N_spp_per_des_decade <- df_describers_year[, list(median_N_spp=median(N)), 
    by="date.decade"]

plot_tax_des_decade <- 
    ggplot(data=df_describers_year, aes(x=date.decade, y=N)) +
        geom_violin(width=3, fill="grey", alpha=.7, color="grey40") + 
        # geom_boxplot(width=0.2, outlier.size=NULL, fill="white") +
        geom_jitter(shape=16, position=position_jitter(0.2), 
                    size=0.2, alpha=0.2, color='grey10') +
        # geom_line(data=median_N_spp_per_pub_decade, 
        #           mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
        #           size=1.3, color="red", alpha=0.9, linetype='dashed') +
        geom_point(data=median_N_spp_per_des_decade[date.decade != "1750s"], 
                  mapping=aes(x=date.decade, y=median_N_spp, group=1),
                  size=2, color='black') +
        geom_line(data=median_N_spp_per_des_decade[date.decade != "1750s"], 
                  mapping=aes(x=date.decade, y=median_N_spp, group=1),
                  size=1, color='black') +
        scale_y_continuous(limit=c(-5, 40)) +
        # stat_summary(fun.data=calc_median, geom="text", fun=median,
        #              position=position_dodge(width = 0.75), size=3) +
        xlab("\nDecade") + ylab("Number of species described \nper PTE\n") +
        theme

# Combined plots
gr <- grid.arrange(grobs=list(plot_tax_pub_decade, plot_tax_des_decade), ncol=1)
ggsave(paste0(dir_plot, 'fig-2.png'), gr, units="cm", width=21, height=16, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Catch per effort graph
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Catch per effort graph"))

gr <- grid.arrange(grobs=list(p5, p13, p16), ncol=1)
ggsave(paste0(dir_plot, 'fig-3.png'), gr, units="cm", width=18, height=15, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Prop species describing <=N species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Prop species describing <=N species"))

taxonomic_effort$N_real_describers.1.prop <- taxonomic_effort$N_real_describers.1 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.2.prop <- taxonomic_effort$N_real_describers.2 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.3.prop <- taxonomic_effort$N_real_describers.3 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.4.prop <- taxonomic_effort$N_real_describers.4 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.5.prop <- taxonomic_effort$N_real_describers.5 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.6.prop <- taxonomic_effort$N_real_describers.6 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.7.prop <- taxonomic_effort$N_real_describers.7 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.8.prop <- taxonomic_effort$N_real_describers.8 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.9.prop <- taxonomic_effort$N_real_describers.9 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.10.prop <- taxonomic_effort$N_real_describers.10 / taxonomic_effort$N_real_describers * 100

cols <- names(taxonomic_effort)[grepl(".prop", names(taxonomic_effort))]
cols <- c("years", cols)
des_y <- melt(taxonomic_effort[, ..cols], id.vars="years")

formatstr <- function(string) {
  string <- gsub("N_real_describers.", "", string)
  string <- gsub(".prop", "", string)
  ifelse(string=="1", paste0(string, " species"), paste0("<=", string, " species"))
}

plot_tax <- ggplot(des_y, aes(x=years, y=value, group=variable)) + 
    geom_line(size=.5, colour="grey", linetype='dashed') + 
    geom_point(size=.5, color='grey') + 
    geom_smooth(size=1, colour="black") +
    xlab("Year") + ylab("Proportion of PTEs describing <= N species (%)\n") +
    facet_wrap(. ~variable, ncol=2, labeller=labeller(variable=formatstr), dir="v") +
    scale_y_continuous(limits=c(0, 50), breaks=seq(0,50,10)) +
    theme

ggsave(paste0(dir_plot, 'fig-4.png'), plot_tax, units="cm", width=15, height=9, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - In-text figures
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- in-text figures"))

# Hyperdiverse Megachile
meg <- df[tolower(genus)=="megachile", list(N=.N), by=c("type.repository.n_short", "country.of.type.repository.n_long")][order(-N)]
sum(meg$N)
sum(meg[type.repository.n_short != "[unknown]"][1:20]$N)
table(meg[type.repository.n_short != "[unknown]"][1:20]$country.of.type.repository.n_long)



#########################################################################################
# Supporting Information
#########################################################################################

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Describers profile - one large monograph towards end of life, or many small?
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Describers profiles (for appendix)"))
des <- df_describers[, c("full.name.of.describer.n", "last.name", 
                         "spp_per_pub_mean", "n_pubs",
                         "spp_per_pub_mean_20y", "n_pubs_20y",
                         "spp_per_pub_mean_5y", "n_pubs_5y",
                         "n_spp_20y", "n_spp_5y",
                         "dob.describer.n", "dod.describer.n",
                         "spp_N",
                         "max", "pub_years")]
des$dod.describer.n <- as.numeric(des$dod.describer.n)
des$dob.describer.n <- as.numeric(des$dob.describer.n)
des$max <- as.numeric(des$max)

des <- des[!(is.na(dod.describer.n) | is.na(dob.describer.n))]
dim(des)
des$years_last_pub_death <- des$dod.describer.n - des$max
des$age_at_death <- des$dod.describer.n - des$dob.describer.n

mround <- function(x,base) base*round(x/base)
des$date.century <- substr(as.character((des$dod.describer.n - des$dob.describer.n)/2 + 
    des$dob.describer.n), 1, 3)
# des$date.century <- substr(as.character(des$dob.describer.n), 1, 3)
des$date.century <- mround(as.numeric(des$date.century), 5)
des$date.century <- paste0(des$date.century, "0s")
table(des$date.century)

des$check_young <- ifelse(des$age_at_death >=40, "T", "F") #young
des$check_few.spp <- ifelse(des$spp_N <= 12, "T", "F") #few spp (than median)
des$check_few.spp.pub <- ifelse(des$spp_per_pub_mean <=10, "T", "F") #few spp/pub
des$check_few.pub <- ifelse(des$n_pubs <=3, "T", "F") # few pub
des$check_few.pub.near.death <- ifelse(is.na(des$n_spp_20y), "T", ifelse(
    des$n_spp_20y/des$spp_N >=0.8, "F", "T")) # few pub

summary(des$age_at_death)

des0 <- des[, list(names=paste0(full.name.of.describer.n, collapse="; "),
                   med_age_at_death=median(age_at_death, na.rm=T),
                   med_spp_N = median(as.numeric(spp_N), na.rm=T),
                   med_spp_per_pub_mean = median(spp_per_pub_mean, na.rm=T),
                   med_prop_20y = median(n_spp_20y/spp_N, na.rm=T)
                   ),
    by=c("check_young")]

des1 <- des[, list(names=paste0(full.name.of.describer.n, collapse="; "),
                   med_age_at_death=median(age_at_death, na.rm=T),
                   med_spp_N = median(as.numeric(spp_N), na.rm=T),
                   med_spp_per_pub_mean = median(spp_per_pub_mean, na.rm=T),
                   med_prop_20y = median(n_spp_20y/spp_N, na.rm=T)
                   ),
    by=c("check_young", "check_few.spp", "check_few.spp.pub", "check_few.pub.near.death")]

des2 <- des[, list(.N), 
    by=c("check_young", "check_few.spp", "check_few.spp.pub", "check_few.pub.near.death", "date.century")]

des3 <- dcast(des2, 
              check_young + check_few.spp.pub + check_few.pub.near.death + check_few.spp  ~ date.century, 
              value.var="N", fun=sum)
des4 <- merge(des1, des3,
              by=c("check_young", "check_few.spp.pub", "check_few.pub.near.death", "check_few.spp"))

write.csv(des4, paste0(dir_data, 'ch1/2019-10-02-taxonomist-one-large-mono.csv'),
          row.names=F)

# Checks
table(is.na(des$check_young))
table(is.na(des$check_few.spp))
table(is.na(des$check_few.spp.pub))
table(is.na(des$check_few.pub.near.death))

# Throughout life proportion
auth_throughout_life <- table(des[check_young=="T",]$check_few.pub.near.death)
prop.table(auth_throughout_life)*100

# Group that died young, few publications, few species, average number of species per pub
# Group that did not die young, few publications, few species, average number of species per pub
# Group that did not die young, large number of publications, large number of species, large number of species per pub
# Group that did not die young, few publications, large number of species {N and when}

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Paragraph on taxonomic effort
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Paragraph on taxonomic effort"))

# Count number of statuses
df2 <- get_df2(write=F)[,c("idx", "status", "date.n")]
df_years <- rbind(df[, c("idx", "status", "date.n")], df2)
status <- table(df_years$status)
sum(status)
round(prop.table(status)*100, 1)

# Get data
df_tax <- df_describers[spp_N_1st_auth_s>0]; dim(df)
df_tax$diff <- df_tax$max - df_tax$ns_max
df_tax$non_valid_spp_N <- df_tax$spp_N - df_tax$ns_spp_N

# Segregate authors with only non-valid and valid w/ non-valid species
tax_only_non_valid <- df_tax[ns_spp_N==0]$full.name.of.describer.n
tax_valid_and_non_valid <-  df_tax[ns_spp_N > 0 & non_valid_spp_N > 0]$full.name.of.describer.n

# Total number of author-years
total_author_years <- dim(df_describers_year)[1]

# Author-years: Only non-valid species
length(tax_only_non_valid)
length(df_describers_year[full.name.of.describer %in% tax_only_non_valid]$N)
length(df_describers_year[full.name.of.describer %in% tax_only_non_valid]$N) / total_author_years * 100

# Did not shift the median
summary(df_describers_year$N)
summary(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid)]$N)
summary(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid, tax_valid_and_non_valid)]$N)

# Author-years: At least valid species and at least 1 non-valid species
length(tax_valid_and_non_valid)
round(prop.table(table(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid]$diff==0))*100, 1)
summary(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid & diff >0]$diff)
sum(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid & diff >0]$diff)

# Proportion of author-years
(721+524)/ total_author_years * 100

# Plot number of authors-years in a density plot
p1 <- ggplot(df_describers_year) + 
    geom_density(aes(x = N), alpha = 0.2) + 
    xlim(c(0, 20)) + theme
p2 <- ggplot(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid)]) +
    geom_density(aes(x = N), alpha = 0.2) + xlim(c(0,20)) + theme
grid.arrange(p1, p2) # they look similar


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Histogram of PTEs active years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Histogram of PTEs active years"))

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
                   fill='grey80', alpha=0.6) +
    scale_x_continuous(breaks= seq(0, max(yrs_active1), 10)) +
    scale_y_continuous(breaks= seq(0, 50, 10), limits=c(0, 50)) +
    geom_vline(xintercept=9, color='grey', size=.5) +
    geom_vline(xintercept=summary(yrs_active1)[5], color='grey', size=.5, linetype="dashed") +
    geom_vline(xintercept=summary(yrs_active1b)[5], color='grey', size=.5, linetype="dotted") +
    xlab("\nNumber of active years") + ylab("Proportion of PTEs (%)\n") + 
    theme
ggsave(paste0(dir_plot, '_si/fig-2.png'), hist_active_yrs, units="cm", width=10, height=8, dpi=300)

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
    xlab("Number of PTEs") + ylab("Year") + 
    ggtitle(paste0("Number of PTEs in each year"))

# Mean number of species/year 
hist_mean_sp_per_auth <- ggplot(df_describers) +
    geom_histogram(mapping=aes(x=ns_species_per_year_active, y=..count../sum(..count..) * 100),
                   binwidth=5) + theme


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Mean number of species/ author over the years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Mean number of species/ author over the years"))

spp_per_auth <- df_describers_year[, list(mean=mean(N), 
                                          median=median(N)), by=c("date.decade")][order(date.decade)]

df_describers_year[date.decade=="1750s"]

plot_spp_per_auth_per_decade <- 
    ggplot(data=spp_per_auth, aes(x=date.decade, y=mean)) +
        geom_bar(stat='identity') + 
        xlab("\nDecade") + ylab("Mean number of species \ndescribed per author per year\n") +
        theme + scale_fill_grey()

ggsave(paste0(dir_plot, '_si/fig-3.png'), plot_spp_per_auth_per_decade, units="cm", width=21, height=5, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Number of authors/ publication over the years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Number of authors/ publication over the years"))

n_auth <- df[, c("date.n", "date.decade", "idx", "full.name.of.describer")] %>% 
    separate_rows(full.name.of.describer, sep="; ")
n_auth <- data.table(n_auth)[order(idx)]
n_auth <- n_auth[, list(N=.N), by=c("date.n", "date.decade", "idx")]
n_auth$N <- as.character(n_auth$N)

calc_median <- function(x){
  return(c(y = -5, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

median_auth_per_sp_decade <- n_auth[, list(median_n_auth=median(N)), by='date.decade'][order(date.decade)]

plot_n_auth_per_decade <- 
    ggplot(data=n_auth, aes(x=date.decade, fill=N)) +
        geom_bar(position = "fill") + 
        xlab("\nDecade") + ylab("Proportion of number \nof authors per species\n") +
        theme + scale_fill_grey()

ggsave(paste0(dir_plot, '_si/fig-4.png'), plot_n_auth_per_decade, units="cm", width=21, height=5, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Histogram of PTEs total spp described per year
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Histogram of PTEs total spp described per year"))

# Tabulate statistics of years active
tax <- df_describers[spp_N_1st_auth_s>=1]

highlight_auth <- tax[ns_spp_N>750]$full.name.of.describer.n
tax_highlight <- tax[full.name.of.describer.n %in% highlight_auth][,c("ns_spp_N", "last.name")]

x_axis <- seq(0, max(tax$ns_spp_N), 1000)
x_axis_minor <- seq(0, max(tax$ns_spp_N), 100)

hist_tl_spp <- ggplot(tax, aes(x=ns_spp_N)) +
    geom_histogram(mapping=aes(y=..count../sum(..count..) * 100), fill='grey30', binwidth=100) + 
    geom_vline(xintercept=median(tax$ns_spp_N), color='grey', size=.5) +
    xlab("\nTotal number of valid species described, by PTE") + 
    ylab("Proportion of PTEs (%)\n") + 
    geom_label_repel(data=tax_highlight, 
                     aes(x=ns_spp_N, y=.1, label=last.name),
                     size=2, nudge_x=10, nudge_y=30,
                     fontface='bold', color='black', segment.color='grey80', force=1,
                     box.padding = unit(0.001, 'lines')) +
    scale_x_continuous(breaks=x_axis, minor_breaks=x_axis_minor) +
    # scale_y_continuous(breaks= seq(0, 12, 1), limits=c(0, 12)) +
    theme
ggsave(paste0(dir_plot, '_si/fig-5.png'), hist_tl_spp, units="cm", width=10, height=8, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Histogram of PTEs mean sp described per year
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Histogram of PTEs mean sp described per year"))

# Tabulate statistics of years active
tax <- df_describers[spp_N_1st_auth_s>=1]

# Highlight auths from previous section
tax_highlight <- tax[full.name.of.describer.n %in% highlight_auth][,c("ns_species_per_year_active", "last.name")]

hist_mean_spp <- ggplot(tax, aes(x=ns_species_per_year_active)) +
    geom_histogram(mapping=aes(y=..count../sum(..count..) * 100), fill='grey30', binwidth=1) + 
    geom_vline(xintercept=median(tax$ns_species_per_year_active), color='grey', size=.5) +
    xlab("\nMean number of species described per year, by PTE") +
    ylab("Proportion of PTEs (%)\n") + 
    scale_x_continuous(breaks= seq(0, max(tax$ns_species_per_year_active), 10)) +
    geom_label_repel(data=tax_highlight, 
                     aes(x=ns_species_per_year_active, y=.1, label=paste0(last.name, " (", round(ns_species_per_year_active, 0),")")), 
                     size=2, nudge_x=20, nudge_y=30,
                     fontface='bold', color='black', segment.color='grey80', force=5,
                     box.padding = unit(0.001, 'lines')) +
theme
ggsave(paste0(dir_plot, '_si/fig-6.png'), hist_mean_spp, units="cm", width=10, height=8, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Tax per person
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- tax per person"))

# ggsave(paste0(dir_plot, '_si/fig-7.png'), p8b, units="cm", width=21, height=5, dpi=300)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Other biodata info
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Other biodata info"))

# Other biodata info
# Who described the most bees
tax_most_spp <- df_describers[order(-ns_spp_N)]
tax_most_spp$full.name.of.describer.n <- paste0(tax_most_spp$full.name.of.describer.n, " (", 
    tax_most_spp$min, "-", tax_most_spp$max_corrected, ")")
tax_most_spp <- tax_most_spp[, c("full.name.of.describer.n", "ns_spp_N", "n_pubs")]
summary(df_describers$ns_spp_N)
write.csv(tax_most_spp[1:10], 
          paste0(dir_data, 'ch1/2019-10-03-tax-1-most-spp.csv'), row.names=F)

# Who had the most number of publications
tax_most_pub <- df_describers[order(-n_pubs)]
tax_most_pub$full.name.of.describer.n <- paste0(tax_most_pub$full.name.of.describer.n, " (", 
    tax_most_pub$min, "-", tax_most_pub$max_corrected, ")")
tax_most_pub <- tax_most_pub[, c("full.name.of.describer.n", "n_pubs")]
write.csv(tax_most_pub[1:10], 
          paste0(dir_data, 'ch1/2019-10-03-tax-2-highest-pub.csv'), row.names=F)

# Who had the most synonyms
check <- df_describers$spp_N >= 10
cols <- c("full.name.of.describer.n", "prop_species_syn", "spp_N", "syn_spp_N", "min", "max",
          "dob.describer.n", "dod.describer.n")
tax_highest_prop_syn <- df_describers[check, ..cols][order(-prop_species_syn)]
table(check)
summary(df_describers[check]$prop_species_syn)

# Francis Walker - many synonyms https://en.wikipedia.org/wiki/Francis_Walker_(entomologist)
df_describers[full.name.of.describer.n=="Francis Walker"]

write.csv(tax_highest_prop_syn[1:10], 
          paste0(dir_data, 'ch1/2019-10-03-tax-3-highest-syn.csv'), row.names=F)

