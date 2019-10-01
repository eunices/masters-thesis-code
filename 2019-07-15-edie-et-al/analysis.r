source('2019-07-15-edie-et-al/init.r')

# Analyses
#############

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 1, S2
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Fig. 1, S2"))

# Individual plots

## Number of species described
# Per year
species_per_year <- df[,.(.N), by=.(date.n)]
template_year <- data.frame(date.n=min(species_per_year$date.n):max(species_per_year$date.n))
species_per_year <- merge(template_year, species_per_year, by="date.n", all.x=T, all.y=F)
species_per_year[is.na(species_per_year$N),]$N <- 0
species_per_year$N_cumsum <- cumsum(species_per_year$N)

p0 <- ggplot(species_per_year, aes(x=date.n, y=N)) + 
    geom_line(size=1) + 
        xlab("") + ylab("Number of species") + 
            theme_minimal() +
                ggtitle("Number of species described per year") + geom_smooth()

species_per_year2 <- melt(species_per_year, "date.n", stringsAsFactors=F)
species_per_year2$variable <- as.character(species_per_year2$variable)
unique(species_per_year2$variable)
labs <- c(`N` = "N species",
          `N_cumsum` = "Cumulative N species")
p1 <- ggplot(species_per_year2, aes(x=date.n, y=value)) + 
    facet_wrap(.~variable, nrow=2, scales = "free_y", labeller= as_labeller(labs)) +
        geom_line(size=1) + 
            xlab("") + ylab("") + 
                theme_minimal() +
                    geom_smooth()

# Per decade
species_per_decade <- df[,.(.N), by=.(date.decade)]
p2 <- ggplot(species_per_decade, aes(x=date.decade, y=N)) + 
    geom_bar(stat="identity") + 
        xlab("") + ylab("Number of species") + 
            theme_minimal() +
                ggtitle("Number of species described per decade")


## Number of publications

# Per year
publications_per_year <- df_publications[,.(.N), by=.(date.n)]
p3 <- ggplot(publications_per_year, aes(x=date.n, y=N)) + 
    geom_line(size=1) + 
        xlab("") + ylab("Number of publications") + 
            theme_minimal() +
                ggtitle("Number of publications per year") + geom_smooth()

# Per decade
publications_per_decade <- df_publications[,.(.N), by=.(date.decade)]
p4 <- ggplot(publications_per_decade, aes(x=date.decade, y=N)) + 
    geom_bar(stat="identity") + 
        xlab("") + ylab("Number of publications") + 
            theme_minimal() +
                ggtitle("Number of publications per decade") 

## Species per publication
df_pubs <- data.table(df_publications %>% separate_rows(idxes, sep="; "))
# df_pubs2 <- df_pubs[as.numeric(idxes) <= 20699]
df_pubs$idxes <- as.numeric(df_pubs$idxes)
df_pubs$type <- ""
df_pubs[idxes <= 20699]$type <- "n_valid"
df_pubs[idxes > 20699 & idxes <= 32285]$type <- "n_synonym"
df_pubs[idxes > 32285 & idxes <= 33198]$type <- "n_subspecies"
df_pubs[idxes > 33198]$type <- "n_var"
df_pubs2 <- df_pubs[, list(N_species=.N), 
                      by=c("date.n", "paper.authors", "journal", "title", 
                           "volume", "issue", "page.numbers.publication", "type")]
df_pubs2 <- dcast(df_pubs2, 
                  date.n + paper.authors + journal + title + volume + issue + page.numbers.publication  ~ type, value.var="N_species", fun.aggregate=sum)
df_pubs2$n_species <- df_pubs2$n_valid + df_pubs2$n_synonym
species_and_pub_per_year <- df_pubs2[, list(species_per_publication=mean(n_species),
                                            N_publications=length(n_species),
                                            N_species=sum(n_species)), by="date.n"]
p5 <- ggplot(species_and_pub_per_year, aes(x=date.n, y=species_per_publication)) + 
    geom_line(size=1) +
        xlab("") + ylab("Species per publication") + 
             theme_minimal() +
                ggtitle("Number of species per publication") + geom_smooth()

## Correlation between species and publications per year
c <- cor.test(species_and_pub_per_year$N_publications, species_and_pub_per_year$N_species, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between publications \nand species per year (R^2=", corr, ")")
p6 <- ggplot(species_and_pub_per_year, aes(x=N_publications, y=N_species)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of publications") + ylab("Number of species") +
            theme_minimal()  +
                ggtitle(title) + 
                        geom_smooth(method='lm',formula=y~x)

## Histogram of number of species in each publication
sum_p7 <- summary(df_pubs2$n_species); sum_p7
p7 <- ggplot(df_pubs2, aes(x=n_species)) + 
    geom_histogram(binwidth=1) +
        xlab("Species per publication") + ylab("Frequency") + 
             theme_minimal() +
                ggtitle("Number of species \nper publication (V+S)")
p7v <- p7 + scale_x_continuous(lim = c(0, sum_p7["3rd Qu."])) # for visualisation purposes
p7d <- ggplot_build(p7)$data[[1]]


# Number of taxonomists 
# Per year
p8 <- ggplot(taxonomic_effort, aes(x=years, y=N_real_describers)) + 
    geom_line(size=1) + 
        xlab("") + ylab("Number of taxonomists") + 
            theme_minimal() +
                ggtitle("Number of taxonomists per year") + geom_smooth()


## Correlation between species and taxonomists per year
c <- cor.test(taxonomic_effort$N_real_describers, taxonomic_effort$N_species_described, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between taxonomists \nand species per year (R^2=", corr, ")")
p9 <- ggplot(taxonomic_effort, aes(x=N_real_describers, y=N_species_described)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of taxonomists") + ylab("Number of species") +
            theme_minimal()  +
                ggtitle(title) + 
                    geom_smooth(method='lm',formula=y~x)

## Histogram of number of species by each taxonomist
sum_p10 <- summary(df_describers$spp_N); sum_p10
p10 <- ggplot(df_describers, aes(x=spp_N)) + 
    geom_histogram(binwidth=1) +
        xlab("Number of species per taxonomist") + ylab("Frequency") + 
             theme_minimal() +
                ggtitle("Number of species \nper taxonomist (V+S)") # in their lifetime
p10v <- p10 + scale_x_continuous(lim = c(0, sum_p10["3rd Qu."])) # for visualisation purposes
p10d <- ggplot_build(p10)$data[[1]]

# Combined plots: Fig S2
## Original plot
grid.arrange(p0, p3, p5, p6, ncol=2, nrow=2)

## Improved plot
grid.arrange(p0, p3, p8, ncol=1) # time series
grid.arrange(p7v, p10v) # histograms
grid.arrange(p6, p9) # correlation

gr <- grid.arrange(p3, p8, p7v, p10v, p6, p9,
                   widths=c(2, 1, 1),
                   layout_matrix=rbind(c(2, 4, 6),
                                       c(3, 5, 7)))
ggsave("plots/2019-07-17-edie-et-al1.png", gr, units="cm", width=20, height=18)
dev.off()

# Combined plots: Fig 1
## Original plot
p1

