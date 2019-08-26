source('2019-07-15-edie-et-al/init.r')

df$date.decade <- paste0(substr(df$date.n, 1, 3), "0s")
df$TE <- paste0(df$author, " ", df$date.n, "--", df$title)

df_biogeo_holt[,.(.N), by="Realm"][order(-N)]
df_continent[,.(.N), by="CONTINENT"][order(-N)]

## Number of species described

# Per year
species_per_year <- df[,.(.N), by=.(date.n)]
p1 <- ggplot(species_per_year, aes(x=date.n, y=N)) + 
    geom_line(size=1) + 
        xlab("") + ylab("Number of species") + 
            theme_minimal() +
                ggtitle("Number of species described per year")

# Per decade
species_per_decade <- df[,.(.N), by=.(date.decade)]
p2 <- ggplot(species_per_decade, aes(x=date.decade, y=N)) + 
    geom_bar(stat="identity") + 
        xlab("") + ylab("Number of species") + 
            theme_minimal() +
                ggtitle("Number of species described per decade")


## Number of publications

# Per year
df_publications <- unique(df[,c("date.n", "date.decade", "TE")])
publications_per_year <- df_publications[,.(.N), by=.(date.n)]
p3 <- ggplot(publications_per_year, aes(x=date.n, y=N)) + 
    geom_line(size=1) + 
        xlab("") + ylab("Number of publications") + 
            theme_minimal() +
                ggtitle("Number of publications per year")

# Per decade
publications_per_decade <- df_publications[,.(.N), by=.(date.decade)]
p4 <- ggplot(publications_per_decade, aes(x=date.decade, y=N)) + 
    geom_bar(stat="identity") + 
        xlab("") + ylab("Number of publications") + 
            theme_minimal() +
                ggtitle("Number of publications per decade")


## Species per publication
species_and_pub_per_year <- merge(species_per_year, publications_per_year, by.x="date.n", by.y="date.n", suffix=c("_species", "_publications"))
species_and_pub_per_year[, species_per_publication := N_species/N_publications]
p5 <- ggplot(species_and_pub_per_year, aes(x=date.n, y=species_per_publication)) + 
    geom_line(size=1) +
        xlab("") + ylab("Species per publication") + 
             theme_minimal() +
                ggtitle("Number of species per publication")


## Correlation between species and publications per year
p6 <- ggplot(species_and_pub_per_year, aes(x=N_publications, y=N_species)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of publications") + ylab("Number of species") +
            theme_minimal()  +
                ggtitle("Correlation between publications \nand species per year")
cor.test(species_and_pub_per_year$N_publications, species_and_pub_per_year$N_species, method = c("pearson"))


## Histogram of number of species per year
df[,species_per_pub := length(unique(paste0(genus, species))), by=.(TE)]
species_per_publication <- unique(df[,c("species_per_pub", "TE")])
p7 <- ggplot(species_per_publication, aes(x=species_per_pub)) + 
    geom_histogram(binwidth=2) +
        xlab("") + ylab("Species per publication") + 
             theme_minimal() +
                ggtitle("Number of species per publication") + 
                    scale_x_continuous(lim = c(0, 30))
summary(df$species_per_pub)

write.csv(species_per_publication, "tmp/test.csv")


gr <- grid.arrange(p1, p3, p5, p6, nrow = 2, ncol=2)
ggsave("plots/2019-07-17-edie-et-al1.png", gr, units="cm", width=20, height=18)
dev.off()

grid.arrange(p1, p3, p7, p6, nrow = 2, ncol=2)

# df_country
# df_continent
# df_biogeo
df_biogeo_holt

names(df_biogeo_holt)
