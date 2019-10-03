source('2019-07-15-edie-et-al/init.r')

# Analyses
#############

# Parameters
theme <- theme_minimal()

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
            theme +
                ggtitle("Number of species described by year") + geom_smooth()

species_per_year2 <- melt(species_per_year, "date.n", stringsAsFactors=F)
species_per_year2$variable <- factor(species_per_year2$variable, c("N_cumsum", "N"))
labs <- c(`N` = "N species",
          `N_cumsum` = "Cumulative N species")
p1 <- ggplot(species_per_year2, aes(x=date.n, y=value)) + 
    facet_wrap(.~variable, nrow=2, scales = "free_y", labeller= as_labeller(labs)) +
        geom_line(size=1) + 
            xlab("") + ylab("") + 
                theme +
                    geom_smooth()

# Per decade
species_per_decade <- df[,.(.N), by=.(date.decade)]
p2 <- ggplot(species_per_decade, aes(x=date.decade, y=N)) + 
    geom_bar(stat="identity") + 
        xlab("") + ylab("Number of species") + 
            theme +
                ggtitle("Number of species described  by decade")


## Number of publications

# Per year
publications_per_year <- df_publications[,.(.N), by=.(date.n)]
p3 <- ggplot(publications_per_year, aes(x=date.n, y=N)) + 
    geom_line(size=1) + 
        xlab("") + ylab("Number of publications") + 
            theme +
                ggtitle("Number of publications by year") + geom_smooth()

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
                                            N_species=sum(n_species)), by="date.n"]
p5 <- ggplot(species_and_pub_per_year, aes(x=date.n, y=species_per_publication)) + 
    geom_line(size=1) +
        xlab("") + ylab("Species/ publication") + 
             theme +
                ggtitle("Number of species/ publication by year") + geom_smooth()

## Correlation between species and publications per year
c <- cor.test(species_and_pub_per_year$N_publications, species_and_pub_per_year$N_species, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between publications \nand species per year (R^2=", corr, ")")
p6 <- ggplot(species_and_pub_per_year, aes(x=N_publications, y=N_species)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of publications") + ylab("Number of species") +
            theme  +
                ggtitle(title) + 
                        geom_smooth(method='lm',formula=y~x)

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
                                               by="date.n"]
p11 <- ggplot(data=mean_N_spp_per_pub, aes(x=date.n, y=mean_N_spp)) +
    geom_line(stat="identity", size=1) + theme +
    xlab("Year") + ylab("Number of species described") + 
                ggtitle("Number of species described/ publication by year") +
                    geom_smooth() + scale_y_continuous(lim = c(0, 40))
summary(df_publications_N$n_species)

# Number of taxonomists 
# Per year
p8 <- ggplot(taxonomic_effort, aes(x=years, y=N_real_describers)) + 
    geom_line(size=1) + 
        xlab("") + ylab("Number of taxonomists") + 
            theme +
                ggtitle("Number of taxonomists by year") + geom_smooth()


## Correlation between species and taxonomists per year
c <- cor.test(taxonomic_effort$N_real_describers, taxonomic_effort$N_species_described, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between taxonomists \nand species per year (R^2=", corr, ")")
p9 <- ggplot(taxonomic_effort, aes(x=N_real_describers, y=N_species_described)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of taxonomists") + ylab("Number of species") +
            theme  +
                ggtitle(title) + 
                    geom_smooth(method='lm',formula=y~x)

## Histogram of number of species by each taxonomist
# sum_p10 <- summary(df_describers$ns_species_per_year_active); sum_p10
# p10 <- ggplot(df_describers, aes(x=spp_N)) + 
#     geom_histogram(binwidth=1) +
#         xlab("Number of species per taxonomist") + ylab("Frequency") + 
#              theme +
#                 ggtitle("Number of species/taxonomist \n(V+S)") # in their lifetime
# p10v <- p10 + scale_x_continuous(lim = c(0, sum_p10["3rd Qu."])) # for visualisation purposes
# p10d <- ggplot_build(p10)$data[[1]]
sum_p10 <- summary(df_describers_year$N); sum_p10
p10 <- ggplot(df_describers_year, aes(x=N)) + 
    geom_histogram(binwidth=1) +
        xlab("Number of species per taxonomist") + ylab("Frequency") + 
             theme +
                ggtitle("Number of species/taxonomist \n(V+S)") # in their lifetime
p10v <- p10 + scale_x_continuous(lim = c(-1, 10)) # for visualisation purposes
p10d <- ggplot_build(p10)$data[[1]]

## Species per author across years # TODO: N_species/N_author
# mean_N_spp_per_des <- df_describers_year[, list(mean_N_spp=mean(N)), 
#                                         by="date.n"]
# p12 <- ggplot(data=mean_N_spp_per_des, aes(x=date.n, y=mean_N_spp)) +
#     geom_line(stat="identity", size=1) + theme +
#     xlab("Year") + ylab("Number of species described") + 
#             ggtitle("Number of species described/ taxonomist by year") + 
#                 geom_smooth() + scale_y_continuous(lim = c(0, 12))
# summary(df_describers_year$N)


p13 <- ggplot(data=taxonomic_effort, aes(x=years, y=species_per_real_taxonomist)) +
    geom_line(stat="identity", size=1) + theme +
    xlab("Year") + ylab("Number of species described") + 
            ggtitle("Number of species described/ taxonomist by year") + 
                geom_smooth() + scale_y_continuous(lim = c(0, 12))



# Weighted taxonomists

## Correlation between species and taxonomists per year
c <- cor.test(taxonomic_effort[is.finite(N_weighted_real_describers)]$N_weighted_real_describers, 
              taxonomic_effort[is.finite(N_weighted_real_describers)]$N_species_described, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between taxonomists (wted) \nand species per year (R^2=", corr, ")")
p14 <- ggplot(taxonomic_effort, aes(x=N_weighted_real_describers, y=N_species_described)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of taxonomists (wted)") + ylab("Number of species") +
            theme  +
                ggtitle(title) + 
                    geom_smooth(method='lm',formula=y~x)

## Species per author across years
p15 <- ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_real_describers)) +
    geom_line(stat="identity", size=1) + theme +
    xlab("Year") + ylab("Number of taxonomists (wted)") + 
            ggtitle("Number of taxonomist (wted) by year") + 
                geom_smooth()

p16 <- ggplot(data=taxonomic_effort, aes(x=years, y=species_per_real_taxonomist_weighted)) +
    geom_line(stat="identity", size=1) + theme +
    xlab("Year") + ylab("Number of species \ndescribed/ taxonomist \n(wted) by year") + 
            ggtitle("Number of species described/ taxonomist (wted) by year") + 
                geom_smooth() 

sum_p17 <- summary(taxonomic_effort$N_weighted_real_describers); sum_p17
p17 <- ggplot(taxonomic_effort, aes(x=N_weighted_real_describers)) + 
    geom_histogram(binwidth=1) +
        xlab("Number of species per taxonomist (wted)") + ylab("Frequency") + 
             theme +
                ggtitle("Number of species/taxonomist (wted) \n(V+S)") # in their lifetime
p17v <- p17 + scale_x_continuous(lim = c(0, sum_p17["3rd Qu."])) # for visualisation purposes
p17d <- ggplot_build(p17)$data[[1]]


# TODO: work on using N_spp [to use valid species min and max? to exclude ]

# Combined plots: Fig S2
## Original plot
gr <- grid.arrange(p0, p3, p5, p6, ncol=2, nrow=2)
ggsave("plots/2019-07-17-edie-et-al1.png", gr, units="cm", width=20, height=18)
dev.off()

## Improved plot
grid.arrange(p0, p3, p8, ncol=1) # time series
grid.arrange(p7v, p10v) # histograms
grid.arrange(p6, p9) # correlation

# gr <- grid.arrange(p3, p8, p7v, p10v, p6, p9,
#                    widths=c(2, 1, 1),
#                    layout_matrix=rbind(c(2, 4, 6),
#                                        c(3, 5, 7)))
# grid.arrange(p3, p8, p6, p9, 
#             widths=c(2, 1),
#             layout_matrix=rbind(c(2, 6),
#                                 c(3, 7)))
# grid.arrange(p3, p8, p6, p9, p15, p14, 
#             widths=c(2, 1),
#             layout_matrix=rbind(c(1, 3),
#                                 c(2, 4),
#                                 c(5, 6)))
grid.arrange(p3, p8, p6, p9, p15, p14, p7v, p10v, p17v,
            widths=c(2, 1, 1),
            layout_matrix=rbind(c(1, 3, 7),
                                c(2, 4, 8),
                                c(5, 6, 9)))

grid.arrange(p5, p13, p16)
# grid.arrange(p12, p13)
# grid.arrange(p12, p13)

# Combined plots: Fig 1
## Original plot
p1


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Describers profile - one large monograph towards end of life, or many small?
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Describers profiles"))
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

write.csv(des4, paste0(dir_data, 'eda4_edie/2019-10-02-taxonomist-one-large-mono.csv'),
          row.names=F)

table(is.na(des$check_young))
table(is.na(des$check_few.spp))
table(is.na(des$check_few.spp.pub))
table(is.na(des$check_few.pub.near.death))

# Group that died young, few publications, few species, average number of species per pub
# Group that did not die young, few publications, few species, average number of species per pub
# Group that did not die young, large number of publications, large number of species, large number of species per pub
# Group that did not die young, few publications, large number of species {N and when}

# See if there is a trend of moving towards smaller publications 
# See if there are particular taxonomists who 


# Other biodata info # TODO: 
# Who described the most bees
tax_most_spp <- df_describers[,
    c("full.name.of.describer.n", "ns_spp_N", "n_pubs")][order(-ns_spp_N)]
summary(df_describers$ns_spp_N)
write.csv(tax_most_spp[1:10], 
          paste0(dir_data, 'eda4_edie/2019-10-03-tax-1-most-spp.csv'), row.names=F)

# Who had the most number of publications
tax_most_pub <- df_describers[, c("full.name.of.describer.n", "n_pubs")][order(-n_pubs)]
write.csv(tax_most_pub[1:10], 
          paste0(dir_data, 'eda4_edie/2019-10-03-tax-2-highest-pub.csv'), row.names=F)

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
          paste0(dir_data, 'eda4_edie/2019-10-03-tax-3-highest-syn.csv'), row.names=F)

