# Set up
source('2019-06-19-jsa-type-ch1/init/init.r')

# Analyses
#############

# Parameters
theme <- theme_minimal()

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
    geom_vline(xintercept=9, color='grey', size=1) +
    geom_vline(xintercept=summary(yrs_active1)[5], color='grey', size=1, linetype="dashed") +
    geom_vline(xintercept=summary(yrs_active1b)[5], color='grey', size=1, linetype="dotted") +
    xlab("\nNumber of active years") + ylab("Proportion of PTEs (%)\n") + 
    theme

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
# Section - Prop species describing <=N species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Species richness and area graph"))

taxonomic_effort$N_real_describers.1 <- taxonomic_effort$N_real_describers.1 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.2 <- taxonomic_effort$N_real_describers.2 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.3 <- taxonomic_effort$N_real_describers.3 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.4 <- taxonomic_effort$N_real_describers.4 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.5 <- taxonomic_effort$N_real_describers.5 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.6 <- taxonomic_effort$N_real_describers.6 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.7 <- taxonomic_effort$N_real_describers.7 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.8 <- taxonomic_effort$N_real_describers.8 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.9 <- taxonomic_effort$N_real_describers.9 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.10 <- taxonomic_effort$N_real_describers.10 / taxonomic_effort$N_real_describers * 100

cols <- names(taxonomic_effort)[grepl("N_real_describers.", names(taxonomic_effort))]
cols <- c("years", cols)
des_y <- melt(taxonomic_effort[, ..cols], id.vars="years")

formatstr <- function(string) {
  string <- gsub("N_real_describers.", "", string)
  ifelse(string=="1", string, paste0("<=", string))
}

ggplot(des_y, aes(x=years, y=value, group=variable)) + 
    geom_line(size=1, colour="grey") + geom_smooth(colour="black") +
    xlab("Year") + ylab("Proportion of PTEs describing <= N species (%)\n") +
    facet_wrap(. ~variable, ncol=2, labeller=labeller(variable=formatstr), dir="v") +
    scale_y_continuous(limits=c(0, 50)) +
    theme

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
pt_sum <- data.table(species_per_year2)[, list(max=max(value)), by=c("variable")]
pts <- data.frame(date.n=rep(c(1914, 1919, 1939, 1945), 2), 
                  variable=c(rep("N_cumsum", 4), rep("N", 4)),
                  value=c(rep(pt_sum[variable=="N_cumsum",]$max, 4), rep(pt_sum[variable=="N",]$max, 4)))
# World War I 1914-1919; World War II 1939-1945

labs <- c(`N` = "N species",
          `N_cumsum` = "Cumulative N species")
p1 <- ggplot(species_per_year2, aes(x=date.n, y=value)) + 
    facet_wrap(.~variable, nrow=2, scales = "free_y", labeller= as_labeller(labs)) +
    geom_ribbon(pts[c(1,2,5,6),], mapping=aes(x=date.n, ymin=0, ymax=value), fill="red", alpha=0.2) +
    geom_ribbon(pts[c(1,2,5,6),], mapping=aes(x=date.n, ymin=0, ymax=value), fill="red", alpha=0.2) +
    geom_ribbon(pts[c(3,4,7,8),], mapping=aes(x=date.n, ymin=0, ymax=value), fill="red", alpha=0.2) +
    geom_ribbon(pts[c(3,4,7,8),], mapping=aes(x=date.n, ymin=0, ymax=value), fill="red", alpha=0.2) +
    geom_line(size=1) + geom_smooth() +
        xlab("") + ylab("") +
            theme

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
        xlab("") + ylab("Number of species/ publication") + 
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

# Number of describers 
# Per year
p8 <- ggplot(taxonomic_effort, aes(x=years, y=N_real_describers)) + 
    geom_line(size=1) + 
        xlab("") + ylab("Number of PTEs") + 
            theme +
                ggtitle("Number of PTEs by year") + geom_smooth()


## Correlation between species and PTEs per year
c <- cor.test(taxonomic_effort$N_real_describers, taxonomic_effort$N_species_described, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between PTEs \nand species per year (R^2=", corr, ")")
p9 <- ggplot(taxonomic_effort, aes(x=N_real_describers, y=N_species_described)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of PTEs") + ylab("Number of species") +
            theme  +
                ggtitle(title) + 
                    geom_smooth(method='lm',formula=y~x)

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

## Species per author across years # TODO: N_species/N_author
# mean_N_spp_per_des <- df_describers_year[, list(mean_N_spp=mean(N)), 
#                                         by="date.n"]
# p12 <- ggplot(data=mean_N_spp_per_des, aes(x=date.n, y=mean_N_spp)) +
#     geom_line(stat="identity", size=1) + theme +
#     xlab("Year") + ylab("Number of species described") + 
#             ggtitle("Number of species described/ PTE by year") + 
#                 geom_smooth() + scale_y_continuous(lim = c(0, 12))
# summary(df_describers_year$N)


p13 <- ggplot(data=taxonomic_effort, aes(x=years, y=species_per_real_taxonomist)) +
    geom_line(stat="identity", size=1) + theme +
    xlab("Year") + ylab("Number of species/ PTE") + 
            ggtitle("Number of species described/ PTE by year") + 
                geom_smooth() + scale_y_continuous(lim = c(0, 12))

# Weighted PTEs

## Correlation between species and PTEs per year
c <- cor.test(taxonomic_effort[is.finite(N_weighted_real_describers)]$N_weighted_real_describers, 
              taxonomic_effort[is.finite(N_weighted_real_describers)]$N_species_described, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between PTEs (wted) \nand species per year (R^2=", corr, ")")
p14 <- ggplot(taxonomic_effort, aes(x=N_weighted_real_describers, y=N_species_described)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of PTEs (wted)") + ylab("Number of species") +
            theme  +
                ggtitle(title) + 
                    geom_smooth(method='lm',formula=y~x)

## Species per author across years
p15 <- ggplot(data=taxonomic_effort, aes(x=years, y=N_weighted_real_describers)) +
    geom_line(stat="identity", size=1) + theme +
    xlab("Year") + ylab("Number of PTEs (wted)") + 
            ggtitle("Number of PTEs (wted) by year") + 
                geom_smooth()

p16 <- ggplot(data=taxonomic_effort, aes(x=years, y=species_per_real_taxonomist_weighted)) +
    geom_line(stat="identity", size=1) + theme +
    xlab("Year") + ylab("Number of species \ndescribed/ PTE \n(wted)") + 
            ggtitle("Number of species described/ PTE (wted) by year") + 
                geom_smooth() 

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
title <- paste0("Correlation between PTEs \nand publications per year (R^2=", 
                corr, ")")
p18 <- ggplot(pub_auth_yr, aes(x=N_real_describers, y=N_publications)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of PTEs") + ylab("Number of publications") +
            theme  +
                ggtitle(title) + 
                    geom_smooth(method='lm',formula=y~x)

c <- cor.test(pub_auth_yr[is.finite(N_weighted_real_describers)]$N_weighted_real_describers, 
              pub_auth_yr[is.finite(N_weighted_real_describers)]$N_publications, 
              method = c("pearson"))
corr <- round(c$estimate^2, 2)
title <- paste0("Correlation between PTEs (wted) \nand publications per year (R^2=", 
                corr, ")")
p19 <- ggplot(pub_auth_yr, aes(x=N_weighted_real_describers, y=N_publications)) + 
    geom_point(alpha=0.5) + 
        xlab("Number of PTEs (wted)") + ylab("Number of publications") +
            theme  +
                ggtitle(title) + 
                    geom_smooth(method='lm',formula=y~x)

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

# Draft 1
grid.arrange(p3, p8, p6, p9, p15, p14, 
            widths=c(2, 1, 1),
            layout_matrix=rbind(c(1, 3),
                                c(2, 4),
                                c(5, 6)))

# Draft 2
grid.arrange(p3, p8, p6, p9, p15, p14, p7v, p10v, p17v,
            widths=c(2, 1, 1),
            layout_matrix=rbind(c(1, 3, 7),
                                c(2, 4, 8),
                                c(5, 6, 9)))

# Draft 3
p <- ggplot() + theme
grid.arrange(p3, p8, p6, p9, p15, p14, p18, p19, p,
            widths=c(2, 1, 1),
            layout_matrix=rbind(c(1, 3, 9),
                                c(2, 4, 7),
                                c(5, 6, 8)))

## Time series
grid.arrange(p5, p13, p16)
# grid.arrange(p12, p13)
# grid.arrange(p12, p13)

# Combined plots: Fig 1
p1


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Taxonomic effort - publications (boxplot + violin plot)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Taxonomic effort - publications (boxplot + violin plot)"))

# Individual plots

## Mean number of species per publication by decade
median_N_spp_per_pub_decade <- df_publications_N[, list(mean_N_spp=median(n_species)), 
                                               by="date.decade"]

calc_median <- function(x){
  return(c(y = -5, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

plot_tax_pub_decade <- 
    ggplot(data=df_publications_N, aes(x=date.decade, y=n_species)) +
        geom_violin(width=3, fill="bisque") + 
        geom_boxplot(width=0.2, outlier.size=NULL, fill="white") +
        geom_jitter(shape=16, position=position_jitter(0.2), 
                    size=0.1, alpha=0.2, color='grey10') +
        # geom_line(data=median_N_spp_per_pub_decade, 
        #           mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
        #           size=1.3, color="red", alpha=0.9, linetype='dashed') +
        geom_smooth(data=median_N_spp_per_pub_decade[date.decade != "1750s"], 
                    mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
                    size=1.3) +
        scale_y_continuous(limit=c(-5, 40)) +
        stat_summary(fun.data = calc_median, geom = "text", fun.y = median,
                     position = position_dodge(width = 0.75)) +
        xlab("\nDecade") + ylab("Number of species described \nper publication\n") +
        theme

# Combined plots
plot_tax_pub_decade

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Describers profile - one large monograph towards end of life, or many small?
# For appendix
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

# Other biodata info
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

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Taxonomic effort paragraph
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Taxonomic effort paragraph"))

# Count number of statuses
df2 <- get_df2(write=F)[,c("idx", "status", "date.n")]
df_years <- rbind(df[, c("idx", "status", "date.n")], df2)
status <- table(df_years$status)
sum(status)
round(prop.table(status)*100, 1)

# Count by author-years
df_tax <- df_describers[spp_N_1st_auth_s>0]
dim(df_tax)
df_tax$diff <- df_tax$max - df_tax$ns_max
tax_only_non_valid <- df_tax[ns_spp_N==0]$full.name.of.describer.n
df_tax$non_valid_spp_N <- df_tax$spp_N - df_tax$ns_spp_N
tax_valid_and_non_valid <-  df_tax[ns_spp_N > 0 & non_valid_spp_N > 0]$full.name.of.describer.n

# Total number of author-years
total_author_years <- dim(df_describers_year)[1]

# Author-years: Only non-valid species
length(tax_only_non_valid)
length(df_describers_year[full.name.of.describer %in% tax_only_non_valid]$N)
length(df_describers_year[full.name.of.describer %in% tax_only_non_valid]$N) / total_author_years * 100

# Author-years: At least valid species and at least 1 non-valid species
length(tax_valid_and_non_valid)
round(prop.table(table(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid]$diff==0))*100, 1)
summary(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid & diff >0]$diff)
sum(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid & diff >0]$diff)

# Did not shift the mean
summary(df_describers_year$N)
summary(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid)]$N)
summary(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid, tax_valid_and_non_valid)]$N)

p1 <- ggplot(df_describers_year) + 
    geom_density(aes(x = N), alpha = 0.2) + 
    xlim(c(0, 20)) + theme
p2 <- ggplot(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid)]) +
    geom_density(aes(x = N), alpha = 0.2) + xlim(c(0,20)) + theme

grid.arrange(p1, p2)

(721+524)/ total_author_years


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


