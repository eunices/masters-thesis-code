print(paste0(
  Sys.time(), " --- Taxonomic effort - publications (boxplot + violin plot)"
))

# Individual plots


## Mean number of species per publication by decade

# Used in chapter 2 as supporting information

p <- ggplot(df_publications_N) +
    theme_minimal() +
    geom_histogram(aes(x=n_species), binwidth=1) +
    xlab('\n Number of species') + ylab('Number of publications (log scale)\n') 
f <- paste0(dir_plot2, 'n-species-pub.png')
ggsave(f, p, units="cm", width=16, height=12, dpi=300)


N_spp_per_pub_decade <- df_publications_N[,
    list(
        mean_N_spp = mean(n_species), 
        median_N_spp = as.double(median(n_species)),
        perc_75 = quantile(n_species, .75)
    ), 
    by = "date.decade"
][order(date.decade)]


N_spp_per_des_decade[, c('date.decade', 'median_N_spp')]

N_spp_per_pub_decade

df_publications_N[date.decade == "1750s"]

calc_median <- function(x){
  return(c(y = -5, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

x_axis <- unique(df_publications_N$date.decade)
x_axis[rep(c(FALSE, TRUE), length(x_axis)/2)] <- ""

plot_tax_pub_decade <- 
    ggplot(data=df_publications_N, aes(x=date.decade, y=n_species)) +
        geom_line(
            data=N_spp_per_pub_decade, 
            mapping=aes(x=date.decade, y=median_N_spp, group=1),
            size=1, color='grey70'
        ) +
        geom_boxplot(width=.5, outlier.size=NULL, fill="white", outlier.shape=NA) +
        geom_jitter(shape=16, position=position_jitter(0.2), 
                    size=0.2, alpha=0.2, color='grey10') +
        scale_y_continuous(limit=c(-5, 40)) +
        scale_x_discrete(breaks = x_axis) + 
        xlab("\nDecade") + 
        ylab("Number of species described \nper publication\n") +
        theme

# Old plot
# plot_tax_pub_decade <- 
#     ggplot(data=df_publications_N, aes(x=date.decade, y=n_species)) +
#         geom_violin(width=3, fill="grey", alpha=.7, color="grey40", trim=T) + 
#         # geom_boxplot(width=0.2, outlier.size=NULL, fill="white") +
#         geom_jitter(
#             shape=16, position=position_jitter(0.2), 
#             size=0.2, alpha=0.2, color='grey10'
#         ) +
#         geom_point(
#             data=N_spp_per_pub_decade, 
#             mapping=aes(x=date.decade, y=median_N_spp, group=1),
#             size=2, color='black'
#         ) +
#         geom_line(
#             data=N_spp_per_pub_decade, 
#             mapping=aes(x=date.decade, y=median_N_spp, group=1),
#             size=1, color='black'
#         ) +
#         # geom_line(
#         #    data=N_spp_per_pub_decade, 
#         #    mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
#         #    size=1.3, color="red", alpha=0.9, linetype='dashed'
#         # ) +
#         scale_y_continuous(limit=c(-5, 40)) +
#         # stat_summary(
#         #    fun.data=calc_median, geom="text", fun=median,
#         #    position=position_dodge(width = 0.75), size=3
#         # ) +
#         scale_x_discrete(breaks = x_axis) + 
#         xlab("\nDecade") +
#         ylab("Number of species described \nper publication\n") +
#         theme


## Species per author across years

N_spp_per_des_decade <- df_describers_year[,
    list(
        mean_N_spp = mean(N), 
        median_N_spp = as.double(median(N)),
        perc_75 = quantile(N, .75)
    ), 
    by = "date.decade"
][order(date.decade)]

# https://stackoverflow.com/questions/12125364 median issue

df_describers_year[date.decade == "1750s"]

plot_tax_des_decade <- 
    ggplot(data=df_describers_year, aes(x=date.decade, y=N)) +
        # geom_violin(width=3, fill="grey", alpha=.7, color="grey40") + 
        # geom_point(
        #     data=N_spp_per_des_decade, 
        #     mapping=aes(x=date.decade, y=median_N_spp, group=1),
        #     size=2, color='black', shape=1
        # ) +
        geom_line(
            data=N_spp_per_des_decade, 
            mapping=aes(x=date.decade, y=median_N_spp, group=1),
            size=1, color='grey70'
        ) +
        # geom_line(
        #    data=N_spp_per_des_decade[date.decade != "1750s"], 
        #    mapping=aes(x=date.decade, y=mean_N_spp, group=1),
        #    size=1, color='grey'
        # ) +
        geom_boxplot(width=.5, outlier.size=NULL, fill="white", outlier.shape=NA) +
        geom_jitter(shape=16, position=position_jitter(0.2), 
            size=0.2, alpha=0.2, color='grey10') +
        scale_y_continuous(limit=c(-5, 40)) +
        scale_x_discrete(breaks = x_axis) + 
        # stat_summary(
        #    fun.data=calc_median, geom="text", fun=median,
        #    position=position_dodge(width = 0.75), size=3
        # ) +
        xlab("\nDecade") + 
        ylab("Number of species described \nper PTE\n") +
        theme

# Save plots
ggsave(paste0(dir_plot, 'fig-2a.png'), plot_tax_pub_decade, units="cm", width=16, height=6, dpi=300)
ggsave(paste0(dir_plot, 'fig-2b.png'), plot_tax_des_decade, units="cm", width=16, height=6, dpi=300)


################################################################################

# Trend analysis
N_spp_per_pub_decade[date.decade != "1750s"]$median_N_spp
N_spp_per_des_decade[date.decade != "1750s"]$median_N_spp

res <- MannKendall(
    N_spp_per_pub_decade[date.decade != "1750s"]$median_N_spp
)

print("--------------------------")
print("Trend test: mean species per publication")
summary(res)

res <- MannKendall(N_spp_per_des_decade[date.decade != "1750s"]$median_N_spp)
print("--------------------------")
print("Trend test: mean species per describer")
summary(res)
