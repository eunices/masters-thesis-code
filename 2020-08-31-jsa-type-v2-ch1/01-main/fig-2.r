print(paste0(
  Sys.time(), " --- Taxonomic effort - publications (boxplot + violin plot)"
))

# Individual plots


## Mean number of species per publication by decade

median_N_spp_per_pub_decade <- df_publications_N[,
    list(median_N_spp=median(n_species)), by = "date.decade"
]

df_publications_N[date.decade == "1750s"]

calc_median <- function(x){
  return(c(y = -5, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

plot_tax_pub_decade <- 
    ggplot(data=df_publications_N, aes(x=date.decade, y=n_species)) +
        geom_violin(width=3, fill="grey", alpha=.7, color="grey40", trim=T) + 
        # geom_boxplot(width=0.2, outlier.size=NULL, fill="white") +
        geom_jitter(
            shape=16, position=position_jitter(0.2), 
            size=0.2, alpha=0.2, color='grey10'
        ) +
        geom_point(
            data=median_N_spp_per_pub_decade, 
            mapping=aes(x=date.decade, y=median_N_spp, group=1),
            size=2, color='black'
        ) +
        geom_line(
            data=median_N_spp_per_pub_decade, 
            mapping=aes(x=date.decade, y=median_N_spp, group=1),
            size=1, color='black'
        ) +
        # geom_line(
        #    data=median_N_spp_per_pub_decade, 
        #    mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
        #    size=1.3, color="red", alpha=0.9, linetype='dashed'
        # ) +
        scale_y_continuous(limit=c(-5, 40)) +
        # stat_summary(
        #    fun.data=calc_median, geom="text", fun=median,
        #    position=position_dodge(width = 0.75), size=3
        # ) +
        xlab("\nDecade") +
        ylab("Number of species described \nper publication\n") +
        theme


## Species per author across years

N_spp_per_des_decade <- df_describers_year[,
    list(mean_N_spp = mean(N), median_N_spp=median(N)), 
    by = "date.decade"
]

df_describers_year[date.decade == "1750s"]

plot_tax_des_decade <- 
    ggplot(data=df_describers_year, aes(x=date.decade, y=N)) +
        geom_violin(width=3, fill="grey", alpha=.7, color="grey40") + 
        # geom_boxplot(width=0.2, outlier.size=NULL, fill="white") +
        geom_jitter(shape=16, position=position_jitter(0.2), 
                    size=0.2, alpha=0.2, color='grey10') +
        # geom_line(
        #    data=median_N_spp_per_pub_decade, 
        #    mapping=aes(x=date.decade, y=mean_N_spp, group = 1),
        #    size=1.3, color="red", alpha=0.9, linetype='dashed'
        # ) +
        geom_point(
            data=N_spp_per_des_decade, 
            mapping=aes(x=date.decade, y=median_N_spp, group=1),
            size=2, color='black'
        ) +
        geom_line(
            data=N_spp_per_des_decade, 
            mapping=aes(x=date.decade, y=median_N_spp, group=1),
            size=1, color='black'
        ) +
        # geom_line(
        #    data=N_spp_per_des_decade[date.decade != "1750s"], 
        #    mapping=aes(x=date.decade, y=mean_N_spp, group=1),
        #    size=1, color='grey'
        # ) +
        scale_y_continuous(limit=c(-5, 40)) +
        # stat_summary(
        #    fun.data=calc_median, geom="text", fun=median,
        #    position=position_dodge(width = 0.75), size=3
        # ) +
        xlab("\nDecade") + 
        ylab("Number of species described \nper PTE\n") +
        theme

# Save plots
ggsave(paste0(dir_plot, 'fig-2a.png'), plot_tax_pub_decade, units="cm", width=21, height=8, dpi=300)
ggsave(paste0(dir_plot, 'fig-2b.png'), plot_tax_des_decade, units="cm", width=21, height=8, dpi=300)


################################################################################

# Trend analysis
median_N_spp_per_pub_decade[date.decade != "1750s"]$median_N_spp
N_spp_per_des_decade[date.decade != "1750s"]$median_N_spp

res <- MannKendall(median_N_spp_per_pub_decade[date.decade != "1750s"]$median_N_spp)
print("--------------------------")
print("Trend test: mean species per publication")
summary(res)

res <- MannKendall(N_spp_per_des_decade[date.decade != "1750s"]$median_N_spp)
print("--------------------------")
print("Trend test: mean species per describer")
summary(res)
