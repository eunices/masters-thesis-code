# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Mean number of species/ author over the years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Mean number of species/ author over the years"))

# Using the data.frame of each describer and number of species they described,
# the mean and median number of species were obtained for each decade.
spp_per_auth <- df_describers_year[, 
    list(mean=mean(N), median=median(N)),
    by=c("date.decade")][order(date.decade)]

df_describers_year[date.decade=="1750s"]

plot_spp_per_auth_per_decade <- 
    ggplot(data=spp_per_auth, aes(x=date.decade, y=mean)) +
        geom_bar(stat='identity') + 
        xlab("\nDecade") + ylab("Mean number of species \ndescribed per PTE per year\n") +
        theme + scale_fill_grey()

ggsave(
    paste0(dir_plot, 'fig-3.png'), 
    plot_spp_per_auth_per_decade, units="cm", width=21, height=5, dpi=300
)




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Number of authors/ publication over the years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Number of authors/ publication over the years"))

n_auth <- df[, c("date", "date.decade", "idx", "full.name.of.describer")] %>% 
    separate_rows(full.name.of.describer, sep="; ")
n_auth <- data.table(n_auth)[order(idx)]
n_auth <- n_auth[, list(N=.N), by=c("date", "date.decade", "idx")]
n_auth$N <- as.character(n_auth$N)

calc_median <- function(x){
  return(c(y = -5, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

median_auth_per_sp_decade <- n_auth[, list(median_n_auth=median(as.numeric(N))),
                                      by='date.decade'][order(date.decade)]

plot_n_auth_per_decade <- 
    ggplot(data=n_auth, aes(x=date.decade, fill=N)) +
        geom_bar(position = "fill") + 
        xlab("\nDecade") + ylab("Proportion of number \nof PTEs per species description\n") +
        theme + scale_fill_grey()

ggsave(paste0(dir_plot, 'fig-4.png'), plot_n_auth_per_decade, units="cm", width=21, height=5, dpi=300)

