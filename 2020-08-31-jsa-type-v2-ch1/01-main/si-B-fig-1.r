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
yrs_active1b <- tax[!is.na(dod.describer),]
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
    geom_vline(
        xintercept=summary(yrs_active1)[5], 
        color='grey', size=.5, linetype="dashed"
    ) +
    geom_vline(
        xintercept=summary(yrs_active1b)[5], 
        color='grey', size=.5, linetype="dotted"
    ) +
    xlab("\nNumber of active years") + ylab("Proportion of PTEs (%)\n") + 
    theme

ggsave(
    paste0(dir_plot, '_si/si-B-fig-1.png'), hist_active_yrs,
    units="cm", width=10, height=8, dpi=300
)

# Active based on max publication dates
yrs_active2 <- tax$max - tax$min

# Based on sum of authors that described a species
yrs_active3 <- df[, c("date", "full.name.of.describer")] %>% 
    separate_rows(full.name.of.describer, sep="; ")

yrs_active3 <- data.table(unique(yrs_active3))
yrs_active3 <- yrs_active3[, .N, by='date'][order(date)]
yrs_active3$date <- as.integer(yrs_active3$date)

plot_tax_effort5 <- ggplot(data=yrs_active3, aes(x=date, y=N)) +
    geom_line() + theme +
    xlab("Number of PTEs") + ylab("Year") + 
    ggtitle(paste0("Number of PTEs in each year"))

# Mean number of species/year 
hist_mean_sp_per_auth <- ggplot(df_describers) +
    geom_histogram(
        mapping=aes(x=ns_species_per_year_active, 
        y=..count../sum(..count..) * 100), binwidth=5) +
    theme

