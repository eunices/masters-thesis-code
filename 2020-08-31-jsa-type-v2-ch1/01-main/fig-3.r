print(paste0(Sys.time(), " --- Histogram of PTEs total spp described"))


# Tabulate statistics of years active

tax <- df_describers[spp_N_1st_auth_s>=1]
highlight_auth <- tax[ns_spp_N > 750]$full.name.of.describer

tax_highlight <- tax[
    full.name.of.describer %in% highlight_auth
][,c("ns_spp_N", "last.name")]

x_axis <- seq(0, max(tax$ns_spp_N), 1000)
x_axis_minor <- seq(0, max(tax$ns_spp_N), 100)

hist_tl_spp <- ggplot(tax, aes(x=ns_spp_N)) +
    geom_histogram(
        mapping=aes(y=..count../sum(..count..) * 100), 
        fill='grey30', binwidth=100
    ) + 
    geom_vline(xintercept=median(tax$ns_spp_N), color='grey', size=.5) +
    xlab("\nTotal number of valid species \ndescribed, by PTE") + 
    ylab("Percentage of PTEs (%)\n") + 
    geom_label_repel(
        data=tax_highlight, 
        aes(x=ns_spp_N, y=.1, label=last.name),
        size=2, nudge_x=10, nudge_y=30,
        fontface='bold', color='black', segment.color='grey80', force=1,
        box.padding = unit(0.001, 'lines')
    ) +
    scale_x_continuous(breaks=x_axis, minor_breaks=x_axis_minor) +
    # scale_y_continuous(breaks= seq(0, 12, 1), limits=c(0, 12)) +
    theme

ggsave(
    paste0(dir_plot, 'fig-3a.png'), hist_tl_spp, units="cm", 
    width=6.5, height=6.5, dpi=300
)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Histogram of PTEs mean sp described per year
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Histogram of PTEs mean sp described per year"))

# Tabulate statistics of years active
tax <- df_describers[spp_N_1st_auth_s>=1]

# Highlight auths from previous section
tax_highlight <- tax[
    full.name.of.describer %in% highlight_auth
][ ,c("ns_species_per_year_active", "last.name")]

hist_mean_spp <- ggplot(tax, aes(x=ns_species_per_year_active)) +
    geom_histogram(
        mapping=aes(y=..count../sum(..count..) * 100), 
        fill='grey30', binwidth=1
    ) + 
    geom_vline(
        xintercept=median(tax$ns_species_per_year_active), 
        color='grey', size=.5
    ) +
    xlab("\nMean number of species \ndescribed per year, by PTE") +
    ylab("Percentage of PTEs (%)\n") + 
    scale_x_continuous(
        breaks= seq(0, max(tax$ns_species_per_year_active), 10)
    ) +
    geom_label_repel(
        data=tax_highlight, 
        aes(x=ns_species_per_year_active, y=.1, label=paste0(last.name, " (", round(ns_species_per_year_active, 0),")")), 
        size=2, nudge_x=30, nudge_y=30,
        fontface='bold', color='black', segment.color='grey80', force=5,
        box.padding = unit(0.001, 'lines')
    ) + theme

ggsave(
    paste0(dir_plot, 'fig-3b.png'), hist_mean_spp, units="cm", 
    width=6.5, height=6, dpi=300
)
