source('2021-06-02-jsa-type-v2-china/init.r')


#########################################################
# N species per year

rfile <- paste0(v2_dir_china, "01-map/lat-lon.csv")
df <- fread(rfile)

n_species <- df[china=="CHN" & status =="Valid species", .N, by="date"]
n_species$date <- as.integer(n_species$date)
n_species <- n_species[order(date)]
n_species$N <- cumsum(n_species$N)

p <- ggplot(n_species, aes(date, N)) + theme_minimal() +
    geom_smooth(fill=NA, color='black', size=1) +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks100, minor_breaks=ybreaks50) + 
    xlab("\nYear") + ylab("Number of valid species \n") 

ggsave(paste0(v2_dir_china, '03-time-series-01.png'), p, units="cm", width=15, height=8, dpi=300)

#########################################################
# N active authors per year

file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_2.csv")
df_des <- read_escaped_data_v2(file)

# Part 1: all valid species
des <- df_des[!is.na(max_corrected) & residence.country.describer.first_long == "China", 
    c(
        "full.name.of.describer", "min", "max_corrected", 
        "ns_species_per_year_active"
    )
]

# Expand dataset by min and max years
seq <- mapply(
    function(a, b) seq(a, b), 
    a = des$min, b = des$max_corrected
)

des$years <- seq
des <- data.table(des %>% unnest(years))
des$years <- as.integer(des$years)

# Calculate number of describers, and weighted number of describers
des$ns_species_per_year_active <- as.numeric(des$ns_species_per_year_active)

taxonomic_effort1 <- des[, list(
    N_describers = length(unique(full.name.of.describer)), 
    N_weighted_describers = sum(ns_species_per_year_active)
    ), 
by = years][order(years)]

p <- ggplot(taxonomic_effort1, aes(years, N_describers)) + theme_minimal() +
    geom_smooth(fill=NA, color='black', size=1) +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    xlab("\nYear") + ylab("Number of active describers\n") 

ggsave(paste0(v2_dir_china, '03-time-series-02.png'), p, units="cm", width=15, height=8, dpi=300)

p <- ggplot(taxonomic_effort1, aes(years, N_weighted_describers)) + theme_minimal() +
    geom_smooth(fill=NA, color='black', size=1) +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    xlab("\nYear") + ylab("Number of active describers\n") 

ggsave(
    paste0(v2_dir_china, '03-time-series-03.png'), p, 
    units="cm", width=15, height=8, dpi=300
)

#########################################################
