source('2021-06-02-jsa-type-v2-china/init.r')


#########################################################
# N species per year


# By country
rfile <- paste0(v2_dir_china, "01-map/lat-lon.csv")
df <- fread(rfile)

df_latlon <- df[!(is.na(lat) | is.na(lon))]

n_species <- df_latlon[china=="CHN" & status =="Valid species", .N, by="date"]
n_species$date <- as.integer(n_species$date)
n_species <- n_species[order(date)]
n_species$N <- cumsum(n_species$N)

p <- ggplot(n_species, aes(date, N)) + theme_minimal() +
    geom_path(color='black', size=1) +
    # geom_smooth(fill=NA, color='black', size=1) +
    # geom_point(size=1, color='grey') + 
    # geom_line(size=.5, color='grey', linetype='dashed') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks100, minor_breaks=ybreaks50) + 
    xlab("\nYear") + ylab("Number of valid species \n") 

ggsave(paste0(v2_dir_china, '03-time-series-01a.png'), p, units="cm", width=15, height=8, dpi=300)


# By pri div
n_species <- df_latlon[china=="CHN" & status =="Valid species", ]
x <- n_species[,.N, by=pri][N>10]
n_species <- n_species[pri %in% x$pri,.N, by=c("date", "pri")]

n_species$date <- as.integer(n_species$date)
n_species <- n_species[order(pri, date)]

template <- expand.grid(date=seq(min(n_species$date), max(n_species$date)), pri=x$pri)
n_species <- merge(template, n_species, by=c("pri", "date"), all.x=T, all.y=F)
n_species <- data.table(n_species)
n_species[is.na(N)]$N <- 0 
n_species[, N := cumsum(N), by="pri"]

n_species$pri <- as.character(n_species$pri)
n_species$pri <- factor(n_species$pri, levels=sort(unique(n_species$pri)))

p <- ggplot(n_species, aes(date, N)) + theme_minimal() +
    # geom_smooth(fill=NA, color='black', size=1) +
    # geom_point(size=1, color='grey') + 
    geom_path(color='black', size=1) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks20, limits=c(0, 100)) +
    # scale_y_continuous(breaks=ybreaks100, minor_breaks=ybreaks50) + 
    facet_wrap(. ~ pri, ncol=3) +
    xlab("\nYear") + ylab("Number of valid species \n") 

ggsave(paste0(v2_dir_china, '03-time-series-01c.png'), p, units="cm", width=15, height=18, dpi=300)


# Using Ascher's fields



# CH, HK, MC

# Valid species
n_species <- df[type.country_n %in% c("CH", "HK", "MC"), .N, by=c("date", "status")]
n_species$date <- as.integer(n_species$date)
n_species <- n_species[order(status, date)]
n_species[, N := cumsum(N), by="status"]
unique(n_species$status)

p <- ggplot(n_species, aes(date, N, color=status)) + theme_minimal() +
    geom_path(size=1) +
    # geom_smooth(fill=NA, color='black', size=1) +
    # geom_point(size=1, color='grey') + 
    # geom_line(size=.5, color='grey', linetype='dashed') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks100, minor_breaks=ybreaks50) + 
    xlab("\nYear") + ylab("Number of species \n") +
    labs(color="")

ggsave(paste0(v2_dir_china, '03-time-series-01a-new.png'), p, units="cm", width=15, height=8, dpi=300)

# Species accumulation curve

n_species <- df[status =="Valid species"]
n_species$mapper_cty<- lapply(n_species$global.mapper_n, function(x) strsplit(x, "; ")[[1]])
n_species <- data.table(n_species %>% unnest(mapper_cty))
n_species <- n_species[mapper_cty %in% c("CH", "HK", "MC")]
n_species$mapper_cty <- NULL
n_species <- unique(n_species)

n_species <- n_species[, .N, by=c("date")]
n_species$date <- as.integer(n_species$date)
n_species <- n_species[order(date)]
n_species[, N := cumsum(N), ]
unique(n_species$status)

p <- ggplot(n_species, aes(date, N)) + theme_minimal() +
    geom_path(color="black", size=1) +
    # geom_smooth(fill=NA, color='black', size=1) +
    # geom_point(size=1, color='grey') + 
    # geom_line(size=.5, color='grey', linetype='dashed') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks200, minor_breaks=ybreaks100) + 
    xlab("\nYear") + ylab("Number of valid species \n") +
    labs(color="")

ggsave(paste0(v2_dir_china, '03-time-series-01b-new.png'), p, units="cm", width=15, height=8, dpi=300)







# CH, HK, MC + PG, TW, IN:AR

# Valid species
n_species <- df[
    type.country_n %in% c("CH", "HK", "MC", "PG", "TW") | 
    (type.country_n == "IN" & type.state == "AR"), .N, by=c("date", "status")
]

n_species$date <- as.integer(n_species$date)
n_species <- n_species[order(status, date)]
n_species[, N := cumsum(N), by="status"]
unique(n_species$status)

p <- ggplot(n_species, aes(date, N, color=status)) + theme_minimal() +
    geom_path(size=1) +
    # geom_smooth(fill=NA, color='black', size=1) +
    # geom_point(size=1, color='grey') + 
    # geom_line(size=.5, color='grey', linetype='dashed') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks100, minor_breaks=ybreaks50) + 
    xlab("\nYear") + ylab("Number of species \n") +
    labs(color="")

ggsave(paste0(v2_dir_china, '03-time-series-01a-max.png'), p, units="cm", width=15, height=8, dpi=300)


# Species accumulation curve

n_species <- df[status =="Valid species"]

india <- n_species[grepl("IN:AR", global.mapper) | grepl("IN:AR", merged.global.mapper)]

n_species$mapper_cty <- lapply(n_species$global.mapper_n, function(x) strsplit(x, "; ")[[1]])
n_species <- data.table(n_species %>% unnest(mapper_cty))
n_species <- n_species[mapper_cty %in% c("CH", "HK", "MC", "PG", "TW")]
n_species$mapper_cty <- NULL
n_species <- unique(n_species)

n_species <- unique(rbind(n_species, india))

n_species <- n_species[, .N, by=c("date")]
n_species$date <- as.integer(n_species$date)
n_species <- n_species[order(date)]
n_species[, N := cumsum(N), ]
unique(n_species$status)

p <- ggplot(n_species, aes(date, N)) + theme_minimal() +
    geom_path(color="black", size=1) +
    # geom_smooth(fill=NA, color='black', size=1) +
    # geom_point(size=1, color='grey') + 
    # geom_line(size=.5, color='grey', linetype='dashed') +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks100) + 
    xlab("\nYear") + ylab("Number of valid species \n") +
    labs(color="")

ggsave(paste0(v2_dir_china, '03-time-series-01b-max.png'), p, units="cm", width=15, height=8, dpi=300)








# By pri div
dir_geo_chn <- "data/geo_processed/gadm/china/gadm36_CHN_shp/"
v_chn_pri <- st_read(paste0(dir_geo_chn, "gadm36_CHN_1.shp"))
lp_chn_pri <- data.table(v_chn_pri)[, c("HASC_1", "NAME_1")]
lp_chn_pri$lp_pri <- unlist(lapply(lp_chn_pri$HASC_1, function(x) strsplit(x, "\\.")[[1]][[2]]))
lp_chn_pri$HASC_1 <- NULL

n_species <- df[type.country_n=="CH",]
n_species[grepl("SN", type.state)]$type.state <- "SA"
n_species[grepl(";", type.state)]$type.state <- unlist(lapply(n_species[grepl(";", type.state)]$type.state, function(x) strsplit(x, ";")[[1]][[1]]))
n_species <- merge(n_species, lp_chn_pri, by.x="type.state", by.y="lp_pri", all.x=T, all.y=F)

n_species[is.na(NAME_1), .N]
n_species <- n_species[!is.na(NAME_1)]
# 18 species without state excluded

x <- n_species[,.N, by=c("NAME_1")][N>10]
n_species <- n_species[NAME_1 %in% x$NAME_1,.N, by=c("date", "NAME_1", "status")]
n_species$date <- as.integer(n_species$date)
n_species <- n_species[order(status, NAME_1, date)]

template <- expand.grid(date=seq(min(n_species$date), max(n_species$date)), NAME_1=x$NAME_1, status=unique(df$status))
n_species <- merge(template, n_species, by=c("NAME_1", "status", "date"), all.x=T, all.y=F)
n_species <- data.table(n_species)
n_species[is.na(N)]$N <- 0 
n_species[, N := cumsum(N), by=c("status", "NAME_1")]

n_species$NAME_1 <- as.character(n_species$NAME_1)
n_species$NAME_1 <- factor(n_species$NAME_1, levels=sort(unique(n_species$NAME_1)))

p <- ggplot(n_species, aes(date, N, color=status)) + theme_minimal() +
    # geom_smooth(fill=NA, color='black', size=1) +
    # geom_point(size=1, color='grey') + 
    geom_path(size=1) +
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(breaks=ybreaks20, limits=c(0, 80)) +
    # scale_y_continuous(breaks=ybreaks100, minor_breaks=ybreaks50) + 
    facet_wrap(. ~ NAME_1, ncol=3) +
    xlab("\nYear") + ylab("Number of valid species \n") +
    labs(color="")

ggsave(paste0(v2_dir_china, '03-time-series-01c-new.png'), p, units="cm", width=18, height=18, dpi=300)




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

df_des[grepl("Orr", full.name.of.describer)]

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
