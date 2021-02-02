
# Information about code:
# This code corresponds to exploratory data analyses for my MSc thesis.
# They are pertaining to EDA for the authors 
# (relevant for Chapter 3, the section on Determinants of taxonomic resources flow).
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# Set up
source('2020-08-31-jsa-type-v2/00-init/main.R')
source('2020-08-31-jsa-type-v2/subset.r')

# Libraries
library(networkD3)
library(ggplot2)

# Parameters
theme = theme_classic()

dir_base = "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\"
dir_plot = paste0(dir_base, "_figures\\_ch3\\_ch3-flow\\")
dir_tables = paste0(dir_base, "_tables\\_ch3\\_ch3-flow\\")

# Read lookup
lookup.cty <- lu <- get_lp_statoid()

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - resource flow
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- resource flow"))

# Summarising where there is flow
spp <- get_df()

spp_s <- spp[
    duplicated == FALSE & status %in% c("Valid species", "Synonym"),
    c("idx", "type.country_n", "full.name.of.describer")
]; rm(spp)

spp_s <- data.table(spp_s %>% separate_rows(full.name.of.describer, sep="; "))

des <- get_des()
des <- des[, c("full.name.of.describer", "residence.country.describer.first")]

spp_s <- merge(spp_s, des, by="full.name.of.describer", all.x=T, all.y=F)

spp_s <- unique(spp_s[,
    c("type.country_n", "residence.country.describer.first", "idx")
])


t <- table(spp_s$type.country_n, spp_s$residence.country.describer.first)
t <- data.table(t)

dim(t); t <- t[N!=0]; dim(t)

names(t) <- c("type.country", "residence.country", "N")

dim(t); t <- t[!(type.country == "" | residence.country=="[unknown]")]; dim(t)

to_merge1 <- lu[, c("DL", "centroid_lat", "centroid_lon")]
to_merge2 <- lu[, c("DL", "centroid_lat", "centroid_lon")]

t <- merge(t, to_merge1, by.x="type.country", by.y="DL", all.x=T, all.y=F)

t <- merge(
    t, to_merge2, by.x="residence.country", by.y="DL", all.x=T, all.y=F,
    suffixes=c("_type.country", "_residence.country")
)

t[is.na(centroid_lat_type.country)]
t[is.na(centroid_lat_residence.country)]$residence.country
names(t) <- c("ori", "des", "N", "dY", "dX", "oY", "oX")

t$Geom <- paste0(
    "LINESTRING (", as.character(t$oX), 
    " ", as.character(t$oY), ", ", 
    as.character(t$dX), " ", as.character(t$dY), ")"
)

t$no_flow <- t$ori == t$des

# For map plotting
cfile <- paste0(v2_dir_data_ch3_flow, "2019-09-22-flow-map-type-loc-des-country.csv")
write.csv(t, cfile, na='', row.names=F, fileEncoding="UTF-8")

v2_shiny_dir_subfolder2 <- paste0(v2_dir_shiny, "eda1.1_shiny/data/")
dir.create(v2_shiny_dir_subfolder2, recursive=T)

cfile <- paste0(
    v2_shiny_dir_subfolder2, "2019-09-22-flow-map-type-loc-des-country.csv"
)

write.csv(t, cfile, na='', row.names=F, fileEncoding="UTF-8")

