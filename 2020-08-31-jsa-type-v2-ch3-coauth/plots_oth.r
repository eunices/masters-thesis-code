# Information about code:
# This code corresponds to exploratory data analyses for my MSc thesis.
# They are pertaining to visualisation (shiny and networkd3) for the authors 
# (relevant for Chapter 3, the section on Coauthor networks).
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2020-08-31-jsa-type-v2/subset.r')

# Libraries
library(tidyverse)
library(networkD3)

# Parameters
date_cutoff <- 2019

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - prepare network data for shiny
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- prepare network data for shiny"))

# Note: these are data prep for shiny apps

# Get table of valid/ invalid species
spp <- get_df()[
    status %in% c("Valid species", "Synonym"),
    c("idx", "full.name.of.describer", "date")
]

dim(spp)

spp <- data.table(separate_rows(spp, full.name.of.describer, sep="; "))
spp_sum <- spp[date <= date_cutoff, .N, by=c('idx')][order(idx)]

rbind(table(spp_sum$N), round(prop.table(table(spp_sum$N))*100, 1))
round(dim(spp_sum[N>=2])[1]/ dim(spp_sum)[1]*100,1)
dim(spp_sum)[1]

# Get authors
cols <- c(
    "full.name.of.describer", "last.name", "alive", 
    "residence.country.describer.first", "describer.gender", 
    "n_pubs", "spp_N", "min", "max_corrected"
)


auth <- get_des()[, ..cols]
v2_shiny_dir_subfolder1 <- paste0(v2_dir_shiny, "eda2.1_shiny/data/")
dir.create(v2_shiny_dir_subfolder1, recursive = TRUE)

write.csv(
    auth, paste0(v2_shiny_dir_subfolder1, "authors.csv"), 
    na='', row.names=F, fileEncoding="UTF-8"
)

# Load data

nw <- get_describer_network()
nw <- nw[!(is.na(p1) | is.na(p2))]

write.csv(
    nw, paste0(v2_shiny_dir_subfolder1, "7.0-author-networks.csv"), 
    na='', row.names=F, fileEncoding="UTF-8"
)

col_des <- c(
    "full.name.of.describer", "last.name", 
    "describer.gender", "residence.country.describer", 
    "max_corrected", "min",
    "n_pubs", "spp_N", "spp_N_1st_auth_s", 
    "residence.country.describer.first"
)

# Country level summary
col_des_subset <- c(
    "residence.country.describer.first", 
    "full.name.of.describer"
)

nw_cty <- merge(
    nw, des[, ..col_des_subset],
    by.x="p1", by.y="full.name.of.describer", all.x=T, all.y=F
)

nw_cty <- merge(
    nw_cty, des[, ..col_des_subset], 
    by.x="p2", by.y="full.name.of.describer", all.x=T, all.y=F,
    suffixes=c("_p1", "_p2")
)

nw_cty <- nw_cty[, 
    list(N=sum(as.numeric(N))), 
    by=c(
        "residence.country.describer.first_p1", 
        "residence.country.describer.first_p2"
    )
]

nw_cty$check_same <- nw_cty$residence.country.describer.first_p1 == 
    nw_cty$residence.country.describer.first_p2

write.csv(
    nw_cty, 
    paste0(v2_dir_data_ch3_coauth, '2019-09-22-auth-country-network.csv'), 
    na='', row.names=F, fileEncoding="UTF-8"
)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - visualisation with NetworkD3
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- visualisation with NetworkD3"))

# Note: these are preliminary visualisations

# network
grp <- "residence.country.describer.first"
# grp <- "describer.gender"

authors <- des[residence.country.describer == "JA"]$full.name.of.describer
nw_f2 <- nw[p1 %in% authors | p2 %in% authors]

authors <- unique(c(nw_f2$p1, nw_f2$p2))
des2 <- des[full.name.of.describer %in% authors]
des2$idx <- seq(0, dim(des2)[1]-1, 1)

subset_col <- c("full.name.of.describer", "idx", "last.name", grp)

nw_f2 <- merge(
    nw_f2, des2[, ..subset_col], 
    by.x="p1", by.y="full.name.of.describer", 
    all.x=T, all.y=F
)

nw_f2 <- merge(
    nw_f2, des2[, ..subset_col], 
    by.x="p2", by.y="full.name.of.describer",
    all.x=T, all.y=F,
    suffixes=c("_p1", "_p2")
)

forceNetwork(
    Links = nw_f2, Nodes = des2,
    Source="p1", Target="p2", Value="N",
    NodeID="last.name", Group = grp,
    zoom=T, legend=T, opacityNoHover = 1, opacity=0.9,
    fontFamily="san-serif"
)

# sankey
threshold <- 1; nw_cty2 <- nw_cty[check_same == "FALSE" & N >= threshold]
nodes <- data.frame(
    label=unique(c(
        nw_cty2$residence.country.describer.first_p1, 
        nw_cty2$residence.country.describer.first_p2
    ))
)
nodes <- data.table(nodes); nodes <- nodes[order(label)]
nodes$idx <- seq(0, dim(nodes)[1]-1, 1)

nw_cty2 <- merge(
    nw_cty2, nodes, 
    by.x="residence.country.describer.first_p1", by.y="label", 
    all.x=T, all.y=F
)

nw_cty2 <- merge(
    nw_cty2, nodes, 
    by.x="residence.country.describer.first_p2", by.y="label",
    all.x=T, all.y=F, suffixes=c("_cty1", "_cty2")
)

sankeyNetwork(
    Links = nw_cty2, Nodes = nodes,
    Source="residence.country.describer.first_p2", 
    Target="residence.country.describer.first_p1", Value="N",
    NodeID="label", fontSize = 16, unit = "Species described collaboratively"
)

# http://www.sthda.com/english/articles/33-social-network-analysis/137-interactive-network-visualization-using-r/
# https://www.r-graph-gallery.com/network-interactive.html


