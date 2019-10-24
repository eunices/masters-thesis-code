source('2019-06-19-ascher-type-data/subset.r')

library(tidyverse)
library(networkD3)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - coauthor network
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- coauthor network"))

# Get authors
cols <- c("full.name.of.describer.n", "last.name", "alive", "residence.country.describer.first",
          "describer.gender.n", "n_pubs", "spp_N", "min", "max_corrected")
auth <- get_des(write=F)[, ..cols]
write.csv(auth, paste0(dir_script, "eda2.1_shiny/data/authors.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")

# Load data
nw <- fread(
    paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_7.0-author-networks.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
nw[, names(nw) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes
nw <- nw[!(is.na(p1) | is.na(p2))]

write.csv(nw, paste0(dir_script, "eda2.1_shiny/data/7.0-author-networks.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")

split_cty <- function(x) {
    strsplit(x, split="; ")[[1]][1]
}
col_des <- c("idx_auth", "full.name.of.describer.n", "last.name", 
             "describer.gender.n", "residence.country.describer.n", "max_corrected", "min",
             "n_pubs", "spp_N", "spp_N_1st_auth_s")

des <- get_des(write=F)[, ..col_des]
des$idx_auth <- as.numeric(des$idx_auth) -1
des$residence.country.describer.first <- sapply(des$residence.country.describer.n, split_cty)

cols_des_join <- c("idx_auth", "full.name.of.describer.n")
nw_f <- merge(nw, des[, ..cols_des_join],
              by.x="p1", by.y="full.name.of.describer.n", all.x=T, all.y=F)
nw_f <- merge(nw_f, des[, ..cols_des_join],
              by.x="p2", by.y="full.name.of.describer.n", all.x=T, all.y=F, suffixes=c("_p1", "_p2"))


# Country level summary
col_des_subset <- c("residence.country.describer.first", "full.name.of.describer.n")
nw_cty <- merge(nw, des[, ..col_des_subset],
                by.x="p1", by.y="full.name.of.describer.n", all.x=T, all.y=F)
nw_cty <- merge(nw_cty, des[, ..col_des_subset], 
                by.x="p2", by.y="full.name.of.describer.n", all.x=T, all.y=F,
                suffixes=c("_p1", "_p2"))

nw_cty <- nw_cty[, list(N=sum(as.numeric(N))), 
                by=c("residence.country.describer.first_p1", 
                     "residence.country.describer.first_p2")]
nw_cty$check_same <- nw_cty$residence.country.describer.first_p1 == nw_cty$residence.country.describer.first_p2
write.csv(nw_cty, paste0(dir_data, 'eda2_auth/2019-09-22-auth-country-network.csv'), na='', row.names=F, fileEncoding="UTF-8")


# Using NetworkD3

# network
grp <- "residence.country.describer.first"
# grp <- "describer.gender.n"
forceNetwork(Links = nw_f2, Nodes = des2,
             Source="idx_auth_p1", Target="idx_auth_p2", Value="N",
             NodeID="last.name", Group = grp,
             zoom=T, legend=T, opacityNoHover = 1, opacity=0.9,
             fontFamily="san-serif")

authors <- des[residence.country.describer.n == "JA"]$idx
nw_f2 <- nw_f[idx_auth_p1 %in% authors | idx_auth_p2 %in% authors]
authors <- unique(c(nw_f2$idx_auth_p1, nw_f2$idx_auth_p2))
des2 <- des[idx_auth %in% authors]
des2$idx <- seq(0, dim(des2)[1]-1, 1)
subset_col <- c("full.name.of.describer.n", "idx")
nw_f2 <- merge(nw_f2, des2[, ..subset_col], by.x="p1", by.y="full.name.of.describer.n", all.x=T, all.y=F)
nw_f2 <- merge(nw_f2, des2[, ..subset_col], by.x="p2", by.y="full.name.of.describer.n", all.x=T, all.y=F,
               suffixes=c("_p1", "_p2"))
forceNetwork(Links = nw_f2, Nodes = des2,
             Source="idx_p1", Target="idx_p2", Value="N",
             NodeID="last.name", Group = grp,
             zoom=T, legend=T, opacityNoHover = 1, opacity=0.9,
             fontFamily="san-serif")
nw_f[grepl("Hirashima", p1) | grepl("Hirashima", p2)]


authors <- des[residence.country.describer.n == "US"]$idx
nw_f2 <- nw_f[idx_auth_p1 %in% authors | idx_auth_p2 %in% authors]
authors <- unique(c(nw_f2$idx_auth_p1, nw_f2$idx_auth_p2))
des2 <- des[idx_auth %in% authors]
des2$idx <- seq(0, dim(des2)[1]-1, 1)
subset_col <- c("full.name.of.describer.n", "idx")
nw_f2 <- merge(nw_f2, des2[, ..subset_col], by.x="p1", by.y="full.name.of.describer.n", all.x=T, all.y=F)
nw_f2 <- merge(nw_f2, des2[, ..subset_col], by.x="p2", by.y="full.name.of.describer.n", all.x=T, all.y=F,
               suffixes=c("_p1", "_p2"))
forceNetwork(Links = nw_f2, Nodes = des2,
             Source="idx_p1", Target="idx_p2", Value="N",
             NodeID="last.name", Group = grp,
             zoom=T, legend=T, opacityNoHover = 1, opacity=0.9,
             fontFamily="san-serif")

# sankey
threshold <- 1; nw_cty2 <- nw_cty[check_same == "FALSE" & N >= threshold]
nodes <- data.frame(label=unique(c(nw_cty2$residence.country.describer.first_p1, 
                                   nw_cty2$residence.country.describer.first_p2)))
nodes <- data.table(nodes); nodes <- nodes[order(label)]
nodes$idx <- seq(0, dim(nodes)[1]-1, 1)
nw_cty2 <- merge(nw_cty2, nodes, by.x="residence.country.describer.first_p1", by.y="label", all.x=T, all.y=F)
nw_cty2 <- merge(nw_cty2, nodes, by.x="residence.country.describer.first_p2", by.y="label", all.x=T, all.y=F, suffixes=c("_cty1", "_cty2"))

sankeyNetwork(Links = nw_cty2, Nodes = nodes,
              Source="idx_cty1", Target="idx_cty2", Value="N",
              NodeID="label", fontSize = 16, unit = "Species described collaboratively")


# Using visNetwork

# http://www.sthda.com/english/articles/33-social-network-analysis/137-interactive-network-visualization-using-r/
# https://www.r-graph-gallery.com/network-interactive.html


