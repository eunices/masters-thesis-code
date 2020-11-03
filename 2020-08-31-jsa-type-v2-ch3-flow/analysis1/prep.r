
# Information about code:
# This code corresponds to exploratory data analyses for my MSc thesis.
# They are pertaining to EDA for the authors 
# (relevant for Chapter 3, the section on Determinants of taxonomic resources flow).
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# Set up
source('2020-08-31-jsa-type/init/init.R')
source('2020-08-31-jsa-type/subset.r')

# Libraries
library(networkD3)
library(ggplot2)

# Read lookup
lu <- get_lp_statoid()

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - resource flow
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- resource flow"))

# Summarising where there is flow
spp <- get_df1(write=F)
spp1 <- spp[,c("idx", "type.country.n", "full.name.of.describer")]
spp2 <- get_df2(write=F)[status=="Synonym"][,c("idx", "type.country.n", "full.name.of.describer")]

spp_s <- rbind(spp1, spp2)
spp_s <- data.table(spp_s %>% separate_rows(full.name.of.describer, sep="; "))
des <- get_des(write=F)
des <- des[, c("full.name.of.describer.n", "residence.country.describer.n")]
des <- data.table(des %>% separate_rows(residence.country.describer.n, sep="; "))
des <- des[, id := seq_len(.N), by = full.name.of.describer.n][
        order(full.name.of.describer.n, id),][!duplicated(full.name.of.describer.n)]
spp_s <- merge(spp_s, des, by.x="full.name.of.describer", by.y="full.name.of.describer.n",
               all.x=T, all.y=F)
spp_s <- spp_s[, c("type.country.n", "residence.country.describer.n", "idx")]
spp_s <- unique(spp_s)

t <- table(spp_s$type.country.n, spp_s$residence.country.describer.n)
t <- data.table(t)
dim(t); t <- t[N!=0]; dim(t)
names(t) <- c("type.country", "residence.country", "N")
dim(t); t <- t[!(type.country == "" | residence.country=="[unknown]")]; dim(t)
to_merge1 <- lookup.cty[, c("DL", "centroid_lat", "centroid_lon")]
to_merge2 <- lookup.cty[, c("DL", "centroid_lat", "centroid_lon")]
t <- merge(t, to_merge1, by.x="type.country", by.y="DL", all.x=T, all.y=F)
t <- merge(t, to_merge2, by.x="residence.country", by.y="DL", all.x=T, all.y=F,
           suffixes=c("_type.country", "_residence.country"))
t[is.na(centroid_lat_type.country)]
t[is.na(centroid_lat_residence.country)]$residence.country
names(t) <- c("ori", "des", "N", "dY", "dX", "oY", "oX")
t$Geom <- paste0("LINESTRING (", as.character(t$oX), 
                " ", as.character(t$oY), ", ", 
                as.character(t$dX), " ", as.character(t$dY), ")")
t$no_flow <- t$ori == t$des

write.csv(t,
          paste0(dir_data_ch3_flow, "2019-09-22-flow-map-type-loc-des-country.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")

write.csv(t,
          paste0(dir_shiny, "eda1.1_shiny/data/2019-09-22-flow-map-type-loc-des-country.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")
