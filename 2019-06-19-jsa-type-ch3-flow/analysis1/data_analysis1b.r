# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Determinants of taxonomic resources flow: data preparation
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2019-06-19-jsa-type/init/var.R')
source('2019-06-19-jsa-type/subset.R')

# Libraries
library(data.table)
library(tidyr)

# Data wrangling
flow <- fread(paste0(dir_data, "eda1_flow/2019-09-22-flow-map-type-loc-des-country.csv"), 
              encoding="UTF-8")
flow[no_flow==FALSE, list(N_cty=length(unique(des))), by=c("ori")][order(-N_cty)]

spp1 <- get_df1(write=F)[, c("type.country.n", "full.name.of.describer")]
spp2 <- get_df2(write=F)[status=="Synonym"][, c("type.country.n", "full.name.of.describer")]
spp <- rbind(spp1, spp2)
spp <- spp %>% separate_rows(full.name.of.describer, sep="; ")
sum_flow <- spp[, .N, by="type.country.n"][order(-N)]
flow <- flow[no_flow == "FALSE" ,c("ori", "des", "N")]

# Get statoid country codes (with socioeconomic status)
lu <- get_lp_statoid()
comb <- expand.grid(lu$DL, lu$DL)
names(comb) <- c("ori", "des")
flow <- data.table(merge(comb, flow, by=c("ori", "des"), all.x=T, all.y=F))
flow[is.na(flow)] <- 0

# Get colonial history data
lu_col <- get_lp_col()
lu_col <- lu_col[, c("_A3", "_ColRulerA3", "_IndYear")]
names(lu_col) <- gsub("_", "", names(lu_col))
lu_col <- lu_col %>% separate_rows(A3, sep="; ")
lu_col <- lu_col %>% separate_rows(ColRulerA3, sep="; ")
lu_col <- merge(lu_col, lu[, c("DL", "A-3")], by.x="A3", by.y="A-3", all.x=T, all.y=F)
lu_col <- merge(lu_col, lu[, c("DL", "A-3")], by.x="ColRulerA3", by.y="A-3", all.x=T, all.y=F,
                suffixes=c("_Col", "_ColRuler"))
lu_col <- lu_col[, c("DL_Col", "DL_ColRuler", "IndYear")]
lu_col$col_check <- 1
lu_col <- unique(lu_col)

# Get country adjacent data
# country_iso3 neighbor_iso3
lu_adj <- get_lp_adj_countries()
lu_adj <- lu_adj[!(DL1 %in% c("No man's land", "Disputed land")), ]
lu_adj <- lu_adj[!(DL2 %in% c("No man's land", "Disputed land")), ]
lu_adj$adj_check <- 1

# Modelling variables
model_vars <- c("continent_check", "class_check", "col_check", "adj_check", 
                "continent_ori", "Class_ori", "Class_des")

# Add SES & continent variables
cols <- c("DL", "Class", "continent")
flow <- merge(flow, lu[, ..cols], by.x="ori", by.y="DL", all.x=T, all.y=F)
flow <- merge(flow, lu[, ..cols], by.x="des", by.y="DL", all.x=T, all.y=F, 
              suffixes=c("_ori", "_des"))
lvls <- c("Low income", "Lower middle income", "Upper middle income", "High income",  "Unclassed")
flow$Class_ori <- factor(flow$Class_ori, levels=lvls)
flow$Class_des <- factor(flow$Class_des, levels=lvls)
flow$class_check <-  ifelse(flow$Class_ori == "Unclassed" | flow$Class_des == "Unclassed", "Unclassed", 
    ifelse(as.numeric(flow$Class_ori) > as.numeric(flow$Class_des), "High-to-low",  
        ifelse(as.numeric(flow$Class_ori) == as.numeric(flow$Class_des), "Equal", "Low-to-high")))
flow$continent_check <- ifelse(flow$continent_ori == flow$continent_des, 
                               "Same continent", "Different continent")

# Add colonialism variable
table(lu_col$DL_ColRuler) # 177
flow <- merge(flow, lu_col, by.x=c("des", "ori"), by.y=c("DL_Col", "DL_ColRuler"), all.x=T, all.y=F)
flow[is.na(col_check)]$col_check <- 0
flow$col_check <- ifelse(flow$col_check == 1, "Colonised", 
    ifelse(flow$col_check == 0, "Not colonised", ""))

# Add neighbouring boundary variable
flow <- merge(flow, lu_adj, by.x=c("des", "ori"), by.y=c("DL1", "DL2"), all.x=T, all.y=T)
flow[is.na(adj_check)]$adj_check <- 0
flow$adj_check <- ifelse(flow$adj_check == 1, "Adjacent", 
    ifelse(flow$adj_check == 0, "Not adjacent", ""))

# Final wrangling
flow <- merge(flow, sum_flow, by.x="des", by.y="type.country.n", 
              suffixes=c("_flow", "_total"), 
              all.x=T, all.y=T)
flow[is.na(N_total)]$N_total <- 0; dim(is.na(flow$ori))
flow <- flow[!is.na(ori)]

# Persist dataset
vars <- c("ori", "des", "N_flow", "N_total", model_vars)
write.csv(flow[, ..vars], paste0(dir_data, "eda1_flow/2019-11-01-flow-GLM.csv"), 
          fileEncoding="UTF-8", row.names=F)
