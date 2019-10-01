source('2019-06-19-ascher-type-data/subset.r')


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - resource flow
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- resource flow"))

# Summarising where there is flow
spp <- get_df1(write=F)
spp_s <- spp[,c("idx", "type.country.n", "full.name.of.describer")]
spp_s <- data.table(spp_s %>% separate_rows(full.name.of.describer, sep="; "))
des <- get_des(write=F)
des <- des[, c("full.name.of.describer.n", "residence.country.describer.n")]
des <- data.table(des %>% separate_rows(residence.country.describer.n, sep="; "))
des <- des[, id := seq_len(.N), by = full.name.of.describer.n][order(full.name.of.describer.n, id),][!duplicated(full.name.of.describer.n)]
spp_s <- merge(spp_s, des, by.x="full.name.of.describer", by.y="full.name.of.describer.n",
      all.x=T, all.y=F)
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
t$no_flow <- t$res == t$des
write.csv(t,
          paste0(dir_data, "eda/2019-09-22-flow-map-type-loc-des-country.csv"), na='', row.names=F, fileEncoding="UTF-8")

# Summarising where there is no flow
table(t$no_flow)
s1 <- t[, list(N=sum(N)), by='des']
s2 <- t[no_flow==TRUE, list(N=sum(N)), by='des']
ss <- merge(s1, s2, by='des', all.x=T, all.y=F, suffixes=c("_total", "_cty"))
ss$prop <- ss$N_cty/ss$N_total
ss <- merge(ss, lookup.cty[, c("GEC", "Country", "A.3")], 
      by.x="des", by.y="GEC", 
      all.x=T, all.y=F)

write.csv(ss[order(-prop)],
          paste0(dir_data, "eda/2019-09-22-summary-country-prop.csv"), na='', row.names=F, fileEncoding="UTF-8")

# Rather cool flow map in R https://kateto.net/network-visualization
