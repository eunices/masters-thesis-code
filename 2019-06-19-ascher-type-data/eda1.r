
source('2019-06-19-ascher-type-data/init.r')
source('2019-06-19-ascher-type-data/subset.r')

library(networkD3)
library(ggplot2)

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
          paste0(dir_data, "eda1_flow/2019-09-22-flow-map-type-loc-des-country.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")
write.csv(t,
          paste0(dir_script, "eda1.1_shiny/data/2019-09-22-flow-map-type-loc-des-country.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")

# 1) Summarising where there is no flow

# Data processing
table(t$no_flow)
s1 <- t[, list(N=sum(N)), by='des']
s2 <- t[no_flow=="TRUE", list(N=sum(N)), by='des']
ss <- merge(s1, s2, by='des', all.x=T, all.y=F, suffixes=c("_total", "_cty"))
ss$prop <- ss$N_cty/ss$N_total
ss <- merge(ss, lookup.cty[, c("GEC", "Country", "A.3", "Class")], 
      by.x="des", by.y="GEC", all.x=T, all.y=F)
ss[is.na(N_cty)]$N_cty <- 0
ss[is.na(prop)]$prop <- 0
ss$Class <- factor(ss$Class, levels=c("High income", 
                                      "Upper middle income",
                                      "Lower middle income",
                                      "Low income", 
                                      "Unclassed"))
ss[Class=="Unclassed"] # visual check

# Plot
ggplot(ss[!is.na(Class)], aes(x=Class, y=round(prop*100,2))) + 
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=1, size=1) +
    labs(x="\nWorld Bank classification", y = "Proportion of species described\n by describers residing in country (%)\n") +
         theme_classic()

# In text figures
ss[N_cty>=1]
summary(ss[prop>0 & N_total>=5]$prop*100)
length(ss[prop>0 & N_total>=5]$prop*100)
shapiro.test(ss[prop>0 & N_total>=5]$prop*100) # not normal
ss[N_total>5][order(-prop)][1:10]
ss[Country=="Brazil"]

# Statistical tests
# http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
kruskal.test(prop~Class, data = ss)
pairwise.wilcox.test(ss$prop, ss$Class, p.adjust.method = "BH")
ss_summary <- ss[, list(mean=mean(prop),
                        median=median(prop),
                        quantile_1st = quantile(prop, 0.25),
                        quantile_3rd = quantile(prop, 0.75),
                        N=.N),
                  by=c("Class")]

write.csv(ss_summary,
          paste0(dir_data, "eda1_flow/2019-09-22-summary-country-prop-summary.csv"), na='', row.names=F, fileEncoding="UTF-8")

write.csv(ss[order(-prop)],
          paste0(dir_data, "eda1_flow/2019-09-22-summary-country-prop.csv"), na='', row.names=F, fileEncoding="UTF-8")

# 2) Count number of countries where there is flow

# Data processing
flow <- unique(spp_s[, c("type.country.n", "residence.country.describer.n")])
flow <- flow[type.country.n != residence.country.describer.n]
flow <- flow[,.N, by=c("residence.country.describer.n")]

des_countries <- unique(des$residence.country.describer.n); length(des_countries)
des_countries <- des_countries[!des_countries %in% flow$residence.country.describer.n]
des_countries <- data.frame(residence.country.describer.n=des_countries, N=0)
flow <- rbind(flow, des_countries)         

flow <- merge(flow, 
              lookup.cty[, c("DL", "Class")], 
              by.x="residence.country.describer.n", by.y="DL", all.x=T, all.y=F)
flow <- flow[!is.na(residence.country.describer.n) & residence.country.describer.n != "[unknown]"]
# flow$Class <- ifelse(flow$Class == "High income", "High income", "Not high income")

flow$Class <- factor(flow$Class, levels=c("High income", 
                                          "Upper middle income",
                                          "Lower middle income",
                                          "Low income", 
                                          "Unclassed"))

kruskal.test(N~Class, data = flow)
pairwise.wilcox.test(flow$N, flow$Class, p.adjust.method = "BH")

ggplot(flow, aes(x=Class, y=N)) + 
  geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=1, size=1) +
    labs(x="\nWorld Bank classification", y = "Number of countries\n to which there is taxonomic flow") +
         theme_classic()
flow$N <- as.numeric(flow$N)
flow_summary <- flow[, list(mean=mean(N),
                        median=median(N),
                        quantile_1st = quantile(N, 0.25),
                        quantile_3rd = quantile(N, 0.75),
                        N=.N),
                  by=c("Class")]
flow[, list(mean=mean(N),
            median=median(N),
            quantile_1st = quantile(N, 0.25),
            quantile_3rd = quantile(N, 0.75),
            N=.N)]
write.csv(flow_summary[order(-N)],
          paste0(dir_data, "eda1_flow/2019-09-22-summary-country-N-summary.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Location analysis suggested by Ascher
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Location analysis"))

# Where are the type repositories?
loc_type_repo_N <- spp[, list(N_type_repo_N=.N), 
    by=c("country.of.type.repository.n_long")][order(-N_type_repo_N)]
loc_type_repo_N[1:20]

loc_type_repo_unique <- spp[, list(N_type_repo_unique=length(unique(type.repository.n_short))), 
                           by=c("country.of.type.repository.n_long")][order(-N_type_repo_unique)]
loc_type_repo_unique[1:20]

loc_type_repo <- merge(loc_type_repo_N, loc_type_repo_unique, 
                       by="country.of.type.repository.n_long", all.x=T, all.y=T)
table(spp$country.of.type.repository.n_long=="[unknown]")

# Where are the publishers?

# by publication
df_publications <- get_pub(write=F)
loc_pub_N <- merge(df_publications[, list(.N), by=c("country.of.publication")],
                       lookup.cty[, c("Country", "DL")], 
                       by.x="country.of.publication", by.y="DL", all.x=T, all.y=F)
loc_pub_N <- loc_pub_N[order(-N)]
loc_pub_N[order(-N)][1:10]
table(is.na(df_publications$country.of.publication))

# by journals
loc_pub_unique <- merge(df_publications[, list(N=length(unique(journal))), 
                                        by=c("country.of.publication")],
                       lookup.cty[, c("Country", "DL")], 
                       by.x="country.of.publication", by.y="DL", all.x=T, all.y=F)
loc_pub_unique <- loc_pub_unique[order(-N)]
loc_pub_unique[order(-N)][1:10]

# Quick checks
# unique(df_publications[country.of.publication=="JA"]$journal)
# unique(df_publications[country.of.publication=="NZ"]$journal)
# table(df_publications[country.of.publication=="CA"]$journal)
# unique(df_publications[country.of.publication=="RU"]$city.of.publication)
# table(df_publications[country.of.publication=="YA"]$city.of.publication)

# Where are the type localities?
loc_type_loc <- spp[type.country.n.full != "", list(.N), by=c("type.country.n.full")][order(-N)]
loc_type_loc[1:20]
table(df$type.country.n.full=="")


# Where are the describers?

# by N species described
df_describers <- get_des(write=F)
loc_des_N <- df_describers[!is.na(residence.country.describer.first), 
                           list(N=sum(ns_spp_N)), 
                           by=c("residence.country.describer.first")][order(-N)]

# by describer
loc_des_unique <- df_describers[!is.na(residence.country.describer.n), list(.N), 
                              by=c("residence.country.describer.first")][order(-N)]
loc_des_unique[1:20]
table(is.na(df_describers$residence.country.describer.first))


merge1 <- merge(loc_type_repo, loc_pub_N[, c("Country", "N")], 
                by.x="country.of.type.repository.n_long", by.y="Country", all.x=T, all.y=T)
names(merge1)[which(names(merge1) == "N")] <- "N_loc-pub-N"
merge2 <- merge(merge1, loc_pub_unique[, c("Country", "N")], 
                by.x="country.of.type.repository.n_long", by.y="Country", all.x=T, all.y=T)
names(merge2)[which(names(merge2) == "N")] <- "N_loc-pub-unique"
merge3 <- merge(merge2, loc_type_loc, 
                by.x="country.of.type.repository.n_long", by.y="type.country.n.full", 
                all.x=T, all.y=T)
names(merge3)[which(names(merge3) == "N")] <- "N_type-loc"
merge4 <- merge(merge3, loc_des_N, 
                by.x="country.of.type.repository.n_long", 
                by.y="residence.country.describer.first", 
                all.x=T, all.y=T)
names(merge4)[which(names(merge4) == "N")] <- "N_des_N"
merge5 <- merge(merge4, loc_des_unique, 
                by.x="country.of.type.repository.n_long", 
                by.y="residence.country.describer.first", 
                all.x=T, all.y=T)
names(merge5)[which(names(merge5) == "N")] <- "N_des-N"
merge5[is.na(merge5)] <- 0

write.csv(merge5[country.of.type.repository.n_long!=0][order(country.of.type.repository.n_long)], 
          paste0(dir_data, 'eda1_flow/2019-10-03-loc.csv'), row.names=F)
