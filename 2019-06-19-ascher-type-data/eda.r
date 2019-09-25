source('2019-06-19-ascher-type-data/subset.r')

# Libraries
#############
library(ggplot2)
library(grid); library(gridExtra)
library(plyr); library(maptools)
library(reshape)

# Exploratory analysis
#############

# # Plot world object
# pdf('plots/2019-06-19-type-data-map.pdf', width=21, height=10)
# ggplot() +
#   geom_sf(data = shp, fill = NA, ) +
#   geom_sf(data = ll, colour = "slategray3") + theme_minimal()
# dev.off()

# Flag
spp <- get_df1(write=F)
spp_s <- spp[,c("idx", "type.country.n", "full.name.of.describer")]
spp_s <- data.table(spp_s %>% separate_rows(full.name.of.describer, sep="; "))

spp2 <- get_df2(write=F)

flag <- rbind(
    data.frame(table(spp$source.of.latlon.n)),
    data.frame(table(spp2$source.of.latlon.n))
)


write.csv(flag,
          paste0(dir, "eda/2019-09-22-type-data-quality2.csv"), na='', row.names=F, fileEncoding="UTF-8")


# Diff
spp$diff <- as.numeric(spp$date.n) - as.numeric(spp$date.of.type.yyyy)
write.csv(spp[diff<0], 'tmp/test.csv', row.names=F)

# Heatmap of types described by?
# TODO: fix russia!!!

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

# ggplot(t[type.country != "US" & residence.country != "US"], aes(type.country, residence.country, fill= N)) + 
#   geom_tile()

# merge t with lat and lon
to_merge1 <- lookup.cty[, c("GEC", "centroid_lat", "centroid_lon")]
to_merge2 <- lookup.cty[, c("A.2", "GEC", "centroid_lat", "centroid_lon")]
t <- merge(t, to_merge1, by.x="type.country", by.y="GEC", all.x=T, all.y=F)
t <- merge(t, to_merge2, by.x="residence.country", by.y="A.2", all.x=T, all.y=F,
           suffixes=c("_type.country", "_residence.country"))
t$residence.country <- NULL

t[is.na(centroid_lat_type.country)]
t[is.na(centroid_lat_residence.country)]$residence.country

names(t) <- c("des", "N", "oY", "oX", "res", "dY", "dX")

t$Geom <- paste0("LINESTRING (", as.character(t$oX), 
                " ", as.character(t$oY), ", ", 
                as.character(t$dX), " ", as.character(t$dY), ")")
t$no_flow <- t$res == t$des


write.csv(t,
          paste0(dir, "eda/2019-09-22-flow-map-type-loc-des-country.csv"), na='', row.names=F, fileEncoding="UTF-8")


table(t$no_flow)
s1 <- t[, list(N=sum(N)), by='des']
s2 <- t[no_flow==TRUE, list(N=sum(N)), by='des']
ss <- merge(s1, s2, by='des', all.x=T, all.y=F, suffixes=c("_total", "_cty"))
ss$prop <- ss$N_cty/ss$N_total
ss <- merge(ss, lookup.cty[, c("GEC", "Country", "A.3")], 
      by.x="des", by.y="GEC", 
      all.x=T, all.y=F)


write.csv(ss[order(-prop)],
          paste0(dir, "eda/2019-09-22-summary-country-prop.csv"), na='', row.names=F, fileEncoding="UTF-8")


map <- geom_sf(data = shp, colour = "white", fill = NA, size=0.02) 

flow <- ggplot() +
    geom_segment(data = t, aes(x=oX, y=oY,xend=dX, yend=dY, alpha=N), col="white") +
        scale_alpha_continuous(range = c(0.03, 0.3)) +
            theme(panel.background = element_rect(fill='black',colour='black'))+
                coord_equal() + 
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


pdf("plots/2019-09-22-flow-map.pdf", height = 8.27, width = 11.69)
flow+map
dev.off()


# Network of authors
nw <- fread(
    paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_7.0-author-networks.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
nw[, names(nw) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

nw
nw_cty <- nw[, list(sum(as.numeric(N))), by=c("residence.country.describer.first_p1", "residence.country.describer.first_p2")]

write.csv(nw_cty, paste0(dir, 'eda/2019-09-22-auth-country-network.csv'), na='', row.names=F, fileEncoding="UTF-8")

# Russian taxonomist

tc <- get_df1(write=F)[,c("idx", "full.name.of.describer", "type.country.n")]
ru <- get_des(write=F)[grepl("RU", residence.country.describer.n)]

tc2 <- data.table(tc %>% separate_rows(full.name.of.describer, sep="; "))
tc2 <- tc2[full.name.of.describer %in% ru$full.name.of.describer.n]
tc2 <- tc2[, list(N_spp=.N), by=c("full.name.of.describer", "type.country.n")]

tc3 <- dcast(tc2, full.name.of.describer ~ type.country.n, value.var="N_spp", fun.aggregate=sum)
fi <- merge(ru, tc3, by.y="full.name.of.describer", by.x="full.name.of.describer.n", all.x=F, all.y=T)

write.csv(fi,
          paste0(dir, "eda/2019-09-25-russians.csv"), na='', row.names=F, fileEncoding="UTF-8")





# type country n = GEC
# publication country = A2
# type.repository.country = A2
