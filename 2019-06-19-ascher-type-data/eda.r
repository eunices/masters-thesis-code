source('2019-06-19-ascher-type-data/subset.r')

# Libraries
#############
library(ggplot2)
library(grid); library(gridExtra)
library(plyr); library(maptools)

# Exploratory analysis
#############

# # Plot world object
# pdf('plots/2019-06-19-type-data-map.pdf', width=21, height=10)
# ggplot() +
#   geom_sf(data = shp, fill = NA, ) +
#   geom_sf(data = ll, colour = "slategray3") + theme_minimal()
# dev.off()

# Heatmap of types described by?
spp <- get_df1(write=F)
spp <- spp[,c("idx", "type.country.n", "full.name.of.describer")]
spp <- data.table(spp %>% separate_rows(full.name.of.describer, sep="; "))

spp2 <- get_df2(write=F)

fn_des <- "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv"
des <- get_des(write=F)
des <- des[, c("full.name.of.describer.n", "residence.country.describer.n")]
des <- data.table(des %>% separate_rows(residence.country.describer.n, sep="; "))
des <- des[, id := seq_len(.N), by = full.name.of.describer.n][order(full.name.of.describer.n, id),][!duplicated(full.name.of.describer.n)]

spp2 <- merge(spp, des, by.x="full.name.of.describer", by.y="full.name.of.describer.n",
      all.x=T, all.y=F)

t <- table(spp2$type.country.n, spp2$residence.country.describer.n)
t <- data.table(t)
t <- t[N!=0]
names(t) <- c("type.country", "residence.country", "N")

# ggplot(t[type.country != "US" & residence.country != "US"], aes(type.country, residence.country, fill= N)) + 
#   geom_tile()

# merge t with lat and lon
dim(t); t <- t[!(is.na(residence.country) | residence.country=="" | is.na(type.country) | type.country=="")]; dim(t)
to_merge1 <- lookup.cty[, c("GEC", "centroid_lat", "centroid_lon")]
to_merge2 <- lookup.cty[, c("A.2", "GEC", "centroid_lat", "centroid_lon")]
t <- merge(t, to_merge1, by.x="type.country", by.y="GEC", all.x=T, all.y=F)
t <- merge(t, to_merge2, by.x="residence.country", by.y="A.2", all.x=T, all.y=F,
           suffixes=c("_type.country", "_residence.country"))
t$A.2 <- NULL
t$residence.country <- NULL

t[is.na(centroid_lat_type.country)]
t[is.na(centroid_lat_residence.country)]$residence.country

names(t) <- c("des", "N", "oY", "oX", "res", "dY", "dX")

t$Geom <- paste0("LINESTRING (", as.character(t$oX), 
                " ", as.character(t$oY), ", ", 
                as.character(t$dX), " ", as.character(t$dY), ")")
t$no_flow <- t$res == t$des


write.csv(t,
          paste0(dir, "eda/2019-09-22-flow-map.csv"), na='', row.names=F, fileEncoding="UTF-8")


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
