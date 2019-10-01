source('2019-06-19-ascher-type-data/subset.r')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - coauthor network
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- coauthor network"))

# Country level summary
nw <- fread(
    paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_7.0-author-networks.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
nw[, names(nw) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

des <- get_des(write=F)

nw <- merge(nw, des, by.x="p1", by.y="full.name.of.describer.n", all.x=T, all.y=F)
nw <- merge(nw, des, by.x="p2", by.y="full.name.of.describer.n", all.x=T, all.y=F,
            suffixes=c("_p1", "_p2"))

split_cty <- function(x) {
    strsplit(x, split="; ")[[1]][1]
}

nw$residence.country.describer.first_p1 <- sapply(nw$residence.country.describer.n_p1, split_cty)
nw$residence.country.describer.first_p2 <- sapply(nw$residence.country.describer.n_p2, split_cty)

nw_cty <- nw[, list(sum(as.numeric(N))), 
                by=c("residence.country.describer.first_p1", 
                     "residence.country.describer.first_p2")]

write.csv(nw_cty, paste0(dir_data, 'eda2_auth/2019-09-22-auth-country-network.csv'), na='', row.names=F, fileEncoding="UTF-8")

