# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - creating dataset for network
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': creating dataset for network"))

ps <- fread(
    paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_1.0-all.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
ps[, names(ps) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

dfx1 <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-coll.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2 <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.3-clean-coll.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx <- rbind(dfx1[,c("idx", "date.n")], dfx2[,c("idx", "date.n")])
dfx <- dfx[date.n <=2018]$idx # limit to 2018
rm(dfx1, dfx2)

ps <- ps[idx %in% dfx]

des <- fread(
    paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
des[, names(des) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# Create pairs
ps2 <- ps[grepl(";", full.name.of.describer),]
ps4 <- ps[!grepl(";", full.name.of.describer),]
ps2 <- strsplit(ps2$full.name.of.describer, split = "; ")
# ps2 <- lapply(ps2, length)
ps2 <- lapply(ps2, function(x) as.data.frame(t(combn(x, m=2))))
ps2 <- rbindlist(ps2); names(ps2) <- c('p1', 'p2')
# https://stackoverflow.com/questions/30702191/

# Count pairs
ps2 <- ps2[, .N, by=c("p1", "p2")][order(N)]
ps2$pairs <- apply(ps2[, c("p1", "p2")], 1, function(x) paste0(sort(x), collapse = "; "))
ps2 <- ps2[, list(N=sum(N)),by=pairs]
ps2[, c("p1", "p2") := tstrsplit(pairs, "; ", fixed = TRUE)] # AMAZING
ps2$pairs <- NULL

ps4 <- ps4[, c("full.name.of.describer")]; names(ps4) <- "p1"
ps4 <- ps4[, .N, by=c("p1")]; ps4$p2 <- NA
ps2 <- rbind(ps2, ps4)

write.csv(ps2[order(-N)], paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_7.0-author-networks.csv"), na='', row.names=F, fileEncoding="UTF-8")
