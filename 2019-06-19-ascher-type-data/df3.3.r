# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - further denormalization of data with cleaned describer data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': further denormalization of data with cleaned describer data"))

filepath <- paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_3.0-by-author.csv")
describers <- fread(filepath, na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describers[, names(describers) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# Denormalization
describers <- describers %>% separate_rows(idxes, idxes_author.order)

# Joining with dates
filepath <- paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-coll.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
filepath <- paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.3-clean-coll.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx <- rbind(dfx1[,c("idx", "date.n")], dfx2[,c("idx", "date.n")])
dfx <- dfx[!duplicated(idx)]
dfx$idx <- as.numeric(dfx$idx)
describers$idxes <- as.numeric(describers$idxes)
describers <- merge(describers, dfx, by.x="idxes", by.y="idx", all.x=T, all.y=F)

write.csv(describers, paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv"), na='', row.names=F, fileEncoding="UTF-8")
