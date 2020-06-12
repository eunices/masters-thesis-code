# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating the coauthor network data.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

source('2019-06-19-jsa-type/df/functions.R')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - creating dataset for network
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': creating dataset for network"))




# Read data
ps <- read_escaped_data(paste0(dir_data_raw, basefile, " describers_1.0-all.csv"))
dfx1 <- read_escaped_data(paste0(dir_data_raw, basefile, " filtered_4.3-clean-coll.csv"))
dfx2 <- read_escaped_data(paste0(dir_data_raw, basefile, " oth_4.3-clean-coll.csv"))



# Filter only for valid species and synonyms
dfx2 <- dfx2[status == "Synonym"]
dfx <- rbind(dfx1[,c("idx", "date.n")], dfx2[,c("idx", "date.n")])
dfx <- dfx[date.n <=2018]$idx # limit to 2018
rm(dfx1, dfx2)
ps <- ps[idx %in% dfx]



# Subset datasets
ps4 <- ps[!grepl(";", full.name.of.describer),]  # only 1 describer
ps2 <- ps[grepl(";", full.name.of.describer),]   # multiple describers



# Create pairs
ps2 <- strsplit(ps2$full.name.of.describer, split = "; ")
# ps2 <- lapply(ps2, length) # !CHECK
# source: # https://stackoverflow.com/questions/30702191/
ps2 <- lapply(ps2, function(x) as.data.frame(t(combn(x, m=2))))
ps2 <- rbindlist(ps2); names(ps2) <- c('p1', 'p2')



# Summarise by pairs
ps2 <- ps2[, .N, by=c("p1", "p2")][order(N)]

# Ensure each pair is unique
ps2$pairs <- apply(ps2[, c("p1", "p2")], 1, function(x) paste0(sort(x), collapse = "; "))
ps2 <- ps2[, list(N=sum(N)),by=pairs]
ps2[, c("p1", "p2") := tstrsplit(pairs, "; ", fixed = TRUE)] # AMAZING
ps2$pairs <- NULL



# Append those that did not coauthor 
ps4 <- ps4[, c("full.name.of.describer")]; names(ps4) <- "p1"
ps4 <- ps4[, .N, by=c("p1")]; ps4$p2 <- NA
ps2 <- rbind(ps2, ps4)




# Write data
write.csv(ps2[order(-N)], paste0(dir_data_raw, basefile, " describers_7.0-author-networks.csv"),
          na='', row.names=F, fileEncoding="UTF-8")
