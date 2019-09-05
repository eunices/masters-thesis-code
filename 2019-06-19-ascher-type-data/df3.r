source('2019-06-19-ascher-type-data/init.r')

# Libraries
#############
library(dplyr)
library(tidyr)
library(data.table)

# Parameters
#############

# NONE

# Scripts
#############

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - count number of species in publication
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- count number of species in publication"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv")
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
df$idxes <- as.numeric(df$idxes)
df$idxes_author.order <- factor(df$didxes_author.order, 
                                levels=c("1", "S", "2", "3", "L"), ordered=T)

df <- df[order(idxes, idxes_author.order)]
df2 <- data.table(df[,c("idxes", "full.name.of.describer.n")] %>%
  group_by(idxes) %>%
  summarise(authors.fn=paste0(full.name.of.describer.n,collapse='; ')))\
df2$idxes <- as.character(df2$idxes)
rm(df)

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.0-clean.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx1[, names(dfx1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_2-clean.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2[, names(dfx2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

cols <- c("idx", "genus", "species", "date.n", "author", "paper.type", 
          "title", "journal", "volume", "issue", 
          "page.numbers.publication", "country.of.publication", "city.of.publication")

dfx <- rbind(dfx1[, ..cols], dfx2[, ..cols])
rm(dfx1, dfx2)

dfx <- merge(dfx, df2, by.x="idx", by.y="idxes", all.x=T, all.y=F)

test <- dfx[, .N, by=c("authors.fn", "author", "date.n", "journal", "volume")][order(authors.fn,  date.n, journal, volume)]
write.csv(test, 'test.csv')

# TODO: cross checking of authors: find mismatch between authors
# create last name reference for finalised author list in 2.4
# combine authors in 3
# use 2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final_edit.csv

# TODO: N species / publications
# use abovementioned code

# TODO: count number of publications per author
# add on above code
