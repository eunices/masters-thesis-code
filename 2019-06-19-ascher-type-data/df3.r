source('2019-06-19-ascher-type-data/init.r')

# Libraries
#############

# NONE

# Parameters
#############

# NONE

# Scripts
#############


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean  journal names
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean  journal names"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.1-synonyms.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx1[, names(dfx1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_2-clean.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2[, names(dfx2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

cols <- c("idx", "genus", "species", "date.n", "author", "paper.type", 
          "title", "journal", "volume", "issue", 
          "page.numbers.publication", "country.of.publication", "city.of.publication")

dfx <- rbind(dfx1[, ..cols], dfx2[, ..cols])
# rm(dfx1, dfx2)

df_n <- data.table(dfx[, c("idx", "journal")] %>%
  group_by(journal) %>%
  summarise(idxes=paste0(idx,collapse='; ')))

write.csv(df_n, paste0(dir, "clean/journal_names.csv"), na='', row.names=F, fileEncoding="UTF-8")

filepath <- paste0(dir, "clean/journal_names_edit.csv")
df_n <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')[,c(  "journal_new", "idxes")]

df_n <- df_n %>% separate_rows(idxes)

dfx1 <- merge(dfx1, df_n, by.x='idx', by.y='idxes', all.x=T, all.y=F)
dfx1$journal <- dfx1$journal_new
dfx1$journal_new <- NULL

dfx2 <- merge(dfx2, df_n, by.x='idx', by.y='idxes', all.x=T, all.y=F)
dfx2$journal <- dfx2$journal_new
dfx2$journal_new <- NULL

write.csv(dfx1, 
        paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.0-clean-journals.csv"), na='', row.names=F, fileEncoding="UTF-8")

write.csv(dfx2, 
        paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.0-clean-journals.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean journal species relationships
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean journal species relationships"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.0-clean-journals.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx1[, names(dfx1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.0-clean-journals.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2[, names(dfx2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_1.0-all_edit.csv")
auth <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
auth[, names(auth) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
auth <- auth[, c("idx", "author", "full.name.of.describer")]
dim(auth[duplicated(auth)])

dim(dfx1); dim (dfx2)
dfx1 <- merge(dfx1, auth, by="idx", all.x=T, all.y=F, suffixes=c("", "_new"))
dfx2 <- merge(dfx2, auth, by="idx", all.x=T, all.y=F, suffixes=c("", "_new"))
dim(dfx1); dim (dfx2)

dfx1$author <- dfx1$author_new; dfx1$author_new <- NULL
dfx1$full.name.of.describer_new <- dfx1$full.name.of.describer; dfx1$full.name.of.describer_new <- NULL

dfx2$author <- dfx2$author_new; dfx2$author_new <- NULL
dfx2$full.name.of.describer_new <- dfx2$full.name.of.describer; dfx2$full.name.of.describer_new <- NULL

cols <- c("idx", "genus", "species", "date.n", "author", "paper.type", 
          "title", "journal", "volume", "issue", 
          "page.numbers.publication")

dfx <- rbind(dfx1[,..cols], dfx2[,..cols])

df_n <- data.table(dfx %>%
  group_by(date.n, author, title, journal,  volume,  issue, page.numbers.publication) %>%
  summarise(idxes=paste0(idx, collapse='; ')))

df_n$page.numbers.publication <- ifelse(is.na(df_n$page.numbers.publication), NA, paste0("'", df_n$page.numbers.publication))
df_n$volume <- ifelse(is.na(df_n$volume), NA, paste0("'", df_n$volume))
df_n$issue <-ifelse(is.na(df_n$issue), NA, paste0("'", df_n$issue))

write.csv(df_n[order(author, date.n, journal, title, volume, issue, page.numbers.publication)], paste0(dir, "clean/journal_species.csv"), na='', row.names=F, fileEncoding="UTF-8")

filepath <- paste0(dir, "clean/journal_species_edit.csv")
df_n <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df_n[, names(df_n) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 


# Information manually merged back into 2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_1.0-all_edit.csv
# df_n2 <- df_n %>% separate_rows(idxes)
# write.csv(df_n2, paste0(dir, "clean/journal_species_edit_denormalised.csv"), na='', row.names=F, fileEncoding="UTF-8")


df_n2 <- data.table(df_n %>% separate_rows(idxes))
df_n2 <- df_n2[, c('idxes', 'author', 'date.n', 'journal', 'title', 'volume',
                   'issue', 'page.numbers.publication')]
dim(df_n2); df_n2 <- unique(df_n2); dim(df_n2)

dfx1 <- merge(dfx1, df_n2, all.x=T, all.y=F, by.x='idx', by.y='idxes',
              suffixes=c("", "_new"))
dfx1[!is.na(author_new)]$author <- dfx1[!is.na(author_new)]$author_new; dfx1$author_new <- NULL
dfx1[!is.na(date.n_new)]$date.n <- dfx1[!is.na(date.n_new)]$date.n_new; dfx1$date.n_new <- NULL
dfx1[!is.na(journal_new)]$journal <- dfx1[!is.na(journal_new)]$journal_new; dfx1$journal_new <- NULL
dfx1[!is.na(title_new)]$title <- dfx1[!is.na(title_new)]$title_new; dfx1$title_new <- NULL
dfx1[!is.na(volume_new)]$volume <- dfx1[!is.na(volume_new)]$volume_new; dfx1$volume_new <- NULL
dfx1[!is.na(issue_new)]$issue <- dfx1[!is.na(issue_new)]$issue_new; dfx1$issue_new <- NULL
dfx1[!is.na(page.numbers.publication_new)]$page.numbers.publication <- dfx1[!is.na(page.numbers.publication_new)]$page.numbers.publication_new; dfx1$page.numbers.publication_new <- NULL
dim(dfx1)

dfx2 <- merge(dfx2, df_n2, all.x=T, all.y=F, by.x='idx', by.y='idxes',
              suffixes=c("", "_new"))
dfx2[!is.na(author_new)]$author <- dfx2[!is.na(author_new)]$author_new; dfx2$author_new <- NULL
dfx2[!is.na(date.n_new)]$date.n <- dfx2[!is.na(date.n_new)]$date.n_new; dfx2$date.n_new <- NULL
dfx2[!is.na(journal_new)]$journal <- dfx2[!is.na(journal_new)]$journal_new; dfx2$journal_new <- NULL
dfx2[!is.na(title_new)]$title <- dfx2[!is.na(title_new)]$title_new; dfx2$title_new <- NULL
dfx2[!is.na(volume_new)]$volume <- dfx2[!is.na(volume_new)]$volume_new; dfx2$volume_new <- NULL
dfx2[!is.na(issue_new)]$issue <- dfx2[!is.na(issue_new)]$issue_new; dfx2$issue_new <- NULL
dfx2[!is.na(page.numbers.publication_new)]$page.numbers.publication <- dfx2[!is.na(page.numbers.publication_new)]$page.numbers.publication_new; dfx2$page.numbers.publication_new <- NULL
dim(dfx2)

write.csv(dfx1[order(as.numeric(idx))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.1-clean-journal-species.csv"), na='', row.names=F, fileEncoding="UTF-8")

write.csv(dfx2[order(as.numeric(idx))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.1-clean-journal-species.csv"), na='', row.names=F, fileEncoding="UTF-8")


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
  summarise(authors.fn=paste0(full.name.of.describer.n,collapse='; ')))
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


write.csv(dfx[author=="Smith [F.]" & date.n =="1860"], 'test.csv')
write.csv(dfx[author=="Houston" & date.n =="1993"], 'test.csv')
write.csv(dfx[author=="Houston" & date.n =="1993"], 'test.csv')
dfx[idx==9140]
