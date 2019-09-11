# TODO: create dataframes that are persistent

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
          "page.numbers.publication")

dfx <- rbind(dfx1[,..cols], dfx2[,..cols])

df_n <- data.table(dfx %>%
  group_by(date.n, author, title, journal,  volume,  issue, page.numbers.publication) %>%
  summarise(idxes=paste0(idx, collapse='; ')))

write.csv(df_n, paste0(dir, "clean/journal_details.csv"), na='', row.names=F, fileEncoding="UTF-8")

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
# Section - clean journal fields
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean other publication fields"))
# "publication.type", "country.of.publication", "city.of.publication", "paper.authors"

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.0-clean-journals.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx1[, names(dfx1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.0-clean-journals.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2[, names(dfx2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

# df_n <- data.table(dfx %>%
#   group_by(date.n, author, title, journal, volume, issue, page.numbers.publication) %>%
#   summarise(idxes=paste0(idx,collapse='; '), 
#             paper.type=paste0(unique(paper.type), collapse=" | "),
#             country.of.publication=paste0(unique(country.of.publication), collapse=" | "),
#             city.of.publication=paste0(unique(city.of.publication), collapse=" | "),
#             paper.authors=paste0(unique(paper.authors), collapse= " | ")
#             ))

# df_n$page.numbers.publication <- ifelse(is.na(df_n$page.numbers.publication), NA, paste0("'", df_n$page.numbers.publication))
# df_n$volume <- ifelse(is.na(df_n$volume), NA, paste0("'", df_n$volume))
# df_n$issue <-ifelse(is.na(df_n$issue), NA, paste0("'", df_n$issue))

# write.csv(df_n, paste0(dir, "clean/journal_details.csv"), na='', row.names=F, fileEncoding="UTF-8")

# Iterative cleaning process
# filepath <- paste0(dir, "clean/journal_details3_edit.csv")
# auth <- fread(filepath, integer64='character', na.strings=c(''), encoding='UTF-8')
# auth[, names(auth) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

# auth2 <- data.table(auth %>% separate_rows(idxes))

# auth <- data.table(auth2 %>%
#   group_by(date.n, author, title, journal, volume, issue, page.numbers.publication) %>%
#   summarise(idxes=paste0(idxes,collapse='; '), 
#             paper.type=paste0(unique(paper.type), collapse=" | "),
#             country.of.publication=paste0(unique(country.of.publication), collapse=" | "),
#             city.of.publication=paste0(unique(city.of.publication), collapse=" | "),
#             paper.authors=paste0(unique(paper.authors), collapse= " | ")
#             ))

# write.csv(auth, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 pub.csv"), na='', row.names=F, fileEncoding="UTF-8")
# = old name journal_details*.csv

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 pub.csv")
auth <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
auth[, names(auth) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

auth2 <- data.table(auth %>% separate_rows(idxes, sep="; "))
dim(auth2); auth2 <- unique(auth2); dim(auth2)

auth2$volume <- gsub("^'*", "", (gsub("^'*", "", auth2$volume)))
auth2$issue <- gsub("^'*", "", (gsub("^'*", "", auth2$issue)))
auth2$page.numbers.publication <- gsub("^'*", "",(gsub("^'*", "", auth2$page.numbers.publication)))

dfx1 <- merge(dfx1, auth2, all.x=T, all.y=F, by.x='idx', by.y='idxes',
              suffixes=c("", "_new"))

table(is.na(dfx1$author_new))
table(is.na(dfx1$date.n_new))
table(is.na(dfx1$journal_new))
table(is.na(dfx1$title_new))
table(is.na(dfx1$volume_new))
table(is.na(dfx1$issue_new))
table(is.na(dfx1$page.numbers.publication_new))
table(is.na(dfx1$paper.type_new))
table(is.na(dfx1$country.of.publication_new))
table(is.na(dfx1$paper.authors_new))

dfx1[!is.na(author_new)]$author <- dfx1[!is.na(author_new)]$author_new; dfx1$author_new <- NULL
dfx1[!is.na(date.n_new)]$date.n <- dfx1[!is.na(date.n_new)]$date.n_new; dfx1$date.n_new <- NULL
dfx1[!is.na(journal_new)]$journal <- dfx1[!is.na(journal_new)]$journal_new; dfx1$journal_new <- NULL
dfx1[!is.na(title_new)]$title <- dfx1[!is.na(title_new)]$title_new; dfx1$title_new <- NULL
dfx1[!is.na(volume_new)]$volume <- dfx1[!is.na(volume_new)]$volume_new; dfx1$volume_new <- NULL
dfx1[!is.na(issue_new)]$issue <- dfx1[!is.na(issue_new)]$issue_new; dfx1$issue_new <- NULL
dfx1[!is.na(page.numbers.publication_new)]$page.numbers.publication <- dfx1[!is.na(page.numbers.publication_new)]$page.numbers.publication_new; dfx1$page.numbers.publication_new <- NULL
dfx1[!is.na(paper.type_new)]$paper.type <- dfx1[!is.na(paper.type_new)]$paper.type_new; dfx1$paper.type_new <- NULL
dfx1[!is.na(country.of.publication_new)]$country.of.publication <- dfx1[!is.na(country.of.publication_new)]$country.of.publication_new; dfx1$country.of.publication_new <- NULL
dfx1[!is.na(city.of.publication_new)]$city.of.publication <- dfx1[!is.na(city.of.publication_new)]$city.of.publication_new; dfx1$city.of.publication_new <- NULL
dfx1[!is.na(paper.authors_new)]$paper.authors <- dfx1[!is.na(paper.authors_new)]$paper.authors_new; dfx1$paper.authors_new <- NULL
dim(dfx1)

dfx2 <- merge(dfx2, auth2, all.x=T, all.y=F, by.x='idx', by.y='idxes',
              suffixes=c("", "_new"))

table(is.na(dfx2$author_new))
table(is.na(dfx2$date.n_new))
table(is.na(dfx2$journal_new))
table(is.na(dfx2$title_new))
table(is.na(dfx2$volume_new))
table(is.na(dfx2$issue_new))
table(is.na(dfx2$page.numbers.publication_new))
table(is.na(dfx2$paper.type_new))
table(is.na(dfx2$country.of.publication_new))
table(is.na(dfx2$paper.authors_new))

dfx2[!is.na(author_new)]$author <- dfx2[!is.na(author_new)]$author_new; dfx2$author_new <- NULL
dfx2[!is.na(date.n_new)]$date.n <- dfx2[!is.na(date.n_new)]$date.n_new; dfx2$date.n_new <- NULL
dfx2[!is.na(journal_new)]$journal <- dfx2[!is.na(journal_new)]$journal_new; dfx2$journal_new <- NULL
dfx2[!is.na(title_new)]$title <- dfx2[!is.na(title_new)]$title_new; dfx2$title_new <- NULL
dfx2[!is.na(volume_new)]$volume <- dfx2[!is.na(volume_new)]$volume_new; dfx2$volume_new <- NULL
dfx2[!is.na(issue_new)]$issue <- dfx2[!is.na(issue_new)]$issue_new; dfx2$issue_new <- NULL
dfx2[!is.na(page.numbers.publication_new)]$page.numbers.publication <- dfx2[!is.na(page.numbers.publication_new)]$page.numbers.publication_new; dfx2$page.numbers.publication_new <- NULL
dfx2[!is.na(paper.type_new)]$paper.type <- dfx2[!is.na(paper.type_new)]$paper.type_new; dfx2$paper.type_new <- NULL
dfx2[!is.na(country.of.publication_new)]$country.of.publication <- dfx2[!is.na(country.of.publication_new)]$country.of.publication_new; dfx2$country.of.publication_new <- NULL
dfx2[!is.na(city.of.publication_new)]$city.of.publication <- dfx2[!is.na(city.of.publication_new)]$city.of.publication_new; dfx2$city.of.publication_new <- NULL
dfx2[!is.na(paper.authors_new)]$paper.authors <- dfx2[!is.na(paper.authors_new)]$paper.authors_new; dfx2$paper.authors_new <- NULL
dim(dfx2)

write.csv(dfx1, 
        paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.1-clean-journals_species.csv"), na='', row.names=F, fileEncoding="UTF-8")

write.csv(dfx2, 
        paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.1-clean-journals_species.csv"), na='', row.names=F, fileEncoding="UTF-8")

# *TODO: Missing records
auth[author=="Yasumatsu" & date.n=="1937"]
dfx2[idx==23428][,c("genus", "species", "author.date")]
dfx2[idx==23429][,c("genus", "species", "author.date")]

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - count number of species in publication
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- count number of species in publication"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 pub_1.0-clean")
auth <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
auth[, names(auth) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.1-clean-journals_species.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')[,c("idx", "status")]
dfx1$idx <- as.numeric(dfx1$idx)

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.1-clean-journals_species.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')[,c("idx", "status")]
dfx2$idx <- as.numeric(dfx2$idx)

max0 <- max(dfx1$idx)
min1 <- min(dfx2[status=="Synonym"]$idx); max1 <- max(dfx2[status=="Synonym"]$idx)
min2 <- min(dfx2[status=="Valid subspecies"]$idx); max2 <- max(dfx2[status=="Valid subspecies"]$idx)
min3 <- min(dfx2[status=="Infrasubspecific"]$idx); max3 <- max(dfx2[status=="Infrasubspecific"]$idx)

auth2 <- data.table(auth %>% separate_rows(idxes, sep="; "))
dim(auth2); auth2 <- unique(auth2); dim(auth2)

summarise <- auth2[, list(idxes=paste0(idxes,collapse='; '), 
            N_species=length(unique(idxes)), 
            N_species_valid=sum(unique(idxes) %in% 1:max0), 
            N_species_syn=sum(unique(idxes) %in% min1:max1), 
            N_species_ss=sum(unique(idxes) %in% min2:max2), 
            N_species_inf=sum(unique(idxes) %in% min3:max3)), 
        by=c("date.n", "author", "title", "journal", "volume", "issue", "page.numbers.publication", "paper.type", "country.of.publication", "city.of.publication", "paper.authors")]

write.csv(summarise, 
        paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 pub_2.0-metrics.csv"), na='', row.names=F, fileEncoding="UTF-8")
