# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for wrangling publication fields and author names,
# as well as to tabulate publication-specific metrics.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# TODO: shift derived variables/dataframes to a different script?

print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting df1.r"))
print("######################################################")
print("######################################################")
print("######################################################")

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
print(paste0(Sys.time(), " --- clean journal names"))

filepath <- paste0(dir_data, basefile, " filtered_1-clean.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx1[, names(dfx1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, basefile, " oth_1-clean.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2[, names(dfx2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

cols <- c("idx", "genus", "species", "date.n", "author", "paper.type", 
          "title", "journal", "volume", "issue", "page.numbers.publication")

dfx <- rbind(dfx1[,..cols], dfx2[,..cols])

df_n <- data.table(dfx %>%
  group_by(date.n, author, title, journal,  volume,  issue, page.numbers.publication) %>%
  summarise(idxes=paste0(idx, collapse='; ')))

filename_write = paste0(dir_data, "clean/journal_details.csv")
write.csv(df_n, filename_write, na='', row.names=F, fileEncoding="UTF-8")

filename_journal <- paste0(dir_data, "clean/journal_names_edit.csv")
df_n <- fread(filename_journal, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df_n <- df_n[, c("journal_new", "idxes")]

df_n <- df_n %>% separate_rows(idxes)

dfx1 <- merge(dfx1, df_n, by.x='idx', by.y='idxes', all.x=T, all.y=F)
dfx1$journal <- dfx1$journal_new
dfx1$journal_new <- NULL

dfx2 <- merge(dfx2, df_n, by.x='idx', by.y='idxes', all.x=T, all.y=F)
dfx2$journal <- dfx2$journal_new
dfx2$journal_new <- NULL

filename_dfx1 = paste0(dir_data, basefile, " filtered_4.0-clean-journals.csv")
write.csv(dfx1[order(as.numeric(idx))], filename_dfx1, na='', row.names=F, fileEncoding="UTF-8")

filename_dfx2 = paste0(dir_data, basefile, " oth_4.0-clean-journals.csv")
write.csv(dfx2[order(as.numeric(idx))], filename_dfx2, na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean journal fields
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean other publication fields"))
# "publication.type", "country.of.publication", "city.of.publication", "paper.authors"

filename_dfx1 <- paste0(dir_data, basefile, " filtered_4.0-clean-journals.csv")
dfx1 <- fread(filename_dfx1, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx1[, names(dfx1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filename_dfx2 <- paste0(dir_data, basefile, " oth_4.0-clean-journals.csv")
dfx2 <- fread(filename_dfx2, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2[, names(dfx2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

# # Iterative cleaning process
# filepath <- paste0(dir_data, basefile, " pub_1.0-clean3.csv")
# auth <- fread(filepath, integer64='character', na.strings=c(''), encoding='UTF-8')
# auth[, names(auth) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

# auth2 <- data.table(auth %>% separate_rows(idxes))

# auth <- data.table(auth2 %>%
#   group_by(date.n, author, title, journal, volume, issue, page.numbers.publication, 
#            paper.type, country.of.publication, city.of.publication, paper.authors) %>%
#   summarise(idxes=paste0(idxes,collapse='; ')))

# filename_write2 = paste0(dir_data, basefile, " pub_1.0-clean4.csv")
# write.csv(auth, filename_write2, na='', row.names=F, fileEncoding="UTF-8")
# # = old name journal_details*.csv

filename_read2 <- paste0(dir_data, basefile, " pub_1.0-clean.csv")
auth <- fread(filename_read2, integer64='character', na.strings=c('NA'), encoding='UTF-8')
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

dfx2 <- merge(dfx2, auth2, all.x=T, all.y=F, by.x='idx', by.y='idxes', suffixes=c("", "_new"))

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

# Row-based changes
dfx2[idx=="21229", ]$author <- 'Linnaeus'
dfx2[idx=="22983", ]$author <- 'Verhoeff [C.]'
dfx2[idx=="23832", ]$author <- 'Evans [W.]'
dfx2[idx=="23865", ]$author <- 'White [A.]'
dfx2[idx=="24018", ]$author <- 'Fox [W. J.]'
dfx2[idx=="27625", ]$author <- 'Smith [F.]'
dfx2[idx=="30048", ]$author <- 'Cockerell and Porter'
dfx2[idx=="31671", ]$author <- 'Linnaeus'
dfx2[idx=="32004", ]$author <- 'White [J. R.]'
dfx2[idx=="32030", ]$author <- 'White [J. R.]'
dfx2[idx=="33101", ]$author <- 'Cockerell'

dfx1$idxes <- gsub(';$', '', dfx$idxes)

filename_write = paste0(dir_data, basefile, " filtered_4.1-clean-journals_species.csv")
write.csv(dfx1, filename_write, na='', row.names=F, fileEncoding="UTF-8")

filename_write = paste0(dir_data, basefile, " oth_4.1-clean-journals_species.csv")
write.csv(dfx2, filename_write, na='', row.names=F, fileEncoding="UTF-8")

# *TODO: Missing records
auth[author=="Yasumatsu" & date.n=="1937"]
dfx2[idx==23428][,c("genus", "species", "author.date")]
dfx2[idx==23429][,c("genus", "species", "author.date")]


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - ensuring author name and full name are consistent
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- ensuring author name and full name are consistent"))

filepath <- paste0(dir_data, basefile, " filtered_4.1-clean-journals_species.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
dfx1[, names(dfx1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, basefile, " oth_4.1-clean-journals_species.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
dfx2[, names(dfx2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, "clean/missing_authors_edit.csv")
auth <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
auth[, names(auth) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

dfx1 <- merge(dfx1, auth, by="author", all.x=T, all.y=F, suffixes=c("", "_new"))
table(is.na(dfx1$full.name.of.describer_new))
dfx1 <- data.table(dfx1)

dfx1$full.name.of.describer <- dfx1$full.name.of.describer_new
dfx1$full.name.of.describer_new <- NULL

# dfx1[full.name.of.describer != full.name.of.describer_new]$full.name.of.describer <- 
#   dfx1[full.name.of.describer != full.name.of.describer_new]$full.name.of.describer_new
# dfx1$full.name.of.describer_new <- NULL

dfx2 <- merge(dfx2, auth, by="author", all.x=T, all.y=F, suffixes=c("", "_new"))
table(is.na(dfx2$full.name.of.describer_new))
dfx2 <- data.table(dfx2)

dfx2$full.name.of.describer <- dfx2$full.name.of.describer_new
dfx2$full.name.of.describer_new <- NULL

# dfx2[full.name.of.describer != full.name.of.describer_new]$full.name.of.describer <- 
#  dfx2[full.name.of.describer != full.name.of.describer_new]$full.name.of.describer_new
# dfx2$full.name.of.describer_new <- NULL

# Row-based changes
dfx1[idx==14019]$author = "Sakagami and Ebmer"
dfx1[idx==14019]$full.name.of.describer = "Andreas Werner Ebmer; Yasuo Maeta"
dfx1[idx==15197]$author = "Astafurova and Proshchalykin"
dfx1[idx==16275]$full.name.of.describer = 
    "Victor Hugo Gonzalez [Betancourt]; Michael Scott Engel; Terry L. Griswold"
dfx1[idx %in% c(18134, 16712)]$full.name.of.describer = "Ze-qing Niu; Yan-ru Wu; Chao-dong Zhu"
dfx1[idx==18134]$species = "guangxiense"
dfx1[idx==3839]$full.name.of.describer = "Wang S.-f."
dfx1[idx==7360]$full.name.of.describer = 
    "Maximilian Schwarz; Fritz Josef [Friedrich] Gusenleitner; Karl Mazzucco"
dfx1[idx==7360]$full.name.of.describer = 
    "Maximilian Schwarz; Fritz Josef [Friedrich] Gusenleitner; Karl Mazzucco"
dfx1[idx==14019]$full.name.of.describer = auth[author=="Sakagami and Ebmer"]$full.name.of.describer
dfx1[idx==14306]$full.name.of.describer = "Rui Zhang; Ze-qing Niu; Qiang Li"
dfx1[idx==1790]$full.name.of.describer = 
    "Sébastien Patiny; Francisco Javier Ortiz-Sánchez; Denis Michez"

dfx2[idx==24043]$date.n = "1900"
dfx2[idx==24113]$date.n = "1900"
dfx2[idx==22919]$full.name.of.describer = "Joseph Bequaert"
dfx2[idx %in% c(29157, 29158)]$full.name.of.describer = "Bronislaw Debski"
dfx2[idx == 30619]$full.name.of.describer = "James R. Baker"
dfx2[idx == 22956]$full.name.of.describer = "G. Trautmann; Woldemar Trautmann"

# Clean short names from full names
filepath <- paste0(dir_data, "clean/last_name.csv")
ln <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
fn <- ln$last.name
names(fn) <- ln$full.name.of.describer.n

format_short <- function(x){
    auths <- strsplit(x, split="; ")[[1]]
    len <- length(auths)
    auths <- fn[auths]

    if(len==1) {
        string <- auths
    } else if(len==2) {
        string <- paste0(auths[1], " and ", auths[2])
    } else if(len>=3) {
        string <- auths[1]
        for (i in 2:(len-1)) {
            string <- paste0(string, ", ", auths[i])
        }
        string <- paste0(string, ", and ", auths[len])
    }
    string
}

authors_redone <- lapply(dfx1$full.name.of.describer, format_short)
authors_redone <- as.data.frame(do.call(rbind, authors_redone), stringsAsFactors=F)
names(authors_redone) <- "x"
authors_redone$x <- as.character(authors_redone$x)
dfx1$author <- authors_redone$x

authors_redone <- lapply(dfx2$full.name.of.describer, format_short)
authors_redone <- as.data.frame(do.call(rbind, authors_redone), stringsAsFactors=F)
names(authors_redone) <- "x"
authors_redone$x <- as.character(authors_redone$x)
dfx2$author <- authors_redone$x

filename_write = paste0(dir_data, basefile, " filtered_4.2-clean-auth-full-name.csv")
write.csv(dfx1, filename_write, na='', row.names=F, fileEncoding="UTF-8")

filename_write = paste0(dir_data, basefile, " oth_4.2-clean-auth-full-name.csv")
write.csv(dfx2, filename_write, na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - count number of species in publication
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- count number of species in publication"))

filename <- paste0(dir_data, basefile, " pub_1.0-clean.csv")
auth <- fread(filename, integer64='character', na.strings=c('NA'), encoding='UTF-8')
auth[, names(auth) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filename <- paste0(dir_data, basefile, " filtered_4.2-clean-auth-full-name.csv")
cols <- c("idx", "status")
dfx1 <- fread(filename, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')[, ..cols]
dfx1$idx <- as.numeric(dfx1$idx)

filename <- paste0(dir_data, basefile, " oth_4.2-clean-auth-full-name.csv")
dfx2 <- fread(filename, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')[, ..cols]
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
        by=c("date.n", "author", "title", "journal", "volume", "issue", "page.numbers.publication", 
             "paper.type", "country.of.publication", "city.of.publication", "paper.authors")]

filename_write = paste0(dir_data, basefile, " pub_2.0-metrics.csv")
write.csv(summarise, filename_write, na='', row.names=F, fileEncoding="UTF-8")
