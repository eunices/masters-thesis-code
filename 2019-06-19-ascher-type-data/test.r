
# Purpose of this is to test 1) if foreign keys match and 2) if rows are consistent

# Specific aims:
# - Do not repeat variables between dataframes
# - Reduce variable dependences, and if there is ensure they are consistent

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - read all "source" dataframes
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- read all source dataframes"))
source('2019-06-19-ascher-type-data/test-read-data.r')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - checks
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- checks"))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# test 1) if foreign keys match 
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#############################
# invalid_species and species - df1 and df2
#############################

dfx1 <- df1[,c("idx", "genus", "species")]
dfx2 <- df2[status=="Synonym", c("idx", "genus", "correct_synonym")]

df1o[Genus=="Allodape" & Species=="mucronota"]

check1 <- merge(dfx1, dfx2, by.x=c("genus", "species"), 
                by.y=c("genus", "correct_synonym"), 
                all.x=F, all.y=T)

table(is.na(check1$idx.x))
# table(is.na(check1$idx.y))
# TEST THAT all synonyms have a valid species

# publication and (species + invalid_species)
# pub and (df1 and df2)
pubs <- pub %>% separate_rows(idxes)
df1_pub <- merge(df1, pubs, by.x='idx', by.y='idxes', all.x=T, all.y=F)
table(is.na(df1_pub$journal.y))
table(df1_pub$author.x == df1_pub$author.y)
table(df1_pub$journal.x == df1_pub$journal.y)
table(df1_pub$title.x == df1_pub$title.y)
table(df1_pub$volume.x == gsub("^'", "", df1_pub$volume.y))
table(df1_pub$issue.x == gsub("^'", "", df1_pub$issue.y))
table(df1_pub$page.numbers.publication.x == gsub("^'", "", df1_pub$page.numbers.publication.y))

dim(df1_pub)
dim(df1_pub[grepl("^'", volume.y) | volume.y==""]) 
dim(df1_pub[grepl("^'", issue.y) | issue.y==""]) 
dim(df1_pub[grepl("^'", page.numbers.publication.y) | page.numbers.publication.y==""]) 

df2_pub <- merge(df2, pubs, by.x='idx', by.y='idxes', all.x=T, all.y=F)
table(is.na(df2_pub$journal.y))
table(df2_pub$author.x == df2_pub$author.y)
table(df2_pub$journal.x == df2_pub$journal.y)
table(df2_pub$title.x == df2_pub$title.y)
table(df2_pub$volume.x == gsub("^'", "", df2_pub$volume.y))
table(df2_pub$issue.x == gsub("^'", "", df2_pub$issue.y))
table(df2_pub$page.numbers.publication.x == gsub("^'", "", df2_pub$page.numbers.publication.y))
dim(df2_pub)
dim(df2_pub[grepl("^'", volume.y) | volume.y==""]) 
dim(df2_pub[grepl("^'", issue.y) | issue.y==""]) 
dim(df2_pub[grepl("^'", page.numbers.publication.y) | page.numbers.publication.y==""]) 
# all the pub stuff match
# this implies that I can store the data with keys for:
# df* = author as a key
# pub = publication info as key without author

#############################
# describers and (species + invalid_species) - des and (df1 and df2) 
#############################

# test that the describer full names are the same
dess <- des[,c('full.name.of.describer.n', 'last.name', 'spp_idxes')] %>% separate_rows(spp_idxes)
dess <- data.table(dess %>% 
    group_by(spp_idxes) %>%
    summarise(full.name.of.describer=paste0(sort(unique((full.name.of.describer.n))), collapse="; ")))
dess$spp_idxes <- as.integer(dess$spp_idxes)
# dess[, lapply(.SD, toString), by=.(spp_idxes)] # equivalent but cannot specify separator
dess <- dess[order(spp_idxes)]

dfx1 <- df1 %>% separate_rows(full.name.of.describer, sep="; ")
dfx1 <- data.table(dfx1 %>% 
    group_by(idx) %>%
    summarise(full.name.of.describer_sorted=paste0(sort(unique((full.name.of.describer))), collapse="; ")))
dfx1$idx <- as.integer(dfx1$idx)
dfx1 <- dfx1[order(idx)]

df1_des <- merge(dfx1, dess, by.x='idx', by.y='spp_idxes', all.x=T, all.y=F)
table(df1_des$full.name.of.describer == df1_des$full.name.of.describer_sorted)

test <- df1_des[full.name.of.describer != full.name.of.describer_sorted]
write.csv(test, 'tmp/test.csv')

df1$idx <- as.numeric(df1$idx)
df1_des <- merge(df1, dess, by.x='idx', by.y='spp_idxes', all.x=T, all.y=F)
table(df1_des$full.name.of.describer.x == df1_des$full.name.of.describer.y)
# test that the author (short form) is the same as the full.name


fn <- des$last.name
names(fn) <- des$full.name.of.describer.n


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

format_short("William Forsell Kirby")
format_short("William Kirby; Gideon Pisanty")
format_short("Erwin Scheuchl; Gideon Pisanty; Gideon Pisanty; Gideon Pisanty")
format_short("Erwin Scheuchl; Gideon Pisanty; Gideon Pisanty; Gideon Pisanty")

authors_redone <- lapply(df1$full.name.of.describer, format_short)
authors_redone <- as.data.frame(do.call(rbind, authors_redone), stringAsFactors=F)
names(authors_redone) <- "x"
authors_redone$x <- as.character(authors_redone$x)

df1$authors_redone <- authors_redone$x
table(df1$author == df1$authors_redone)

test <- df1[author != authors_redone, c('idx', 'full.name.of.describer', 'author', 'authors_redone')]
write.csv(test, 'tmp/test.csv')
df1$authors_redone <- NULL

authors_redone <- lapply(df2$full.name.of.describer, format_short)
authors_redone <- as.data.frame(do.call(rbind, authors_redone), stringAsFactors=F)
names(authors_redone) <- "x"
authors_redone$x <- as.character(authors_redone$x)

df2$authors_redone <- authors_redone$x
table(df2$author == df2$authors_redone)
df2$authors_redone <- NULL

test <- df2[author != authors_redone, c('idx', 'full.name.of.describer', 'author', 'authors_redone')]
write.csv(test, 'tmp/test.csv')

df2[idx %in% c(26814, 29874), c('idx', 'author')]
#############################
# collectors and (species + invalid_species)
#############################

colls <- coll %>% separate_rows(idxes, sep="; ")
colls <- colls %>% 
    group_by(idxes) %>%
    summarise(collector.of.type.n = paste0(collector.of.type.n, collapse="; "),
              full.name.of.collector.n = paste0(full.name.of.collector.n, collapse="; "),
              uncertain = paste0(uncertain, collapse="; "),
              collector.gender.n = paste0(collector.gender.n, collapse="; "),
              title.of.collector.n = paste0(title.of.collector.n, collapse="; ")
              )

colls$idxes <- as.numeric(colls$idxes)
df1_coll <- merge(df1, colls, by.x="idx", by.y="idxes", all.x=T, all.y=F, 
                  suffixes=c("", "2"))

table(df1_coll$collector.of.type.n == df1_coll$collector.of.type.n2)
table(df1_coll$full.name.of.collector.n == df1_coll$full.name.of.collector.n2)
table(df1_coll$uncertain == df1_coll$uncertain2)
table(df1_coll$collector.gender.n == df1_coll$collector.gender.n2)
table(df1_coll$title.of.collector.n == df1_coll$title.of.collector.n)

colls$idxes <- as.character(colls$idxes)
df2_coll <- merge(df2, colls, by.x="idx", by.y="idxes", all.x=T, all.y=F, 
                  suffixes=c("", "2"))

table(df2_coll$collector.of.type.n == df2_coll$collector.of.type.n2)
table(df2_coll$full.name.of.collector.n == df2_coll$full.name.of.collector.n2)
table(df2_coll$uncertain == df2_coll$uncertain2)
table(df2_coll$collector.gender.n == df2_coll$collector.gender.n2)
table(df2_coll$title.of.collector.n == df2_coll$title.of.collector.n)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# test 2) if variables are consistent within data frame
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# publication date v date.n
dfx_dates <- rbind(df1[, c('idx', 'date.n')], df2[, c('idx', 'date.n')])
pubs <- pub %>% separate_rows(idxes)
pubs <- data.table(pubs)[, c("idxes", "date.n")]
dfx_dates <- merge(dfx_dates, pubs, by.x="idx", by.y="idxes", all.x=T, all.y=F, 
                   suffixes=c("", "_pub"))
dfx_dates$check <- dfx_dates$date.n == dfx_dates$date.n_pub

# NANs 
df1[host.plant.of.type != "",.(.N), by=.(host.plant.of.type)][order(-N)][,sum(N)]
df2[host.plant.of.type != "",.(.N), by=.(host.plant.of.type)][order(-N)]
