source('2019-06-19-ascher-type-data/init.r')


# Purpose of this is to test if foreign keys match

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - read all "source" dataframes
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- read all source dataframes"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_3.0-collectors.csv")
coll <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
coll[, names(coll) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv")
des <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
des[, names(des) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-col.csv")
df1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df1[, names(df1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_2-clean.csv")
df2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df2[, names(df2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_1-idx.csv")
df1o <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df1o[, names(df1o) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_1-idx.csv")
df2o <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df2o[, names(df2o) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 pub_1.0-clean_edit.csv")
pub <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
pub[, names(pub) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - checks
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- checks"))

# invalid_species and species
# df1 and df2
dfx1 <- df1[,c("idx", "genus", "species")]
dfx2 <- df2[status=="Synonym",c("idx", "genus", "correct_synonym")]

df1o[Genus=="Allodape" & Species=="mucronota"]

check1 <- merge(dfx1, dfx2, by.x=c("genus", "species"), 
                by.y=c("genus", "correct_synonym"), 
                all.x=F, all.y=T)

table(is.na(check1$idx.x))
table(is.na(check1$idx.y))
# all synonyms have a valid specie s

# publication and (species + invalid_species)
# pub and (df1 and df2)


pubs <- pub %>% separate_rows(idxes)
df1_pub <- merge(df1, pubs, by.x='idx', by.y='idxes', all.x=T, all.y=F)
# df1_pub[is.na(journal.y)]
table(is.na(df1_pub$journal.y))
table(df1_pub$author.x == df1_pub$author.y)

df2_pub <- merge(df2, pubs, by.x='idx', by.y='idxes', all.x=T, all.y=F)
# df1_pub[is.na(journal.y)]
table(is.na(df2_pub$journal.y))
table(df2_pub$author.x == df2_pub$author.y)

# Manual edits


write.csv(df2_pub[author.x != author.y], 'tmp/test.csv')

# describers and (species + invalid_species)


# collectors and (species + invalid_species)


# variable checks

df1[host.plant.of.type != "",.(.N), by=.(host.plant.of.type)][order(-N)][,sum(N)]
df2[host.plant.of.type != "",.(.N), by=.(host.plant.of.type)][order(-N)]
