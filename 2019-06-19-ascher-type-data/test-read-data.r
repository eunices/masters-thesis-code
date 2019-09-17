source('2019-06-19-ascher-type-data/init.r')

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_3.0-collectors.csv")
coll <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
coll[, names(coll) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv")
des <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
des[, names(des) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-coll.csv")
# filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.1-clean-journals_species.csv")
# filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.2-clean-auth-full-name.csv")
df1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df1[, names(df1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.3-clean-coll.csv")
df2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df2[, names(df2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_1-idx.csv")
df1o <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df1o[, names(df1o) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_1-idx.csv")
df2o <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df2o[, names(df2o) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 pub_1.0-clean.csv")
pub <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
pub[, names(pub) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
