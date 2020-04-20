source('2019-06-19-ascher-type-data/init/init.R')

filepath <- paste0(dir_data, basefile, " collectors_3.0-collectors.csv")
coll <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
coll[, names(coll) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, basefile, " describers_5.0-describers-final.csv")
des <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
des[, names(des) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, basefile, " filtered_4.3-clean-coll.csv")
df1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df1[, names(df1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, basefile, " oth_4.3-clean-coll.csv")
df2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df2[, names(df2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, basefile, " filtered_1-idx.csv")
df1o <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df1o[, names(df1o) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, basefile, " oth_1-idx.csv")
df2o <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df2o[, names(df2o) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir_data, basefile, " pub_1.0-clean.csv")
pub <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
pub[, names(pub) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
