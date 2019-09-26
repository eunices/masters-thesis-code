# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - cleaning repository
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- cleaning repository"))

filepath <- paste0(dir_data, '2019-05-23-Apoidea world consensus file Sorted by name 2019-idx-1-geocoded.csv')
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# clean repository
filepath <- paste0(dir_data, "clean/check-type-repo2_edit.csv")
edit <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
edit[, names(edit) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

dim(edit[country.of.type.repository.n_short != "[unknown]"][duplicated(country.of.type.repository.n_short)])

edit <- edit[, c("country.of.type.repository.n_short", "country.of.type.repository.n_long",
                 "type.repository.n_short", "type.repository.n_long", "idxes")]

edit <- edit %>% separate_rows(idxes, sep="; ")
df <- merge(df, edit, all.x=T, all.y=F, by.x="idx", by.y="idxes")

write.csv(df[order(as.numeric(idx))], 
          paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019-idx-2-clean-repo.csv"), na='', row.names=F, fileEncoding="UTF-8")
