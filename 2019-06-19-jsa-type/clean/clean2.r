# Information about code:
# This code corresponds to cleaning code for my MSc thesis.
# A series of other codes are named as clean1|2|3|4.r
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - cleaning repository
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- cleaning repository"))




# Read data
filepath <- paste0(dir_data, basefile, '-idx-1-geocoded.csv')
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
# fread does not escape double quotes




# Clean repository
filepath <- paste0(dir_data, "clean/check-type-repo2_edit.csv")
edit <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
edit[, names(edit) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
# fread does not escape double quotes

# !CHECK
dim(edit[country.of.type.repository.n_short != "[unknown]"][
	duplicated(country.of.type.repository.n_short)])

# Merge info back
edit <- edit[, c("country.of.type.repository.n_short", "country.of.type.repository.n_long",
                 "type.repository.n_short", "type.repository.n_long", "idxes")]
edit <- edit %>% separate_rows(idxes, sep="; ")
df <- merge(df, edit, all.x=T, all.y=F, by.x="idx", by.y="idxes")




# Write output
write.csv(df[order(as.numeric(idx))], paste0(dir_data, basefile, "-idx-2-clean-repo.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")
