

# Groups of names that need to be cleaned

source("2019-06-19-jsa-type/subset.r")
df1 = get_df1(write=F)  # cleaned 
df = get_raw_data()     # raw

# rename column names
names(df) <- gsub("\\.\\.", "\\.", gsub(" ", ".", gsub("[[:punct:]]", "", tolower(names(df)))))
names(df) <- iconv(names(df), from = 'UTF-8', to = 'ASCII//TRANSLIT')
if (any(grepl("full.name.a.e", names(df)))) {
    names(df)[which(grepl("full.name.a.e", names(df)))] <- 'full.name' # renaming this long name
}

names(df1)[!(names(df1) %in% names(df))]

#  [1] "date.n"                             "flag"
#  [3] "source.of.latlon.n"                 "type.country.n"
#  [5] "type.state.n"                       "type.country.n.full"
#  [7] "type.state.n.full"                  "date.of.type.string"
#  [9] "date.of.type.dd"                    "date.of.type.mm"
# [11] "date.of.type.yyyy"                  "type.repository.n_short"
# [13] "type.repository.n_long"             "country.of.type.repository.n_short"
# [15] "country.of.type.repository.n_long"  "duplicated.row"
# [17] "years.lag"                          "N_synonyms"
# [19] "N_ss"                               "N_var"
