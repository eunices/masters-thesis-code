

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

## variables cleaned with new names

### used
date = date.n
type.country = type.country.n
type.country = type.country.n.full (lookup)

### not used
type.state = type.state.n
type.state = type.state.n.full
date.of.type = date.of.type.string
date.of.type = date.of.type.dd
date.of.type = date.of.type.mm
date.of.type = date.of.type.yyyy
date.of.type = date.of.type.yyyy
type.repository = type.repository.n_short
type.repository = type.repository.n_long 
country.of.type.repository = country.of.type.repository.n_long

## variables cleaned but with old names

# TODO:
### used


### not used



## new variables
### used
flag
source.of.latlon.n
duplicated.row

N_synonyms
N_ss
N_var

### not used
years.lag

# other variables not in the main dataset

## describer dataset



# TODO: identify datasets used by ch1-3, 
# TODO: add those code in here for processing from raw dataset