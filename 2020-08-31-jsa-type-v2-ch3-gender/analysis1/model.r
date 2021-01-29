# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Gender analysis: utility  functions.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# adapted from https://github.com/lukeholman/genderGapCode/

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - load data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- load data"))

source('2020-08-31-jsa-type-ch3-gender/analysis1/fun.r') # functions functions
# source('2020-08-31-jsa-type-ch3-gender/analysis1/data-un.r') # get UN data 

# Read/initialize all data/variables
CURRENT_YEAR <- 2018
theme <- theme_minimal()

# Plausible UN data
un_path <- "data/2019-11-11-un-indicators/"

df_r <- fread(
    paste0(un_path, '2019-11-12-indicators.csv'), encoding="UTF-8",
    stringsAsFactors=F, na=c("")
)

# Lookup table for countries
lu <- get_lp_statoid()

# For species

# Denormalised authors
dat = get_species_denormalised()
cols <- c('idxes', 'full.name.of.describer.n', 'idxes_author.order', 'date.n')
dat <- dat[,..cols]

# Author info
auth_full <- get_des(write=F)
auth_full <- auth_full[!is.na(min)]

auth <- auth_full[, c(
        'full.name.of.describer.n', 'residence.country.describer.n',
        'describer.gender.n', "min", "max_corrected"
    )]

auth$residence.country.describer.n <- sapply(
    auth$residence.country.describer.n, 
    function(x) strsplit(x, "; ")[[1]][1]
)

auth <- merge(
    auth, lu[, c("DL", "Country")], all.x=T, all.y=F, 
    by.x="residence.country.describer.n", by.y="DL"
)

auth$residence.country.describer.n <- NULL
countries <- c(auth[!is.na(Country), .N, by=Country][order(-N)]$Country)

print(table(dat$idxes_author.order))

# Merge dataframes
dat <- merge(
    dat, auth[, c("full.name.of.describer.n", "describer.gender.n", "Country")],
    all.x=T, all.y=F, by="full.name.of.describer.n"
)

# For taxonomist

seq <- mapply(function(a, b) {
    seq(a, b)
}, a=auth$min, b=auth$max_corrected)

auth$years <- seq
auth_years <- data.table(unnest(auth, years))
auth$years <- NULL

