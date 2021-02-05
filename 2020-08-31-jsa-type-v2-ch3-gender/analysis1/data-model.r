# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Gender analysis: utility  functions.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# adapted from https://github.com/lukeholman/genderGapCode/

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - load data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- load data"))

source('2020-08-31-jsa-type-v2/subset.r')
source('2020-08-31-jsa-type-v2-ch3-gender/analysis1/fun.r')
source('2020-08-31-jsa-type-v2-ch3-gender/analysis1/params.r')

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
df <- get_df()

dat <- df[!is.na(full.name.of.describer), 
    c('idx', 'full.name.of.describer', 'date')
]

dat$idx <- as.integer(dat$idx)
dat$date.n <- as.integer(dat$date)
dat$date <- NULL
dat <- dat[date.n <= CURRENT_YEAR]

dat <- data.table(separate_rows(dat, full.name.of.describer, sep="; "))
dat[, auth.i:=seq_len(.N), by=c("idx")]
dat[, N:=.N, by=c("idx")]

dat$auth.i.n <- as.character(dat$auth.i)

dat[auth.i == N]$auth.i.n <- "L"
dat[auth.i == N & auth.i == 2]$auth.i.n <- "S"

table(dat$auth.i)
table(dat$auth.i.n)

dat$auth.i <- NULL
dat$auth.i <- dat$auth.i.n
dat$auth.i.n <- NULL

head(dat, 5)

# Author info
auth_full <- get_des()

auth <- auth_full[(ns_spp_N + syn_spp_N) >=1, c(
        'full.name.of.describer', 'residence.country.describer',
        'residence.country.describer.first',
        'describer.gender', "min", "max_corrected"
)]

# Move first to variable
auth$residence.country.describer <- auth$residence.country.describer.first
auth$residence.country.describer.first <- NULL

auth <- merge(
    auth, lu[, c("DL", "Country")], all.x=T, all.y=F, 
    by.x="residence.country.describer", by.y="DL"
)

auth$residence.country.describer <- NULL
countries <- c(auth[!is.na(Country), .N, by=Country][order(-N)]$Country)

# "auth" looks like this
#    full.name.of.describer describer.gender  min max_corrected   Country
# 1:           Kamel Louadi                M 2011          2019   Algeria
# 2:       Noudjoud Benarfa                F 2011          2011   Algeria
# 3:         Adolfo Doering                M 1875          1875 Argentina

print(table(dat$auth.i))

# Merge dataframes
dat <- merge(
    dat, auth[, c("full.name.of.describer", "describer.gender", "Country")],
    all.x=T, all.y=F, by="full.name.of.describer"
)

# "dat" looks like this
#    full.name.of.describer   idx date auth.i describer.gender      Country
# 1:              A. Hensel 22047 1870      1                M         <NA>
# 2:   Abdulaziz S. Alqarni 17222 2012      1                M Saudi Arabia
# 3:   Abdulaziz S. Alqarni 19165 2012      1                M Saudi Arabia

# For taxonomist
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=auth$min, b=auth$max_corrected)

auth$years <- seq
auth_years <- data.table(unnest(auth, years))
auth$years <- NULL

# "auth_years" looks like this
#    full.name.of.describer describer.gender  min max_corrected Country years
# 1:           Kamel Louadi                M 2011          2019 Algeria  2011
# 2:           Kamel Louadi                M 2011          2019 Algeria  2012
# 3:           Kamel Louadi                M 2011          2019 Algeria  2013
