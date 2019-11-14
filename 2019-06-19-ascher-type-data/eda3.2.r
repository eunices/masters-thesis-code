
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - load data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- load data"))

# Read/initialize all data

# Plausible UN data
un_path <- "data/2019-11-11-un-indicators/"
df_r <- fread(paste0(un_path, '2019-11-12-indicators.csv'), encoding="UTF-8", stringsAsFactors=F, na=c(""))
names_df_r <- df_r[1,]; df_r <- df_r[-1,]
names(df_r) <- unlist(names_df_r, use.names=FALSE)

# Lookup table for countries
lu <- fread('data/lookup/2019-05-29-statoid-country-codes.csv',  encoding="UTF-8")

# Denormalised authors
filepath <- '2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv'
dat <- fread(paste0(dir_data, filepath), encoding="UTF-8", stringsAsFactors=F, na=c(""))
cols <- c('idxes', 'full.name.of.describer.n', 'idxes_author.order', 'date.n'); dat <- dat[,..cols]

# Author info
auth <- get_des(write=F)
auth <- auth[, c('full.name.of.describer.n', 'residence.country.describer.n', 'describer.gender.n')]
auth$residence.country.describer.n <- sapply(auth$residence.country.describer.n, function(x) strsplit(x, "; ")[[1]][1])
auth <- merge(auth, lu[, c("DL", "Country")], all.x=T, all.y=F, 
              by.x="residence.country.describer.n", by.y="DL")
auth$residence.country.describer.n <- NULL

# Merge dataframes
dat <- merge(dat, auth[, c("full.name.of.describer.n", "describer.gender.n", "Country")],
             all.x=T, all.y=F, by="full.name.of.describer.n")

generate_prop_t <- function(country="All") { 
    # Filtering for each country should happen here
    print(head(dat))
    if (country != "All") {
        dat <- dat[Country == country]
    }

    # count N by gender
    prop <- dat[, .N,by=c("date.n", "describer.gender.n")]
    prop <- dcast(prop, date.n ~ describer.gender.n, value.var="N")

    # normalize date to minimum year with >0
    prop$date <- prop$date.n - min(prop$date.n)
    prop <- merge(prop, data.frame(date=0:max(prop$date)),
                all.x=T, all.y=T, by="date")
    prop[is.na(prop)] <- 0

    # tabulate proportion
    prop$prop_F <- prop$F / (prop$M + prop$F)
    prop$N <- prop$F + prop$M

    # filter data to year with at least 1 female
    first_year_female <- min(prop[F>0]$date)
    prop <- prop[date>first_year_female]
    prop$date <- prop$date - (first_year_female + 1)

    # rename data
    names(prop)[which(names(prop) == 'F')] <- "nFemales"
    names(prop)[which(names(prop) == 'M')] <- "nMales"
    names(prop)[which(names(prop) == 'N')] <- "n"

    return prop
}