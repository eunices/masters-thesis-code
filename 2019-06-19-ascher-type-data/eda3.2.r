
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - load data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- load data"))

un_path <- "data/2019-11-11-un-indicators/"
df_r <- fread(paste0(un_path, '2019-11-12-indicators.csv'), encoding="UTF-8", stringsAsFactors=F, na=c(""))
names_df_r <- df_r[1,]; df_r <- df_r[-1,]
names(df_r) <- unlist(names_df_r, use.names=FALSE)

filepath <- '2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv'
dat <- fread(paste0(dir_data, filepath), encoding="UTF-8", stringsAsFactors=F, na=c(""))
cols <- c('idxes', 'full.name.of.describer.n', 'idxes_author.order', 'date.n')
dat <- dat[,..cols]

lu <- fread('data/lookup/2019-05-29-statoid-country-codes.csv',  encoding="UTF-8")

auth <- get_des(write=F)
auth <- auth[, c('full.name.of.describer.n', 'residence.country.describer.n', 'describer.gender.n')]
auth$residence.country.describer.n <- sapply(auth$residence.country.describer.n, function(x) strsplit(x, "; ")[[1]][1])
auth <- merge(auth, lu[, c("DL", "Country")], all.x=T, all.y=F, 
              by.x="residence.country.describer.n", by.y="DL")
auth$residence.country.describer.n <- NULL

dat <- merge(dat, auth[, c("full.name.of.describer.n", "describer.gender.n")],
             all.x=T, all.y=F, by="full.name.of.describer.n")
prop <- dat[, .N,by=c("date.n", "describer.gender.n")]
prop <- dcast(prop, date.n ~ describer.gender.n, value.var="N")
prop$date <- prop$date.n - min(prop$date.n)
prop <- merge(prop, data.frame(date=0:max(prop$date)),
              all.x=T, all.y=T, by="date")
prop[is.na(prop)] <- 0
prop$prop_F <- prop$F / (prop$M + prop$F)
prop$N <- prop$F + prop$M

first_year_female <- min(prop[F>0]$date)
prop <- prop[date>first_year_female]
prop$date <- prop$date - (first_year_female + 1)
