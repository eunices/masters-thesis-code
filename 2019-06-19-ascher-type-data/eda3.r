source('2019-06-19-ascher-type-data/subset.r')

library(tidyverse)
library(httr)
library(jsonlite)

theme <- theme_classic()

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - get UN data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- get UN data"))

# http://ec2-54-174-131-205.compute-1.amazonaws.com/API/Information.php

indicators <- c(24106,  # GDI: Mean years of schooling (females aged 25 years and above)
                24206,  # GDI: Mean years of schooling (males aged 25 years and above)
                120606, # GDI: Life expectancy at birth, female
                121106, # GDI: Life expectancy at birth, male
                123306, # GDI: Expected years of schooling, females
                123406, # GDI: Expected years of schooling, males
                123506, # GDI: Estimated GNI per capita (PPP), female
                123606, # GDI: Estimated GNI per capita (PPP), male
                137906  # GDI
)
query_indicator <- do.call(paste, c(as.list(indicators), sep = ","))
path <- paste0(
    "http://ec2-54-174-131-205.compute-1.amazonaws.com/API/HDRO_API.php/indicator_id=",
    query_indicator, "/year=1990,2017/structure=cyi")

request <- GET(url = path)
request$status_code
response <- content(request, as = "text", encoding = "UTF-8")
parsed_json <- fromJSON(response, flatten = TRUE)

df <-  parsed_json$indicator_value %>% data.frame() %>% t() %>% data.frame()
df$col <- rownames(df)
df <- data.table(df)
df[, c('country', 'year', 'indicator') := tstrsplit(col, ".", fixed=TRUE)]
names(df)[which(names(df)==".")] <- "value"

df_r <- dcast(df[year==2017], country ~ indicator, value.var="value")
df_r

un_path <- "data/2019-11-11-un-indicators/"
dir.create(un_path)
write.csv(df_r, paste0(un_path, '2019-11-12-indicators.csv'), na='', row.names=F, fileEncoding="UTF-8")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender representation
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender representation"))

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


ggplot(prop, aes(x=date.n, y=prop_F)) + 
  geom_bar(stat="identity") + theme + xlab("Year") + ylab("Proportion of authors (%)")


# obtained from https://github.com/lukeholman/genderGapCode/blob/master/Plot%20and%20analysis%20functions.R

pfunc <- function(v) {
    t = v[1]; r = v[2]; c = v[3]
    exp(0.5*r*t) / (2*exp(0.5*r*t) + c)
}

pfunc.deriv <- function(p, r) r*p*(0.5-p)

# li <- sapply(1:100, function(x) pfunc(c(x, 1, 1)))
# plot(1:100, li)
li <- sapply(1:100, function(x) pfunc(c(x, 0.5, 1)))
points(1:100, li)

# suppressWarnings(find.ll())
find.ll <- function(data, par) {
  r = par[1]; c = par[2]
  -1 * sum(dbinom(x = data$F, size = data$N, prob = pfunc(c(data$date, r, c)), log = TRUE))
}

res <- optim(par = c(0.1, 1), find.ll, data = prop)

get_vector <- function(x, res) {
  # print(paste(x, res$par[1], res$par[2]))
  c(x, res$par[1], res$par[2])
}
vec <- sapply(prop$date, get_vector, res=res)
y <- apply(vec, 2, pfunc)

df_p <- data.frame(x=0:(length(y)-1), 
                   y=y)
df_p <- merge(df_p, prop[, c('date', 'prop_F')], by.x="x", by.y="date", all.x=T, all.y=T)

ggplot(df_p) + 
  geom_bar(stat="identity",  aes(x=x, y=y)) + 
  geom_bar(stat="identity", aes(x=x, y=prop_F), fill = "#FF6666") + 
  theme + xlab("Year") + ylab("Proportion of authors (%)")


# as a whole
# % of authors
# rate of change
# N number of years till parity



# by continent
# % of authors
# rate of change
# N number of years till parity