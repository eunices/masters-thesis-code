source('2019-06-19-ascher-type-data/subset.r')

library(tidyverse)
library(httr)
library(jsonlite)

theme <- theme_classic()

# source('2019-06-19-ascher-type-data/eda3.1.r')
source('2019-06-19-ascher-type-data/eda3.2.r')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep analysis
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender rep analysis"))

# main function https://github.com/lukeholman/genderGapCode/blob/master/Plots%20and%20analyses.R
# obtained from https://github.com/lukeholman/genderGapCode/blob/master/Plot%20and%20analysis%20functions.R

ggplot(prop, aes(x=date.n, y=prop_F)) + 
  geom_bar(stat="identity") + theme + xlab("Year") + ylab("Proportion of authors (%)")

pfunc <- function(v) {
    t = v[1]; r = v[2]; c = v[3]
    exp(0.5*r*t) / (2*exp(0.5*r*t) + c)
}

pfunc.deriv <- function(p, r) r*p*(0.5-p)

# li <- sapply(1:100, function(x) pfunc(c(x, 1, 1)))
# plot(1:100, li)
# li <- sapply(1:100, function(x) pfunc(c(x, 0.5, 1)))
# points(1:100, li)

# suppressWarnings(find.ll())
find.ll <- function(data, par) {
  r = par[1]; c = par[2]
  -1 * sum(dbinom(x = data$F, size = data$M, prob = pfunc(c(data$date, r, c)), log = TRUE))
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