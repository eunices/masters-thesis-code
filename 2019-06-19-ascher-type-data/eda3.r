source('2019-06-19-ascher-type-data/subset.r')

library(dplyr)
library(RSQLite)

library(tidyverse)
library(httr)
library(jsonlite)

theme <- theme_classic()

source('2019-06-19-ascher-type-data/eda3_util.r')
# source('2019-06-19-ascher-type-data/eda3.1.r')
source('2019-06-19-ascher-type-data/eda3.2.r')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep analysis
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender rep analysis"))

# adapted from https://github.com/lukeholman/genderGapCode/

# as a whole

res <- find.response.variables(prop)
df_p <- data.frame(x=1:200,
                   y=sapply(1:200, pfunc, r=res$r, c=res$c))
df_p <- merge(df_p, prop[, c('date', 'prop_F')], by.x="x", by.y="date", all.x=T, all.y=T)

ggplot(df_p) + 
  geom_bar(stat="identity",  aes(x=x, y=y)) + 
  geom_bar(stat="identity", aes(x=x, y=prop_F), fill = "#FF6666") + 
  theme + xlab("Year") + ylab("Proportion of authors (%)")