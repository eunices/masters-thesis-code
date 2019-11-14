source('2019-06-19-ascher-type-data/subset.r')


# Libraries
library(dplyr)
library(RSQLite)

library(tidyverse)
library(httr)
library(jsonlite)

# Scripts
source('2019-06-19-ascher-type-data/eda3_util.r') # util functions
# source('2019-06-19-ascher-type-data/eda3.1.r') # get data from UN 's API and save locally
source('2019-06-19-ascher-type-data/eda3.2.r') # read local/ bee data

# Parameters
theme <- theme_classic()

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep analysis
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender rep analysis"))

# adapted from https://github.com/lukeholman/genderGapCode/

##################################
# overall
##################################

# for plotting purposes
prop <- generate_prop_t(country="All")
res <- find.response.variables(prop)
max_predict_year <- as.numeric(paste0(as.numeric(substr(as.character(max(prop$date.n)), 1, 2)) + 1, "00"))
df_p <- data.frame(x= 0:max_predict_year+ min(prop$date.n),
                   y=sapply(0:max_predict_year, pfunc, r=res$r, c=res$c))
df_p <- merge(df_p, prop[, c('date.n', 'prop_F')], by.x="x", by.y="date.n", all.x=T, all.y=T)

df_p$y <- round(df_p$y*100, 5)
df_p$prop_F <- round(df_p$prop_F*100, 5)

ggplot(df_p) + 
  geom_bar(stat="identity",  aes(x=x, y=y), fill="grey") + 
  geom_bar(stat="identity", aes(x=x, y=prop_F), fill = "#FF6666") + 
  geom_vline(xintercept= (res$parity.year + min(prop$date.n)), linetype="dashed", size=1.5, color="black") +
  theme + xlab("Year") + ylab("Proportion of female-authored species, overall (%)") + ylim(c(0,50))

# get CI


##################################
# usa, germany, brazil, france, united kingdom, japan [top 6 countries] 
# as case studies; they have more than 30 taxonomists across the years
# and potentially have interesting stories to tell
##################################
countries <- c("United States of America", "Germany", "Brazil", "France", "United Kingdom", "Japan")
auth[, .N, by=Country][order(-N)][1:6]