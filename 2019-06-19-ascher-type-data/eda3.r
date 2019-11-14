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


output <- main(country = "All", position = "All")
output2 <- main(country = "All", position = "First")

##################################
# usa, germany, brazil, france, united kingdom, japan [top 6 countries]
# as case studies; they have more than 30 taxonomists across the years
# and potentially have interesting stories to tell
##################################

# countries <- c("United States of America", "Germany", "Brazil", "France", "United Kingdom", "Japan")
countries <- c(auth[, .N, by=Country][order(-N)][1:6]$Country)

for (i in 1:length(countries)) {
    country <- countries[i]
    output <- main(country = country, position="All")
}