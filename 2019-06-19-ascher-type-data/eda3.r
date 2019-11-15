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

dir_data_subf <- paste0(dir_data, "eda3_gender/"); if(!dir.exists(dir_data_subf)) dir.create(dir_data_subf)
dir_data_subf1 <- paste0(dir_data_subf, "time-series-pub/"); if(!dir.exists(dir_data_subf1)) dir.create(dir_data_subf1)
dir_data_subf2 <- paste0(dir_data_subf, "time-series-tax/"); if(!dir.exists(dir_data_subf2)) dir.create(dir_data_subf2)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep - papers
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender rep - papers"))

# adapted from https://github.com/lukeholman/genderGapCode/

#################
# By Position
#################

result_summary_all <- lapply(c("All", "First", "Last", "First_s", "Last_s"), function(pos) {
    run_specific_scenario(country="All", position=pos, dir_data_subf1)
})

#################
# By Country
#################

# countries <- c("United States of America", "Germany", "Brazil", "France", "United Kingdom", "Japan")
countries <- c(auth[, .N, by=Country][order(-N)][1:6]$Country)
result_summary_countries <- lapply(countries, function(country) {
    run_specific_scenario(country=country, position="All", dir_data_subf1)
})
# usa, germany, brazil, france, united kingdom, japan [top 6 countries]
# as case studies; they have more than 30 taxonomists across the years
# and potentially have interesting stories to tell

outputs <- rbindlist(c(result_summary_all, result_summary_countries))
write.csv(outputs, paste0(dir_data_subf1, "_outputs.csv"), row.names=F)

# Testing
# print(result_summary[[1]])
# rbindlist(result_summary)

# generate_prop_t("Germany", "All")
# generate_prop_t("France", "All")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep - taxonomists
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender rep - taxonomists"))

result_summary_tax <- run_specific_scenario(country="All", position="All", dir_data_subf2, "tax")
write.csv(result_summary_tax, paste0(dir_data_subf2, "_outputs.csv"), row.names=F)

result_summary_countries_tax <- lapply(countries, function(country) {
    run_specific_scenario(country=country, position="All", dir_data_subf2, "tax")
})

outputs <- rbindlist(c(list(result_summary_tax), result_summary_countries_tax))
write.csv(outputs, paste0(dir_data_subf2, "_outputs.csv"), row.names=F)

# generate_prop_t_tax("Germany")
