library(data.table)
library(tidyr)
library(gridExtra)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Read datasets"))

dir_analysis_edie_tmp <- 'tmp/edie/'
dir_script <- '2019-06-19-ascher-type-data/'
source(paste0(dir_script, "subset.r"))

