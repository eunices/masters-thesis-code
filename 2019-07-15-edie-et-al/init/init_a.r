# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
dir_analysis_edie_tmp <- 'tmp/edie/'
dir_script_ed <- "2019-07-15-edie-et-al/"
dir_script <- '2019-06-19-ascher-type-data/'

source(paste0(dir_script, "subset.r"))
source(paste0(dir_script_ed, "init/libraries.r"))

dir.create(file.path(dir_analysis_edie_tmp))

options(scipen = 999)
source('2019-07-15-edie-et-al/init/init.r')
source('2019-07-15-edie-et-al/init/util_a.r')
source('2019-07-15-edie-et-al/init/util_stan.r')
