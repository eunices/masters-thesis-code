# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
dir_analysis_edie_model <- 'model/edie/'
dir_script_ed <- "2019-06-19-jsa-type-ch2/"
dir_script <- '2019-06-19-jsa-type/'

source(paste0(dir_script, "subset.r"))
source(paste0(dir_script_ed, "init/libraries.r"))

dir.create(file.path(dir_analysis_edie_model))

options(scipen = 999)
source('2019-06-19-jsa-type-ch2/init/util_a.r')
source('2019-06-19-jsa-type-ch2/init/util_stan.r')
