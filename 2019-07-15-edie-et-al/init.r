# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Read datasets
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Initialize"))

dir_analysis_edie_tmp <- 'tmp/edie/'
dir_script_ed <- "2019-07-15-edie-et-al/"
dir_script <- '2019-06-19-ascher-type-data/'
source(paste0(dir_script, "subset.r"))

dir.create(file.path(dir_analysis_edie_tmp))
