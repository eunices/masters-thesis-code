
dir_analysis_edie_model <- '2020-08-31-jsa-type-v2-ch2/model/'
dir_script_ed <- "2020-08-31-jsa-type-v2-ch2/"
dir_script <- '2020-08-31-jsa-type-v2/'

dir.create(file.path(dir_analysis_edie_model))

cutoff_ch2 <- 2019

options(scipen = 999)

source(paste0(dir_script_ed, "00-init/libraries.r"))
source(paste0(dir_script_ed, '00-init/util-a.r'))