# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Initialize
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source('2020-08-31-jsa-type-v2-ch2/00-init/init.r')

source(paste0(dir_script_ed, '00-init/util-stan.r'))
dir.create(file.path(dir_analysis_edie_model))

options(scipen = 999)

