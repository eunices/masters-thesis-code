# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - creating dataset for network
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': creating dataset for network"))

# Create pairs

describers_info <- fread(
    paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_1.0-all_edit.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

# https://stackoverflow.com/questions/30702191/

# count pairs
