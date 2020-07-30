source("2020-07-07-deep-learning-sys-rev/util.r")
source("2020-07-07-deep-learning-sys-rev/libraries.r")

# Params
pub_dir <- "C:/Users/ejysoh/Dropbox/msc-thesis/research/"
pub_dir_table <- paste0(pub_dir, "_tables/_ch4/")

data_dir <- "data/2020-07-07-deep-learning-sys-rev/"
data_dir_wos_raw <- paste0(data_dir, "review-v1/wos-raw/")
data_dir_sco_raw <- paste0(data_dir, "review-v1/scopus-raw/")
data_dir_wos_cle <- paste0(data_dir, "review-v1/wos-clean/")
data_dir_sco_cle <- paste0(data_dir, "review-v1/scopus-clean/")
data_dir_lp <- paste0(data_dir, "lookup/")

# Lookup files
lp_file_cols <- paste0(pub_dir_table, "2020-07-08-dl-studies.xlsx")
lp_cols <- names(read.xlsx2(lp_file_cols, sheetName = "all"))

lp_biblio__type <- fread(paste0(data_dir_lp, "2020-07-29-lp-biblio__type.csv"))