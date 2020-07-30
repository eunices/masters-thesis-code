# Init
source("2020-07-07-deep-learning-sys-rev/init.r")

# Web of science
# http://apps.webofknowledge.com.libproxy1.nus.edu.sg/WOS_GeneralSearch_input.do?product=WOS&search_mode=GeneralSearch

# paste search string
# set year to 2019
# export by other file formats, tab limited windows, author/title/source/abstract
# filter by PT = "J"

year <- 2019
files <- get_files_in_folder(data_dir_wos_raw, year)
df_wos <- read_files(files, "UTF-8")

# Names
nm <- names(df_wos)[2:length(names(df_wos))]

# Remove last blank column
df_wos[, 68] <- NULL

# Rename names
names(df_wos) <- nm

df_wos$identifier <- 1:dim(df_wos)[1]
df_wos$db_source <- "Web of Science"

df_wos$biblio__type <- df_wos$PT
df_wos$biblio__year <- year # or use PY / EA (if PY is blank)
df_wos$biblio__authors <- df_wos$AU # or AF
df_wos$biblio__article_title <- df_wos$TI
df_wos$biblio__journal <- df_wos$SO
df_wos$biblio__volume <- df_wos$VL
df_wos$biblio__issue <- df_wos$IS
df_wos$biblio__page_start <- df_wos$BP
df_wos$biblio__page_end <- df_wos$EP
df_wos$biblio__page_e <- df_wos$AR
df_wos$biblio__doi <- df_wos$DI
df_wos$biblio__abstract <- df_wos$AB

df_wos$prelim__is_journal <- ifelse(df_wos$biblio__type == "J", "T", "F")
df_wos$prelim__is_not_duplicate <- "MANUAL CHECK LATER"
df_wos$prelim__is_abstract <- ifelse(df_wos$biblio__abstract == "" |
                                     is.na(df_wos$biblio__abstract),
                                     "F", "T")

df_wos$eligibility__is_pri_research <- "MANUAL CHECK LATER"
df_wos$eligibility__is_english <- 
    ifelse(grepl("[\\p{Han}]", df_wos$biblio__article_title, perl = TRUE), "F", ifelse(
        grepl("\\[", df_wos$biblio__article_title), "F (check)", "T"
    ))
df_wos$eligibility__is_not_software <- "MANUAL CHECK LATER"
df_wos$eligibility__is_deep_learning <- "MANUAL CHECK LATER"
df_wos$eligibility__is_complete_paper_avail <- "MANUAL CHECK LATER"

file <- paste0(data_dir_wos_cle, year, ".csv")
write.csv(df_wos[, ..lp_cols], file, na = "", fileEncoding = "UTF-8", row.names = FALSE)
