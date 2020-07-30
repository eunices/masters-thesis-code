# Init
source("2020-07-07-deep-learning-sys-rev/init.r")

# Scopus
# https://www-scopus-com.libproxy1.nus.edu.sg/home.uri
# AND PUBYEAR = 2019

year <- 2019
files <- get_files_in_folder(data_dir_sco_raw, year)
df_sco <- read_files(files, "ASCII")

# Rename names
names(df_sco) <- gsub(" |\\.", "_", tolower(names(df_sco)))   # remove spaces and dots
names(df_sco) <- gsub("__", "_", names(df_sco))               # remove __
names(df_sco) <- sub("[^[:alpha:]]+$", "", names(df_sco))     # remove trailing non alpha chars

# File names
df_sco$identifier <- 1:dim(df_sco)[1]
df_sco$db_source <- "Scopus"
df_sco$biblio__type <-
    lp_biblio__type[match(df_sco$document_type, lp_biblio__type$scopus)]$wos
df_sco$biblio__year <- year

df_sco$biblio__authors <- df_sco$authors # follow wos
df_sco$biblio__authors  <- unlist(lapply(df_sco$biblio__authors, format_scopus_authors))

df_sco$biblio__authors[1:10]

df_sco$biblio__article_title <- df_sco$title
df_sco$biblio__journal <- df_sco$source_title

df_sco$biblio__volume <- df_sco$volume
df_sco$biblio__issue <- df_sco$issue
df_sco$biblio__page_start <- df_sco$page_start
df_sco$biblio__page_end <- df_sco$page_end
df_sco$biblio__page_e <- df_sco$art_no
df_sco$biblio__doi <- df_sco$doi
df_sco$biblio__abstract <- df_sco$abstract

df_sco$prelim__is_journal <- ifelse(df_sco$biblio__type == "J", "T", "F")
df_sco$prelim__is_abstract <- ifelse(df_sco$biblio__abstract == "" |
                                     is.na(df_sco$biblio__abstract),
                                     "F", "T")
df_sco$prelim__is_english <- "MANUAL CHECK LATER"
df_sco$prelim__is_not_duplicate <- "MANUAL CHECK LATER"

df_sco$eligibility__is_pri_research <- "MANUAL CHECK LATER"
df_sco$eligibility__is_deep_learning <- "MANUAL CHECK LATER"
df_sco$eligibility__is_complete_paper_avail <- "MANUAL CHECK LATER"

file <- paste0(data_dir_sco_cle, year, ".csv")
write.csv(df_sco[, ..lp_cols], file, na = "", fileEncoding = "UTF-8", row.names = FALSE)
