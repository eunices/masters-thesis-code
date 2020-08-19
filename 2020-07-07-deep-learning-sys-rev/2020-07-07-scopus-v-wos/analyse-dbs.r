# Init
source("2020-07-07-deep-learning-sys-rev/init.r")

year <- 2019
df_sco <- fread(paste0(data_dir_sco_cle, year, ".csv"), encoding="UTF-8")
df_wos <- fread(paste0(data_dir_wos_cle, year, ".csv"), encoding="UTF-8")

df <- rbind(df_sco, df_wos)

df <- df[order(tolower(biblio__journal),
               tolower(biblio__article_title),
               tolower(biblio__authors))]

cols <- c("biblio__journal", "biblio__article_title", "biblio__authors")
check <- data.table(data.frame(lapply(df[, ..cols], toupper)))
df$duplicated <- duplicated(check)

table(df$db_source, df$biblio__type)

write.csv(df, paste0(data_dir_rev, "2020-07-29-compare-sco-wos-2019.csv"),
          row.names = F, fileEncoding = "UTF-8")



# eligibility__is_pri_research: framework / systematic review 
