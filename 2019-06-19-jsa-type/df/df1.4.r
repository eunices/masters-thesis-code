# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating the describer dataset with details.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

source('2019-06-19-jsa-type/df/functions.R')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - summarizing describer information
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': summarizing describer information"))



# Get synonyms idxes
synonyms <- read_escaped_data(paste0(dir_data_raw, basefile, " oth_1-clean.csv"))
synonym_idxes <- synonyms[status=="Synonym",]$idx
subsp_idxes <- synonyms[status=="Valid subspecies",]$idx
var_idxes <- synonyms[status=="Infrasubspecific",]$idx
rm(synonyms)



# Read data
describers_template <- read_escaped_data(paste0(dir_data_raw, basefile, " describers_3.0-by-author.csv"))
cols <- c("idx_auth", "full.name.of.describer.n", 
          "describer.gender.n", "dob.describer.n",
          "dod.describer.n", "alive", "origin.country.describer.n", 
          "residence.country.describer.n", "institution.of.describer.n")
describers_template <- describers_template[ ,..cols]

describers_all = read_escaped_data(paste0(dir_data_raw, basefile, " describers_4.0-denormalised2.csv"))




# Calculate metrics for non-synonyms
describers <- describers_all[!idxes %in% c(synonym_idxes, subsp_idxes, var_idxes)]
describers <- describers[date.n<=2018,]
describers[, ns_max:=max(date.n, na.rm=T), by="idx_auth"]
describers[, ns_min:=min(date.n, na.rm=T), by="idx_auth"]
describers[, ns_spp_N := length(unique(idxes)), by="idx_auth"]
describers[, ns_spp_N_1st_auth := sum(idxes_author.order=="1"), by="idx_auth"]
describers[, ns_spp_N_1st_auth_s := sum(idxes_author.order %in% c("1", "2")), by="idx_auth"]
describers[, ns_spp_N_not_1st_auth := sum(idxes_author.order!="1"), by="idx_auth"]
describers[, ns_spp_N_lst_auth := sum(idxes_author.order=="L"), by="idx_auth"]
describers[, ns_spp_idxes := paste(unique(idxes), collapse=", "), by="idx_auth"]

cols <- c("idx_auth", "full.name.of.describer.n",
          "ns_min", "ns_max", "ns_spp_N", "ns_spp_N_1st_auth", "ns_spp_N_1st_auth_s",
          "ns_spp_N_not_1st_auth", "ns_spp_N_lst_auth", "ns_spp_idxes")
describers <- data.table(unique(describers[,..cols]))




# Calculate metrics for all
describers_a <- describers_all[]
describers_a <- describers_a[date.n<=2018,]
describers_a[, max:=max(date.n, na.rm=T), by="idx_auth"]
describers_a[, min:=min(date.n, na.rm=T), by="idx_auth"]
describers_a[, spp_N := length(unique(idxes)), by="idx_auth"]
describers_a[, spp_N_1st_auth := sum(idxes_author.order=="1"), by="idx_auth"]
describers_a[, spp_N_1st_auth_s := sum(idxes_author.order %in% c("1", "2")), by="idx_auth"]
describers_a[, spp_N_not_1st_auth := sum(idxes_author.order!="1"), by="idx_auth"]
describers_a[, spp_N_lst_auth := sum(idxes_author.order=="L"), by="idx_auth"]
describers_a[, spp_idxes := paste(unique(idxes), collapse=", "), by="idx_auth"]
cols <- c("idx_auth", "full.name.of.describer.n",
          "min", "max", "spp_N", "spp_N_1st_auth", "spp_N_1st_auth_s",
          "spp_N_not_1st_auth", "spp_N_lst_auth", "spp_idxes")
describers_a <- data.table(unique(describers_a[,..cols]))




# Calculate metrics for synonyms
describers_s <- describers_all[idxes %in% synonym_idxes]
describers_s <- describers_s[date.n<=2018,]
describers_s[, syn_max:=max(date.n, na.rm=T), by="idx_auth"]
describers_s[, syn_min:=min(date.n, na.rm=T), by="idx_auth"]
describers_s[, syn_spp_N := length(unique(idxes)), by="idx_auth"]
describers_s[, syn_spp_N_1st_auth := sum(idxes_author.order=="1"), by="idx_auth"]
describers_s[, syn_spp_N_1st_auth_s := sum(idxes_author.order %in% c("1", "2")), by="idx_auth"]
describers_s[, syn_spp_N_not_1st_auth := sum(idxes_author.order!="1"), by="idx_auth"]
describers_s[, syn_spp_N_lst_auth := sum(idxes_author.order=="L"), by="idx_auth"]
describers_s[, syn_spp_idxes := paste(unique(idxes), collapse=", "), by="idx_auth"]

cols <- c("idx_auth", "full.name.of.describer.n",
          "syn_max", "syn_min", 
          "syn_spp_N", "syn_spp_N_1st_auth",
          "syn_spp_N_1st_auth_s", "syn_spp_N_not_1st_auth",
          "syn_spp_idxes")
describers_s <- data.table(unique(describers_s[, ..cols]))

describers <- merge(describers_template, describers, 
                    by=c("idx_auth", "full.name.of.describer.n"), all.x=T, all.y=T)
describers <- merge(describers, describers_a, 
                    by=c("idx_auth", "full.name.of.describer.n"), all.x=T, all.y=T)
describers <- merge(describers, describers_s, 
                    by=c("idx_auth", "full.name.of.describer.n"), all.x=T, all.y=T)

numeric_cols <- c("ns_spp_N", "ns_spp_N_1st_auth",
                  "ns_spp_N_1st_auth_s", "ns_spp_N_not_1st_auth",
                  "ns_spp_N_lst_auth",
                  
                  "spp_N", "spp_N_1st_auth", 
                  "spp_N_1st_auth_s", "spp_N_not_1st_auth",
                  "spp_N_lst_auth", 

                  "syn_spp_N", "syn_spp_N_1st_auth", 
                  "syn_spp_N_1st_auth_s", "syn_spp_N_not_1st_auth")

for (col in numeric_cols) describers[is.na(get(col)), (col) := 0]

# Making max corrected
describers$max_corrected <- describers$max
describers[alive=="Y"]$max_corrected <- "2018"

describers$ns_max_corrected <- describers$ns_max
describers[alive=="Y"]$ns_max_corrected <- "2018"




# Other metrics
describers$years_active <- as.numeric(describers$max_corrected) - as.numeric(describers$min) +1
describers$years_alive <- as.numeric(describers$dod.describer.n) - as.numeric(describers$dob.describer.n) + 1
describers$years_discrepancy <- describers$years_alive - describers$years_active

describers$ns_species_per_year_active <- round(describers$ns_spp_N / describers$years_active, 2)
describers$ns_species_per_year_alive <- round(describers$ns_spp_N / describers$years_alive, 2)

# first author defined as first, or first and second for two authors
describers$prop_species_as_1st_author_s <- round(describers$spp_N_1st_auth_s
/ describers$spp_N,2)

# proportion of synonyms
describers$prop_species_syn <- round(describers$syn_spp_N
/ describers$spp_N,2)




# Clean countries
describers.origin.cty <- data.table(describers[,c("idx_auth", "origin.country.describer.n")] %>%
    separate_rows(origin.country.describer.n), " ")[order(as.numeric(idx_auth))] # technically no need for this
describers.res.cty <- data.table(describers[,c("idx_auth", "residence.country.describer.n")] %>% 
    separate_rows(residence.country.describer.n, sep="; "))[order(as.numeric(idx_auth))]
describers.res.cty[, order:=1:.N, by="idx_auth"] # retain order

describers.origin.cty <- describers.origin.cty[origin.country.describer.n != ""]
describers.origin.cty <- merge(describers.origin.cty, 
                               lookup.cty[,c("Country", "DL")],
                               by.x="origin.country.describer.n", 
                               by.y="DL", all.x=T, all.y=F)
describers.origin.cty <-  data.table(describers.origin.cty)[order(as.numeric(idx_auth))]
describers.origin.cty[is.na(origin.country.describer.n)]$Country <- NA
dup <- describers.origin.cty[duplicated(idx_auth)]$idx_auth
describers.origin.cty[idx_auth %in% dup] # CHECK

describers.origin.cty <- describers.origin.cty[order(as.numeric(idx_auth), Country),]
describers.origin.cty <- describers.origin.cty[!duplicated(idx_auth)][,c("idx_auth", "Country")]
names(describers.origin.cty) <- c("idx_auth", "origin.country.describer.full")

describers.res.cty <- describers.res.cty[residence.country.describer.n != ""]
describers.res.cty <- merge(describers.res.cty, 
                            lookup.cty[,c("Country", "DL")],
                            by.x="residence.country.describer.n", 
                            by.y="DL", all.x=T, all.y=F)
describers.res.cty <-  data.table(describers.res.cty)
describers.res.cty[is.na(residence.country.describer.n)]$Country <- NA
describers.res.cty$idx_auth <- as.numeric(describers.res.cty$idx_auth)

describers.res.cty <- describers.res.cty[!is.na(Country)][,c("idx_auth", "Country", "order")][
    order(as.numeric(idx_auth), order)]
describers.res.cty.grp <- data.table(describers.res.cty[,c("idx_auth", "Country")] %>%
  group_by(idx_auth) %>%
  summarise(residence.country.describer.full=paste0(Country,collapse='; ')))
describers.res.cty.grp$idx_auth <- as.character(describers.res.cty.grp$idx_auth)

describers.res.cty.first <- 
    data.table(describers.res.cty[order(idx_auth, order),c("idx_auth", "Country")][!duplicated(idx_auth)])
names(describers.res.cty.first) <- c("idx_auth", "residence.country.describer.first")
describers.res.cty.first$idx_auth <- as.character(describers.res.cty.first$idx_auth)

describers_final <- merge(describers, describers.origin.cty, 
                          by='idx_auth', all.x=T, all.y=F)
describers_final <- merge(describers_final, describers.res.cty.grp, 
                          by='idx_auth', all.x=T, all.y=F)
describers_final <- merge(describers_final, describers.res.cty.first,
                          by='idx_auth', all.x=T, all.y=F)




# Other metrics: count pub/author metrics

# Read data
pub <- read_escaped_data(paste0(dir_data_raw, basefile, " pub_1.0-clean.csv"))
df1 <- read_escaped_data(paste0(dir_data_raw, basefile, " filtered_4.3-clean-coll.csv"))
df2 <- read_escaped_data(paste0(dir_data_raw, basefile, " oth_4.3-clean-coll.csv"))

# Process data
pubs <- pub %>% separate_rows(idxes)
cols = c("idx", "full.name.of.describer")
df <- rbind(df1[, ..cols], df2[, ..cols])
df$full.name.of.describer <- gsub('\\"\\"', '\\"', df$full.name.of.describer)
df <- data.table(df %>% separate_rows(full.name.of.describer, sep="; "))
df$idx <- as.integer(df$idx); pubs$idx <- as.integer(pubs$idx)
df_pub <- merge(df, pubs, by.x="idx", by.y="idx", all.x=T, all.y=T)

# Count mean/SD number of species described per publication
df_sp_per_pub <- df_pub[, .N, by=c("full.name.of.describer", "date.n", "author", "title",
                                   "journal", "volume", "issue", "page.numbers.publication")]
df_sp_per_pub$date.pub <- paste0(df_sp_per_pub$date.n, " (", df_sp_per_pub$N, ")")

author_ss <- df_sp_per_pub[, list(spp_per_pub_mean=mean(N),
                                  spp_per_pub_sd=sd(N),
                                  n_pubs=.N, 
                                  pub_years =paste0(sort(date.pub), collapse= "; ")),
                                 , by="full.name.of.describer"]
describers_final$full.name.of.describer.n <- 
    gsub('\\"\\"', '\\"', describers_final$full.name.of.describer.n)
                                                  
describers_final <- merge(describers_final, author_ss, 
                          by.x="full.name.of.describer.n",
                          by.y="full.name.of.describer", all.x=T, all.y=F)

# Count mean/SD number of species described per publication - ONLY FOR <=15 yrs before death
df_sp_per_pub2 <- 
    merge(df_sp_per_pub, describers_final[, c("full.name.of.describer.n", "dod.describer.n")],
          by.x='full.name.of.describer', by.y='full.name.of.describer.n', all.x=T, all.y=F)
df_sp_per_pub2$dod.describer.n <- as.numeric(df_sp_per_pub2$dod.describer.n)
df_sp_per_pub2$date.n <- as.numeric(df_sp_per_pub2$date.n)
df_sp_per_pub2$dod.describer.n.m <- ifelse(!is.na(df_sp_per_pub2$dod.describer.n),
                                            df_sp_per_pub2$dod.describer.n - df_sp_per_pub2$date.n , NA)
df_sp_per_pub2$pub_yr_cat <- ""
df_sp_per_pub2[dod.describer.n.m <= 15]$pub_yr_cat <- "20y"
df_sp_per_pub2[dod.describer.n.m <= 5]$pub_yr_cat <- "5y"

author_ss2 <- df_sp_per_pub2[pub_yr_cat != "", 
                             list(spp_per_pub_mean=mean(N),
                                  spp_per_pub_sd=sd(N),
                                  n_spp=sum(N),
                                  n_pubs=.N),
                                by=c("full.name.of.describer", "pub_yr_cat")]
authors_ss2 <- dcast(author_ss2, full.name.of.describer  ~ pub_yr_cat,
                     value.var=c("spp_per_pub_mean", "spp_per_pub_sd", "n_spp", "n_pubs"))

# Merge back info
describers_final <- merge(describers_final, authors_ss2, 
                          by.x="full.name.of.describer.n",
                          by.y="full.name.of.describer", all.x=T, all.y=F)




# Clean last name
# Read last name data
ln <- read_escaped_data("lookup/2019-09-16-last-name.csv")
ln <- ln[, c("full.name.of.describer.n", "last.name", "last.name.no.initials")]

describers_final <- merge(describers_final, ln, by="full.name.of.describer.n", all.x=T, all.y=F)
setcolorder(describers_final, c(2, 1, 3:length(names(describers_final))))
# *TODO: write code
# 1. get last word 
# 2. for those containing special characters, get second last word
# 3. for those with duplicated surname, abbrev. name and add in sq brackets





# Quick fixes
cockerell <- 
    strsplit(describers_final[full.name.of.describer.n=="Theodore Dru Alison Cockerell"]$spp_idxes, ", ")[[1]]
cockerell <- data.frame(cockerell_idx=cockerell)





# Write data
filename_write = paste0(dir_data_raw, basefile, " describers_5.0-describers-final-cockerell.csv")
write.csv(cockerell, filename_write, na='', row.names=F, fileEncoding="UTF-8")

filename_write = paste0(dir_data_raw, basefile, " describers_5.0-describers-final.csv")
write.csv(describers_final[order(as.numeric(idx_auth))], filename_write, 
          na='', row.names=F, fileEncoding="UTF-8")

describers_final[full.name.of.describer.n=="Theodore Dru Alison Cockerell"]$spp_idxes <- 
    paste0("Check ", basefile, " describers_5.0-describers-final-cockerell.csv")

filename_write = paste0(dir_data_raw, basefile, " describers_5.0-describers-final-view.csv")
write.csv(describers_final[order(as.numeric(idx_auth))], filename_write, 
          na='', row.names=F, fileEncoding="UTF-8")
