
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - summarizing describer information
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': summarizing describer information"))

synonyms <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_2-clean.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
synonym_idxes <- synonyms[status=="Synonym",]$idx
subsp_idxes <- synonyms[status=="Valid subspecies",]$idx
var_idxes <- synonyms[status=="Infrasubspecific",]$idx
rm(synonyms)

describers_template <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_3.0-by-author.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')[
    ,c("idx_auth", "full.name.of.describer.n", "describer.gender.n", "dob.describer.n", "dod.describer.n", "alive",
       "origin.country.describer.n", "residence.country.describer.n", "institution.of.describer.n")]
describers_template[, names(describers_template) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes


describers_template_edits <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final_edit.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describers_template_edits[, names(describers_template_edits) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes
describers_template_edits <- describers_template_edits[rowSums(is.na(describers_template_edits)) != ncol(describers_template_edits)-1, ] # remove rows with all NAs
describers_template_edits$alive_certainty <- NULL

merge_column <- function(df, df_change, by) {
    # get column name to modify
    col_change <- names(df_change)[names(df_change) != by]
    names(df_change)[names(df_change) == col_change] <- "modify"
    print(paste0("Column to be changed: ", col_change))

    # modify column 
    df <- merge(df, df_change, by=by, all.x=T, all.y=F)
    df[!is.na(modify), col_change] <- df[!is.na(modify),]$modify
    df$modify <- NULL
    df
}

for (i in 2:length(names(describers_template_edits))) {
    col_name <- names(describers_template_edits)[i]
    cols <- c("full.name.of.describer.n", col_name)
    describers_template <- merge_column(describers_template, describers_template_edits[,..cols], "full.name.of.describer.n")
}

names(describers_template_edits)


describers_all <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describers_all[, names(describers_all) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes


# For non-synonyms
describers <- describers_all[!idxes %in% c(synonym_idxes, subsp_idxes, var_idxes)]
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

# All
describers_a <- describers_all[]
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

# Row by row discrepancies
describers_a[full.name.of.describer.n=="Ismael Alejandro Hinojosa-Díaz", "min"] = "2003"
describers_a[full.name.of.describer.n=="Johan Christian Fabricius", "max"] = "1804"
describers_a[full.name.of.describer.n=="Michael Kuhlmann", "min"] = "1998"
describers_a[full.name.of.describer.n=="Eduardo Andrade Botelho de Almeida", "min"] = "2008" # should be modified in original file
describers_a[full.name.of.describer.n=="Michael Scott Engel", "min"] = "1995" # should be modified in original file
describers_a[full.name.of.describer.n=="[Carl Eduard] Adolph Gerstaecker", "full.name.of.describer.n"] == "[Carl Eduard] Adolph Gerstäcker"
describers_a[full.name.of.describer.n=="Francisco Javier Ortiz-Sanchez", "full.name.of.describer.n"] == "Francisco Javier Ortiz-Sánche"



# Synonyms
describers_s <- describers_all[idxes %in% synonym_idxes]
describers_s[, syn_spp_N := length(unique(idxes)), by="idx_auth"]
describers_s[, syn_spp_N_1st_auth := sum(idxes_author.order=="1"), by="idx_auth"]
describers_s[, syn_spp_N_1st_auth_s := sum(idxes_author.order %in% c("1", "2")), by="idx_auth"]
describers_s[, syn_spp_N_not_1st_auth := sum(idxes_author.order!="1"), by="idx_auth"]
describers_s[, syn_spp_N_lst_auth := sum(idxes_author.order=="L"), by="idx_auth"]
describers_s[, syn_spp_idxes := paste(unique(idxes), collapse=", "), by="idx_auth"]

cols <- c("idx_auth", "full.name.of.describer.n",
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

# Metrics
describers$years_active <- as.numeric(describers$max) - as.numeric(describers$min) +1
describers$years_alive <- as.numeric(describers$dod.describer.n) - as.numeric(describers$dob.describer.n) + 1
describers$years_active_likely <- ifelse(is.na(describers$years_alive), describers$years_active, describers$years_alive - 25) # use min publication date or after PhD
describers$max_corrected <- ifelse(describers$alive =='Y', 2019, describers$max) # to account for whether alive or not 
describers$years_discrepancy <- describers$years_alive - describers$years_active

describers$ns_species_per_year_active <- round(describers$ns_spp_N / describers$years_active, 2)
describers$ns_species_per_year_alive <- round(describers$ns_spp_N / describers$years_alive, 2)

# first author defined as first, or first and second for two authors
describers$prop_species_as_1st_author_s <- round(describers$spp_N_1st_auth_s
/ describers$spp_N,2)

# proportion of synonyms
describers$prop_species_syn <- round(describers$syn_spp_N
/ describers$spp_N,2)

describers[, origin.country.describer.n := gsub('UK', 'GB', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('EZ', 'CZ', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('SW', 'SE', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('UP', 'UA', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('CS', 'CR', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('PO', 'PT', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('DA', 'DK', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('JA', 'JP', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('TU', 'TR', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('KS', 'KR', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('SF', 'ZA', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('SP', 'ES', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('EN', 'EE', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('EI', 'IE', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('LO', 'SK', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('RP', 'PH', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('DA', 'DK', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('AU', 'AT', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('AS', 'AU', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('GM', 'DE', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('PM', 'PA', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('CH', 'CN', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('SZ', 'CH', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('CI', 'CL', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('IS', 'IL', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('RS', 'RU', origin.country.describer.n)]
describers[, origin.country.describer.n := gsub('SG', 'SN', origin.country.describer.n)]


describers[, residence.country.describer.n := gsub('UK', 'GB', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('EZ', 'CZ', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('SW', 'SE', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('UP', 'UA', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('CS', 'CR', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('PO', 'PT', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('DA', 'DK', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('JA', 'JP', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('TU', 'TR', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('KS', 'KR', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('SF', 'ZA', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('SP', 'ES', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('EN', 'EE', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('EI', 'IE', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('LO', 'SK', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('RP', 'PH', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('DA', 'DK', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('AU', 'AT', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('AS', 'AU', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('GM', 'DE', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('PM', 'PA', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('CH', 'CN', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('SZ', 'CH', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('CI', 'CL', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('IS', 'IL', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('RS', 'RU', residence.country.describer.n)]
describers[, residence.country.describer.n := gsub('SG', 'SN', residence.country.describer.n)]


describers.origin.cty <- data.table(describers[,c("idx_auth", "origin.country.describer.n")] %>% separate_rows(origin.country.describer.n), " ")[order(as.numeric(idx_auth))] # technically no need for this
describers.res.cty <- data.table(describers[,c("idx_auth", "residence.country.describer.n")] %>% separate_rows(residence.country.describer.n, sep="; "))[order(as.numeric(idx_auth))]
describers.res.cty[, order:=1:.N, by="idx_auth"] # retain order


lookup <- data.table(lookup.cty)
describers.origin.cty <- describers.origin.cty[origin.country.describer.n != ""]
describers.origin.cty <- merge(describers.origin.cty, 
                               lookup[,c("Country", "A.2")],
                               by.x="origin.country.describer.n", 
                               by.y="A.2", all.x=T, all.y=F)
describers.origin.cty <-  data.table(describers.origin.cty)[order(as.numeric(idx_auth))]
describers.origin.cty[is.na(origin.country.describer.n)]$Country <- NA
dup <- describers.origin.cty[duplicated(idx_auth)]$idx_auth
describers.origin.cty[idx_auth %in% dup] # CHECK

describers.origin.cty <- describers.origin.cty[order(as.numeric(idx_auth), Country),]
describers.origin.cty <- describers.origin.cty[!duplicated(idx_auth)][,c("idx_auth", "Country")]
names(describers.origin.cty) <- c("idx_auth", "origin.country.describer.full")


describers.res.cty <- describers.res.cty[residence.country.describer.n != ""]
describers.res.cty <- merge(describers.res.cty, 
                            lookup[,c("Country", "A.2")],
                            by.x="residence.country.describer.n", 
                            by.y="A.2", all.x=T, all.y=F)
describers.res.cty <-  data.table(describers.res.cty)
describers.res.cty[is.na(residence.country.describer.n)]$Country <- NA
describers.res.cty$idx_auth <- as.numeric(describers.res.cty$idx_auth)

describers.res.cty <- describers.res.cty[!is.na(Country)][,c("idx_auth", "Country", "order")][
    order(as.numeric(idx_auth), order)]


describers.res.cty.grp <- data.table(describers.res.cty[,c("idx_auth", "Country")] %>%
  group_by(idx_auth) %>%
  summarise(residence.country.describer.full=paste0(Country,collapse='; ')))
describers.res.cty.grp$idx_auth <- as.character(describers.res.cty.grp$idx_auth)

describers.res.cty.first <- data.table(describers.res.cty[order(idx_auth, order),c("idx_auth", "Country")][!duplicated(idx_auth)])
names(describers.res.cty.first) <- c("idx_auth", "residence.country.describer.first")
describers.res.cty.first$idx_auth <- as.character(describers.res.cty.first$idx_auth)

rm(lookup)

describers_final <- merge(describers, describers.origin.cty, by='idx_auth', all.x=T, all.y=F)
describers_final <- merge(describers_final, describers.res.cty.grp, by='idx_auth', all.x=T, all.y=F)
describers_final <- merge(describers_final, describers.res.cty.first, by='idx_auth', all.x=T, all.y=F)

# Get last name
# describers_final$last.name <- sapply(
#     strsplit(as.character(describers_final$full.name.of.describer.n), " "), tail, 1)

ln <- fread(paste0(dir, "clean/last_name.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
ln[, names(ln) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes
ln <- ln[, c("full.name.of.describer.n", "last.name", "last.name.no.initials")]
# ln$last.name.no.initials <- gsub(" ", "", gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", ln$last.name)) 

# write.csv(ln,
#           paste0(dir, "clean/last_name2.csv"), na='', row.names=F, fileEncoding="UTF-8")

describers_final <- merge(describers_final, ln, by="full.name.of.describer.n", all.x=T, all.y=F)
setcolorder(describers_final, c(2, 1, 3:length(names(describers_final))))
# *TODO: write code
# 1. get last word 
# 2. for those containing special characters, get second last word
# 3. for those with duplicated surname, abbrev. name and add in sq brackets

# Quick fixes
cockerell <- strsplit(describers_final[full.name.of.describer.n=="Theodore Dru Alison Cockerell"]$spp_idxes, ", ")[[1]]
cockerell <- data.frame(cockerell_idx=cockerell)
write.csv(cockerell,
          paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final-cockerell.csv"), na='', row.names=F, fileEncoding="UTF-8")

write.csv(describers_final[order(as.numeric(idx_auth))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv"), na='', row.names=F, fileEncoding="UTF-8")

describers_final[full.name.of.describer.n=="Theodore Dru Alison Cockerell"]$spp_idxes <- "Check 2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final-cockerell.csv"

write.csv(describers_final[order(as.numeric(idx_auth))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final-view.csv"), na='', row.names=F, fileEncoding="UTF-8")
