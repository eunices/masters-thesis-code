source('2019-06-19-ascher-type-data/init.r')

# Libraries
library(dplyr)
library(tidyr)

# Parameters
# loop <- 'Y'
loop <- 'N'

# Describers dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section -  individual author species rows 
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': individual author species rows"))

describers_info <- fread(
    paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_1.0-all_edit.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

describers_info[, names(describers_info) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape 

# A loop was written because currently this doesn't really work!~
# describers <- describers_info %>% separate_rows(full.name.of.describer, 
#                                                 describer.gender, dob.describer,
#                                                 dod.describer, origin.country.describer,
#                                                 residence.country.describer, institution.of.describer, 
#                                                 sep=";|,", convert=T)


# # =================
# # DONE ONCE ONLY ##
# # =================

# Split authors by ;
split_semicolon <- function(x) {
    strsplit(x, "; ")
}
split_gender <- function(x) {
    strsplit(x, "")
}

run_loop <- function() {
    describers_info$full.name.of.describer.n <- lapply(describers_info$full.name.of.describer, split_semicolon)
    describers_info$describer.gender.n <- lapply(describers_info$describer.gender,
                                                split_gender)
    describers_info$dob.describer.n <- lapply(describers_info$dob.describer,
                                            split_semicolon)
    describers_info$dod.describer.n <- lapply(describers_info$dod.describer,
                                            split_semicolon)
    describers_info$origin.country.describer.n <- lapply(describers_info$origin.country.describer,
                                                        split_semicolon)
    describers_info$residence.country.describer.n <- lapply(describers_info$residence.country.describer,
                                                        split_semicolon)
    describers_info$institution.of.describer.n <- lapply(describers_info$institution.of.describer,
                                                        split_semicolon)

    # Create a new row for each author
    describers <- data.frame(idx=character(), full.name.of.describer.n=character(),
                            describer.gender.n=character(), dob.describer.n=character(),
                            dod.describer.n=character(), origin.country.describer.n=character(),
                            residence.country.describer.n=character(), institution.of.describer.n=character(), author.order=integer())
    for (i in 1:dim(describers_info)[1]) {
    # for (i in 1:2) {
        idx_row <- describers_info[i]$idx
        describer_row <- describers_info[i]$full.name.of.describer.n[[1]][[1]]
        gender_row <- describers_info[i]$describer.gender.n[[1]][[1]]
        dob_row <- describers_info[i]$dob.describer.n[[1]][[1]]
        dod_row <- describers_info[i]$dod.describer.n[[1]][[1]]
        origin_row <- describers_info[i]$origin.country.describer.n[[1]][[1]]
        residence_row <- describers_info[i]$residence.country.describer.n[[1]][[1]]
        inst_row <- describers_info[i]$institution.of.describer.n[[1]][[1]]

        if(!identical(describer_row, character(0))){
            for (j in 1:length(describer_row)) {
                if(is.na(describer_row[j])) {
                    to_merge <- data.frame(idx=idx_row, full.name.of.describer.n=NA,
                            describer.gender.n=NA, dob.describer.n=NA,
                            dod.describer.n=NA, origin.country.describer.n=NA,
                            residence.country.describer.n=NA, institution.of.describer.n=NA,author.order=NA)
                } else {

                    gender <- ifelse(is.na(gender_row[j]) || identical(gender_row[j], logical(0)) , NA, gender_row[j])
                    dob <- ifelse(is.na(dob_row[j]) || identical(dob_row[j], logical(0)), NA, dob_row[j])
                    dod <- ifelse(is.na(dod_row[j]) || identical(dod_row[j], logical(0)), NA, dod_row[j])
                    origin <- ifelse(is.na(origin_row[j]) || identical(origin_row[j], logical(0)), NA, origin_row[j])
                    residence <- ifelse(is.na(residence_row[j]) || identical(residence_row[j], logical(0)), 
                                        NA, residence_row[j])
                    inst <- ifelse(is.na(inst_row[j]) || identical(inst_row[j], logical(0)), NA, inst_row[j])
                    order <- ifelse(j==1, 1, 
                        ifelse(j==length(describer_row) & length(describer_row) != 2, "L", 
                            ifelse(j==length(describer_row) & length(describer_row) == 2, "S", j)))

                    to_merge <- data.frame(idx=idx_row,
                                        full.name.of.describer.n=describer_row[j],
                                        describer.gender.n=gender,
                                        dob.describer.n=dob,
                                        dod.describer.n=dod,
                                        origin.country.describer.n=origin,
                                        residence.country.describer.n=residence,
                                        institution.of.describer.n=inst,
                                        author.order=order)
                    describers <- rbind(describers, to_merge)
                }
            }
        } else {
            to_merge <- data.frame(idx=idx_row, full.name.of.describer.n=NA,
                            describer.gender.n=NA, dob.describer.n=NA,
                            dod.describer.n=NA, origin.country.describer.n=NA,
                            residence.country.describer.n=NA, institution.of.describer.n=NA,
                            author.order=NA)
            describers <- rbind(describers, to_merge)
        }
        print(paste0("Row ", i , " completed for ", idx_row))
    }

    write.csv(describers, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_2.0-denormalised.csv"), na='', row.names=F, fileEncoding="UTF-8")
}

if (loop=='Y') {
    run_loop()
}

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - summarize by species idx for checks of authors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': summarize by species idx for checks on authors"))

describers <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_2.0-denormalised.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')

describers[, names(describers) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes


# Merge back the other columns
describers_merged <- data.table(describers)
cols <- c("idx", "author", "full.name.of.describer", "describer.gender", 
          "dob.describer", "dod.describer",
          "origin.country.describer", "residence.country.describer", "institution.of.describer")
describers_merged[] <- lapply(describers_merged, as.character)
describers_info[] <- lapply(describers_info, as.character)
describers_merged <- merge(describers_merged, describers_info[,..cols], by='idx', all.x=T, all.y=F)

# Summarize by idx
describers_idx <- describers_merged[, idxes:=paste0(idx, collapse=', '), by=c('full.name.of.describer.n')]
describers_idx <- describers_idx[, idxes_author.order:=paste0(author.order, collapse=', '), 
                                 by=c("full.name.of.describer.n")]
describers_idx <- describers_idx[,c("full.name.of.describer.n", 
                                    "describer.gender.n",
                                    "dob.describer.n",
                                    "dod.describer.n",
                                    "origin.country.describer.n",
                                    "residence.country.describer.n",
                                    "institution.of.describer.n", 
                                    "idxes", "idxes_author.order")]
describers_idx <- describers_idx[order(full.name.of.describer.n,
                                       describer.gender.n,
                                       -dob.describer.n, 
                                       -dod.describer.n,
                                       -origin.country.describer.n,
                                       -residence.country.describer.n,
                                       -institution.of.describer.n),]
describers_idx <- describers_idx[!duplicated(full.name.of.describer.n)]
describers_idx[] <- lapply(describers_idx, as.character)
describers_idx <- data.table(describers_idx)
describers_idx <- describers_idx[order(full.name.of.describer.n),]

# Prioritize based on gender
describers_idx$describer.gender.n <- factor(describers_idx$describer.gender.n, levels=c("F", "M", "U"), ordered=T)
describers_idx <- describers_idx[order(full.name.of.describer.n, describer.gender.n)]
dim(describers_idx)
describers_idx <- describers_idx[!duplicated(full.name.of.describer.n)]
dim(describers_idx)
describers_idx$describer.gender.n <- as.character(describers_idx$describer.gender.n)

describers$dob.describer.original <- describers$dob.describer.n 
describers$dod.describer.original <- describers$dod.describer.n 

describers$alive <- "N"
describers[grepl(describers$dod.describer.original, "\\[alive in 2019\\]")]$alive <- "Y"

describers$dob.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                   describers$origin.country.describer.n)
describers$dod.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                   describers$origin.country.describer.n)
describers_idx$dob.describer.n <- gsub(";| ", "", describers_idx$dob.describer.n)
describers_idx$dod.describer.n <- gsub(";| ", "", describers_idx$dod.describer.n)

describers_idx[full.name.of.describer.n=="Haroldo Toro [Guttierez]"]$dob.describer.n <- ""
describers_idx[full.name.of.describer.n=="Suzanne Willington Tubby Batra"]$dob.describer.n <- "1937"
dob = describers[full.name.of.describer.n=="Wilhelm Albert Schulz", "dod.describer.n"]
dod = describers[full.name.of.describer.n=="Wilhelm Albert Schulz", "dob.describer.n"]
describers[full.name.of.describer.n=="Wilhelm Albert Schulz", "dod.describer.n"] = dod
describers[full.name.of.describer.n=="Wilhelm Albert Schulz", "dob.describer.n"] = dob

describers[full.name.of.describer.n=="Moses Harris", "dod.describer.n"] = 1788

describers_idx$idx_auth <- 1:dim(describers_idx)[1]

write.csv(describers_idx, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_3.0-by-author.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - further denormalization of data with cleaned describer data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': further denormalization of data with cleaned describer data"))

describers <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_3.0-by-author.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describers[, names(describers) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# Denormalization
describers <- describers %>% separate_rows(idxes, idxes_author.order)

# Joining with dates
dfx1 <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.1-useful-col.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2 <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_3-useful-col.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx <- rbind(dfx1[,c("idx", "date.n")], dfx2[,c("idx", "date.n")])

dfx$idx <- as.numeric(dfx$idx)
describers$idxes <- as.numeric(describers$idxes)
describers <- merge(describers, dfx, by.x="idxes", by.y="idx", all.x=T, all.y=F)

write.csv(describers, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - summarizing describer information
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': summarizing describer information"))

synonyms <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_3-useful-col.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
synonym_idxes <- synonyms[status=="Synonym",]$idx
subsp_idxes <- synonyms[status=="Valid subspecies",]$idx
var_idxes <- synonyms[status=="Infrasubspecific",]$idx
rm(synonyms)

describers_template <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_3.0-by-author.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')[, c(1:7, 10)]
describers_template[, names(describers_template) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

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
describers_a[full.name.of.describer.n=="Ismael Alejandro Hinojosa-Díaz", "min"] = 2003
describers_a[full.name.of.describer.n=="Johan Christian Fabricius", "max"] = 1804  
describers_a[full.name.of.describer.n=="Michael Kuhlmann", "min"] = 1998  
describers_a[full.name.of.describer.n=="Eduardo Andrade Botelho de Almeida", "min"] = 2008 # should be modified in original file
describers_a[full.name.of.describer.n=="Michael Scott Engel", "min"] = 1995 # should be modified in original file

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
describers$years_discrepancy <- describers$years_alive - describers$years_active

describers$ns_species_per_year_active <- round(describers$ns_spp_N / describers$years_active, 2)
describers$ns_species_per_year_alive <- round(describers$ns_spp_N / describers$years_alive, 2)

# first author defined as first, or first and second for two authors
describers$prop_species_as_1st_author_s <- round(describers$spp_N_1st_auth_s
/ describers$spp_N,2)

# proportion of synonyms
describers$prop_species_syn <- round(describers$syn_spp_N
/ describers$spp_N,2)

# clean countries
describers$origin.country.describer.n2 <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                               describers$origin.country.describer.n)
describers$residence.country.describer.n2 <-  gsub(" ", ", ", gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                                  describers$residence.country.describer.n)) 


describers[, origin.country.describer.n2 := gsub('UK', 'GB', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('EZ', 'CZ', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('SW', 'SE', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('UP', 'UA', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('CS', 'CR', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('PO', 'PT', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('DA', 'DK', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('JA', 'JP', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('TU', 'TR', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('KS', 'KR', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('SF', 'ZA', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('SP', 'ES', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('EN', 'EE', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('EI', 'IE', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('LO', 'SK', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('RP', 'PH', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('DA', 'DK', origin.country.describer.n2)]
describers[, origin.country.describer.n2 := gsub('AS', 'AU', origin.country.describer.n2)]


describers[, residence.country.describer.n2 := gsub('UK', 'GB', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('EZ', 'CZ', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('SW', 'SE', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('UP', 'UA', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('CS', 'CR', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('PO', 'PT', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('DA', 'DK', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('JA', 'JP', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('TU', 'TR', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('KS', 'KR', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('SF', 'ZA', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('SP', 'ES', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('EN', 'EE', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('EI', 'IE', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('LO', 'SK', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('RP', 'PH', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('DA', 'DK', residence.country.describer.n2)]
describers[, residence.country.describer.n2 := gsub('AS', 'AU', residence.country.describer.n2)]


describers.origin.cty <- data.table(describers[,c("idx_auth", "origin.country.describer.n2")] %>% separate_rows(origin.country.describer.n2), " ")[order(as.numeric(idx_auth))]
describers.res.cty <- data.table(describers[,c("idx_auth", "residence.country.describer.n2")] %>% separate_rows(residence.country.describer.n2), " ")[order(as.numeric(idx_auth))]

lookup <- data.table(lookup.cty)
describers.origin.cty <- describers.origin.cty[origin.country.describer.n2 != ""]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('UK', 'GB', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('EZ', 'CZ', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('SW', 'SE', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('UP', 'UA', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('CS', 'CR', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('PO', 'PT', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('DA', 'DK', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('JA', 'JP', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('TU', 'TR', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('KS', 'KR', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('SF', 'ZA', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('SP', 'ES', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('EN', 'EE', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('EI', 'IE', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('LO', 'SK', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('RP', 'PH', x))]
# describers.origin.cty[, names(describers.origin.cty) := lapply(.SD, function(x) gsub('DA', 'DK', x))]
describers.origin.cty <- merge(describers.origin.cty, 
                               lookup[,c("Country", "A.2")],
                               by.x="origin.country.describer.n2", 
                               by.y="A.2", all.x=T, all.y=F)
describers.origin.cty <-  data.table(describers.origin.cty)[order(as.numeric(idx_auth))]
describers.origin.cty[is.na(origin.country.describer.n2)]$Country <- NA
dup <- describers.origin.cty[duplicated(idx_auth)]$idx_auth
describers.origin.cty[idx_auth %in% dup] # CHECK

describers.origin.cty <- describers.origin.cty[order(as.numeric(idx_auth), Country),]
describers.origin.cty <- describers.origin.cty[!duplicated(idx_auth)][,c("idx_auth", "Country")]
names(describers.origin.cty) <- c("idx_auth", "origin.country.describer.full")


describers.res.cty <- describers.res.cty[residence.country.describer.n2 != ""]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('UK', 'GB', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('EZ', 'CZ', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('SW', 'SE', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('UP', 'UA', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('CS', 'CR', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('PO', 'PT', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('DA', 'DK', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('JA', 'JP', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('TU', 'TR', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('KS', 'KR', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('SF', 'ZA', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('SP', 'ES', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('EN', 'EE', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('EI', 'IE', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('LO', 'SK', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('RP', 'PH', x))]
# describers.res.cty[, names(describers.res.cty) := lapply(.SD, function(x) gsub('DA', 'DK', x))]
describers.res.cty <- merge(describers.res.cty, 
                            lookup[,c("Country", "A.2")],
                            by.x="residence.country.describer.n2", 
                            by.y="A.2", all.x=T, all.y=F)
describers.res.cty <-  data.table(describers.res.cty)[order(as.numeric(idx_auth))]
describers.res.cty[is.na(residence.country.describer.n2)]$Country <- NA
describers.res.cty$idx_auth <- as.numeric(describers.res.cty$idx_auth)

describers.res.cty <- describers.res.cty[!is.na(Country)][,c("idx_auth", "Country")]

describers.res.cty.grp <- data.table(describers.res.cty %>%
  group_by(idx_auth) %>%
  summarise(residence.country.describer.full=paste0(Country,collapse='; ')))
describers.res.cty.grp$idx_auth <- as.character(describers.res.cty.grp$idx_auth)

rm(lookup)

describers <- merge(describers, describers.origin.cty, by='idx_auth', all.x=T, all.y=F)
describers <- merge(describers, describers.res.cty.grp, by='idx_auth', all.x=T, all.y=F)


write.csv(describers[order(as.numeric(idx_auth))], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv"), na='', row.names=F, fileEncoding="UTF-8")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - no of taxonomist active per year
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': number of taxonomist active per year"))

describer_date <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describer_date[, names(describer_date) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

describer_data <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
describer_data[, names(describer_data) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

synonyms <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_3-useful-col.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
synonyms[, names(synonyms) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes


# N number of describers
describers <- describer_data[,c("idx_auth", "full.name.of.describer.n", "min", "max")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max)
describers$years <- seq
describers <- describers %>% unnest(years)
describers[,N_describers := length(idx_auth), by=years]
describers_active_by_year <- unique(describers[,c("years", "N_describers")])[order(as.numeric(years))]

# Weighted 
describers <- describer_data[,c("idx_auth", "full.name.of.describer.n",
                                "min", "max", "ns_species_per_year_active")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max)
describers$years <- seq
describers <- describers %>% unnest(years)
describers[,N_weighted_describers := sum(as.numeric(ns_species_per_year_active)), by=years]
describers_weighted_by_year <- unique(describers[,c("years", "N_weighted_describers")])[order(as.numeric(years))]

taxonomic_effort1 <- merge(describers_active_by_year, describers_weighted_by_year, by="years", all.x=T, all.y=T)
min_year <- min(taxonomic_effort1$years)
max_year <- max(taxonomic_effort1$years)
taxonomic_effort1 <- merge(data.frame(years=min_year:max_year), taxonomic_effort1, by="years", all.x=T, all.y=F)


# Exclude these
to_exclude <- describer_data[ns_spp_N_1st_auth_s == 0]$idx_auth

# N number of describers
describers <- describer_data[!idx_auth %in% to_exclude, c("idx_auth", "full.name.of.describer.n", "min", "max")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max)
describers$years <- seq
describers <- describers %>% unnest(years)
describers[,N_real_describers := length(idx_auth), by=years]
describers_active_by_year <- unique(describers[,c("years", "N_real_describers")])[order(as.numeric(years))]

# Weighted 
describers <- describer_data[!idx_auth %in% to_exclude,c("idx_auth", "full.name.of.describer.n", "min", "max", "ns_species_per_year_active")]
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=describers$min, b=describers$max)
describers$years <- seq
describers <- describers %>% unnest(years)
describers[,  N_weighted_real_describers := sum(as.numeric(ns_species_per_year_active)), by="years"]
describers_weighted_by_year <- unique(describers[,c("years", "N_weighted_real_describers")])[order(as.numeric(years))]


taxonomic_effort2 <- merge(describers_active_by_year, describers_weighted_by_year, by="years", all.x=T, all.y=T)
taxonomic_effort <- merge(taxonomic_effort1, taxonomic_effort2, by="years", all.x=T, all.y=F)


# number of species
described_species_by_year <- describer_date[]
described_species_by_year[,N_species_described:=length(unique(idxes)),by="date.n"]
described_species_by_year <- unique(
        described_species_by_year[,c("date.n", "N_species_described")])

described_per_year_final2 <- merge(taxonomic_effort, described_species_by_year,
                                   by.x="years", by.y="date.n", all.x=T, all.y=F)

# number of synonyms
described_species_by_year <- describer_date[]
described_species_by_year[,N_species_described:=length(unique(idxes)),by="date.n"]
described_species_by_year <- unique(
        described_species_by_year[,c("date.n", "N_species_described")])

described_per_year_final2 <- merge(taxonomic_effort, described_species_by_year,
                                   by.x="years", by.y="date.n", all.x=T, all.y=F)

# synonyms
described_species_by_year <- synonyms[]
described_species_by_year[,N_synonyms:=length(unique(idx)),by="date.n"]
described_species_by_year <- unique(
        described_species_by_year[,c("date.n", "N_synonyms")])

described_per_year_final3 <- merge(described_per_year_final2, described_species_by_year,
                                   by.x="years", by.y="date.n", all.x=T, all.y=F)



described_per_year_final3[is.na(described_per_year_final3)] <- 0
write.csv(described_per_year_final3, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_6.0-active-by-year.csv"), na='', row.names=F, fileEncoding="UTF-8")

# Other metrics
# Number of taxonomists active per year DONE
# Number as first author DONE
# Number as last author DONE
# Ratio of synonym v valid name
