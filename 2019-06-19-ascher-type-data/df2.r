source('2019-06-19-ascher-type-data/init.r')

# Libraries
library(dplyr)
library(tidyr)

# Describers dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section -  get individual rows from dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': get individual rows from dataset"))

describers_info <- fread(
    paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.0-describers_edit.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

# # =================
# # DONE ONCE ONLY ##
# # =================


# # Split authors by ;
# split_semicolon <- function(x) {
#     strsplit(x, "; ")
# }
# split_gender <- function(x) {
#     strsplit(x, "")
# }

# describers_info$full.name.of.describer.n <- lapply(describers_info$full.name.of.describer,
#                                                    split_semicolon)
# describers_info$describer.gender.n <- lapply(describers_info$describer.gender,
#                                              split_gender)
# describers_info$dob.describer.n <- lapply(describers_info$dob.describer,
#                                           split_semicolon)
# describers_info$dod.describer.n <- lapply(describers_info$dod.describer,
#                                           split_semicolon)
# describers_info$origin.country.describer.n <- lapply(describers_info$origin.country.describer,
#                                                      split_semicolon)
# describers_info$residence.country.describer.n <- lapply(describers_info$residence.country.describer,
#                                                        split_semicolon)
# describers_info$institution.of.describer.n <- lapply(describers_info$institution.of.describer,
#                                                      split_semicolon)

# # Create a new row for each author
# describers <- data.frame(idx=character(), full.name.of.describer.n=character(),
#                          describer.gender.n=character(), dob.describer.n=character(),
#                          dod.describer.n=character(), origin.country.describer.n=character(),
#                          residence.country.describer.n=character(), institution.of.describer.n=character())
# for (i in 1:dim(describers_info)[1]) {
# # for (i in 1:2) {
#     idx_row <- describers_info[i]$idx
#     describer_row <- describers_info[i]$full.name.of.describer.n[[1]][[1]]
#     gender_row <- describers_info[i]$describer.gender.n[[1]][[1]]
#     dob_row <- describers_info[i]$dob.describer.n[[1]][[1]]
#     dod_row <- describers_info[i]$dod.describer.n[[1]][[1]]
#     origin_row <- describers_info[i]$origin.country.describer.n[[1]][[1]]
#     residence_row <- describers_info[i]$residence.country.describer.n[[1]][[1]]
#     inst_row <- describers_info[i]$institution.of.describer.n[[1]][[1]]

#     if(!identical(describer_row, character(0))){
#         for (j in 1:length(describer_row)) {
#             if(is.na(describer_row[j])) {
#                 to_merge <- data.frame(idx=idx_row, full.name.of.describer.n=NA,
#                          describer.gender.n=NA, dob.describer.n=NA,
#                          dod.describer.n=NA, origin.country.describer.n=NA,
#                          residence.country.describer.n=NA, institution.of.describer.n=NA)
#             } else {

#                 gender <- ifelse(is.na(gender_row[j]) || identical(gender_row[j], logical(0)) , NA, gender_row[j])
#                 dob <- ifelse(is.na(dob_row[j]) || identical(dob_row[j], logical(0)), NA, dob_row[j])
#                 dod <- ifelse(is.na(dod_row[j]) || identical(dod_row[j], logical(0)), NA, dod_row[j])
#                 origin <- ifelse(is.na(origin_row[j]) || identical(origin_row[j], logical(0)), NA, origin_row[j])
#                 residence <- ifelse(is.na(residence_row[j]) || identical(residence_row[j], logical(0)), 
#                                     NA, residence_row[j])
#                 inst <- ifelse(is.na(inst_row[j]) || identical(inst_row[j], logical(0)), NA, inst_row[j])

#                 to_merge <- data.frame(idx=idx_row,
#                                        full.name.of.describer.n=describer_row[j],
#                                        describer.gender.n=gender,
#                                        dob.describer.n=dob,
#                                        dod.describer.n=dod,
#                                        origin.country.describer.n=origin,
#                                        residence.country.describer.n=residence,
#                                        institution.of.describer.n=inst)
#                 describers <- rbind(describers, to_merge)
#             }
#         }
#     } else {
#         to_merge <- data.frame(idx=idx_row, full.name.of.describer.n=NA,
#                          describer.gender.n=NA, dob.describer.n=NA,
#                          dod.describer.n=NA, origin.country.describer.n=NA,
#                          residence.country.describer.n=NA, institution.of.describer.n=NA)
#         describers <- rbind(describers, to_merge)
#     }
#     print(paste0("Row ", i , " completed for ", idx_row))
# }

# write.csv(describers, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.0-describers2.csv"), na='', row.names=F, fileEncoding="UTF-8")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - summarize by idx
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': summarize by idx"))

describers <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.0-describers2.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')

# Merge back the other columns
describers_merged <- data.table(describers)
cols <- c("idx", "author", "full.name.of.describer", "describer.gender", 
          "dob.describer", "dod.describer",
          "origin.country.describer", "residence.country.describer", "institution.of.describer")
describers_merged[] <- lapply(describers_merged, as.character)
describers_info[] <- lapply(describers_info, as.character)
describers_merged <- merge(describers_merged, describers_info[,..cols], by='idx', all.x=T, all.y=F)

# Summarize by idx
describers_idx <- describers_merged[, idxes:=paste0(unique(as.numeric(idx)), collapse=', '), 
                                      by=c("full.name.of.describer.n")]
describers_idx <- describers_idx[,c("full.name.of.describer.n", 
                                    "describer.gender.n",
                                    "dob.describer.n",
                                    "dod.describer.n",
                                    "origin.country.describer.n",
                                    "residence.country.describer.n",
                                    "institution.of.describer.n", 
                                    "idxes")]
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
describers_idx$idx_auth <- 1:dim(describers_idx)[1]

write.csv(describers_idx, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.1-describers.csv"), na='', row.names=F, fileEncoding="UTF-8")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - further cleaning 
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': further cleaning"))

describers <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.1-describers.csv"),
            na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describers$describer.gender.n <- factor(describers$describer.gender.n, levels=c("F", "M", "U"), ordered=T)
describers <- describers[order(full.name.of.describer.n, describer.gender.n)]
dim(describers)
describers <- describers[!duplicated(full.name.of.describer.n)]
dim(describers)
describers$describer.gender.n <- as.character(describers$describer.gender.n)

write.csv(describers, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.2-describers.csv"), na='', row.names=F, fileEncoding="UTF-8")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - further denormalization of data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': further denormalization of data"))

describers <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.2-describers.csv"),
            na.strings=c('', 'NA'), encoding="UTF-8", quote='"')

# Dataset unnested from idxes
describers <- describers %>% 
  mutate(idxes = strsplit(as.character(idxes), ",")) %>%
  unnest(idxes)

describers <- data.table(describers)

write.csv(describers, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.3-describers.csv"), na='', row.names=F, fileEncoding="UTF-8")

describers <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.3-describers.csv"),
            na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
describers$idxes <- as.numeric(describers$idxes)

describers[, N_authors := length(unique(full.name.of.describer.n)), by="idxes"]
describers[, names_authors:=paste(unique(full.name.of.describer.n), collapse="; "), by="idxes"]
describers[, idx_authors:=paste(unique(idx_auth), collapse="; "), by="idxes"]
describers[, gender_authors:=paste(describer.gender.n, collapse="; "), by="idxes"]
describers <- describers[,c("idxes", "N_authors", "idx_authors", "gender_authors")]
describers <- unique(describers)
describers$idxes <- as.numeric(describers$idxes)
describers <- describers[order(idxes)]

write.csv(describers, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.4-describers.csv"), na='', row.names=F, fileEncoding="UTF-8")

# https://stackoverflow.com/questions/13773770/


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - joining author data back to main dataframe
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': joining author data back to main dataframe"))

df <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.1-useful-col.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

describers <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.4-describers.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

describers2 <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.1-describers.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

df$idx <- as.numeric(df$idx)
describers$idx <- as.numeric(describers$idx)

df <- merge(df, describers, by.x="idx", by.y="idxes", all.x=T, all.y=F)
df$idx.y <- NULL

df <- df[,c("date.n", "idx_authors")]
df <- df %>% 
  mutate(idx_authors = strsplit(as.character(idx_authors), "; ")) %>%
  unnest(idx_authors)
df <- data.table(df)

df[,max:=max(date.n), by=idx_authors]
df[,min:=min(date.n), by=idx_authors]
df <- unique(df[,c("idx_authors", "min", "max")])
df$idx_authors <- as.integer(df$idx_authors)
df <- merge(df, describers2, by.x="idx_authors", by.y="idx_auth", all.x=T, all.y=F)

df[full.name.of.describer.n=="Ismael Alejandro Hinojosa-Díaz"]$min = 2003  
df[full.name.of.describer.n=="Johan Christian Fabricius"]$max = 1804  
df[full.name.of.describer.n=="Michael Kuhlmann"]$min = 1998  
df[full.name.of.describer.n=="Eduardo Andrade Botelho de Almeida"]$min = 2008 # should be modified in original file
df[full.name.of.describer.n=="Michael Scott Engel"]$min = 1995 # should be modified in original file

dob = df[full.name.of.describer.n=="Wilhelm Albert Schulz"]$dod.describer.n
dod = df[full.name.of.describer.n=="Wilhelm Albert Schulz"]$dob.describer.n  
df[full.name.of.describer.n=="Wilhelm Albert Schulz"]$dod.describer.n = dod
df[full.name.of.describer.n=="Wilhelm Albert Schulz"]$dob.describer.n  = dob

df$dob.describer.n <- gsub(";| ", "", df$dob.describer.n)
df$dod.describer.n <- gsub(";| ", "", df$dod.describer.n)

df$years_active <- df$max - df$min +1
df$years_alive <- as.numeric(df$dod.describer.n) - as.numeric(df$dob.describer.n) + 1
df$years_discrepancy <- df$years_alive - df$years_active

df[, num_species_described:= length(strsplit(idxes, ", ")[[1]]), by=idx_authors]

df$species_per_year_active <- df$num_species_described / df$years_active
df$species_per_year_alive <- df$num_species_described / df$years_alive

cols <- c("idx_authors", "full.name.of.describer.n", "describer.gender.n", "min", "max", "years_active",
          "dob.describer.n", "dod.describer.n", "years_alive", "years_discrepancy", "idxes", "num_species_described", 
          "origin.country.describer.n", "residence.country.describer.n", "institution.of.describer.n")
# idxes = species idxes

write.csv(df[,..cols], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2.5-describers_minmax.csv"), na='', row.names=F, fileEncoding="UTF-8")

