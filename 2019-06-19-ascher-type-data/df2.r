source('2019-06-19-ascher-type-data/init.r')

# Describers dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section -  get individual rows from dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': get individual rows from dataset"))

describers_info <- fread(
    paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2-describers_edit.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

# Split authors by ;
split_semicolon <- function(x) {
    strsplit(x, "; ")
}
split_gender <- function(x) {
    strsplit(x, "")
}

describers_info$full.name.of.describer.n <- lapply(describers_info$full.name.of.describer,
                                                   split_semicolon)
describers_info$describer.gender.n <- lapply(describers_info$describer.gender,
                                             split_gender)
describers_info$dob.describer.n <- lapply(describers_info$dob.describer,
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
                         residence.country.describer.n=character(), institution.of.describer.n=character())
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
                         residence.country.describer.n=NA, institution.of.describer.n=NA)
            } else {

                gender <- ifelse(is.na(gender_row[j]) || identical(gender_row[j], logical(0)) , NA, gender_row[j])
                dob <- ifelse(is.na(dob_row[j]) || identical(dob_row[j], logical(0)), NA, dob_row[j])
                dod <- ifelse(is.na(dod_row[j]) || identical(dod_row[j], logical(0)), NA, dod_row[j])
                origin <- ifelse(is.na(origin_row[j]) || identical(origin_row[j], logical(0)), NA, origin_row[j])
                residence <- ifelse(is.na(residence_row[j]) || identical(residence_row[j], logical(0)), 
                                    NA, residence_row[j])
                inst <- ifelse(is.na(inst_row[j]) || identical(inst_row[j], logical(0)), NA, inst_row[j])

                to_merge <- data.frame(idx=idx_row,
                                       full.name.of.describer.n=describer_row[j],
                                       describer.gender.n=gender,
                                       dob.describer.n=dob,
                                       dod.describer.n=dod,
                                       origin.country.describer.n=origin,
                                       residence.country.describer.n=residence,
                                       institution.of.describer.n=inst)
                describers <- rbind(describers, to_merge)
            }
        }
    } else {
        to_merge <- data.frame(idx=idx_row, full.name.of.describer.n=NA,
                         describer.gender.n=NA, dob.describer.n=NA,
                         dod.describer.n=NA, origin.country.describer.n=NA,
                         residence.country.describer.n=NA, institution.of.describer.n=NA)
        describers <- rbind(describers, to_merge)
    }
    print(paste0("Row ", i , " completed for ", idx_row))
}


# Merge back the other columns
describers_merged <- data.table(describers)
cols <- c("idx", "author", "full.name.of.describer", "describer.gender", 
          "dob.describer", "dod.describer",
          "origin.country.describer", "residence.country.describer", "institution.of.describer")
describers_merged[] <- lapply(describers_merged, as.character)
describers_info[] <- lapply(describers_info, as.character)
describers_merged <- merge(describers_merged, describers_info[,..cols], by='idx', all.x=T, all.y=F)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - summarize by idx
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': summarize by idx"))

describers_idx <- describers_merged[, idxes:=paste0(unique(as.numeric(idx)), collapse=', '), 
                                      by=c("full.name.of.describer.n")]
describers_idx <- unique(describers_idx[,c("full.name.of.describer.n", 
                                           "describer.gender.n",
                                        #    "dob.describer.n",
                                        #    "dod.describer.n",
                                        #    "origin.country.describer.n",
                                        #    "residence.country.describer.n",
                                        #    "institution.of.describer.n", 
                                           "idxes")])
describers_idx[] <- lapply(describers_idx, as.character)
describers_idx <- data.table(describers_idx)
describers_idx <- describers_idx[order(full.name.of.describer.n),]
describers_idx$idx_auth <- 1:dim(describers_idx)[1]

write.csv(describers_idx, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2-describers_edit_spreaded-out.csv"), na='', row.names=F, fileEncoding="UTF-8")

describers_merged <- merge(describers_merged, describers_info[,..cols], by='idx', all.x=T, all.y=F)



# Pseudo code
## Get individual rows of unique authors 
## Group by idxes
## Clean individual authors and assign a unique idx_auth manually
