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

if (loop_2=='Y') {
    run_loop()
}
