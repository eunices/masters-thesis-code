

###################################### df3.1.r



run_describer_split_loop <- function() {

    # Split authors by ;
    describers_info$full.name.of.describer.n <- 
        lapply(describers_info$full.name.of.describer, strsplit, split="; ")

    describers_info$describer.gender.n <-
        lapply(describers_info$describer.gender, strsplit, split="")

    describers_info$dob.describer.n <- 
        lapply(describers_info$dob.describer, strsplit, split="; ")

    describers_info$dod.describer.n <- 
        lapply(describers_info$dod.describer, strsplit, split="; ")

    describers_info$origin.country.describer.n <- 
        lapply(describers_info$origin.country.describer, strsplit, split="; ")

    describers_info$residence.country.describer.n <- 
        lapply(describers_info$residence.country.describer, strsplit, split="; ")

    describers_info$institution.of.describer.n <- 
        lapply(describers_info$institution.of.describer, strsplit, split="; ")

    # Create a new row for each author
    describers <- data.frame(idx=character(), full.name.of.describer.n=character(),
                            describer.gender.n=character(), dob.describer.n=character(),
                            dod.describer.n=character(), origin.country.describer.n=character(),
                            residence.country.describer.n=character(), 
                            institution.of.describer.n=character(),
                            author.order=integer())

    n_rows <- dim(describers_info)[1]

    for (i in 1:n_rows) {
        
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

                    gender <- ifelse(is.na(gender_row[j]) || 
                                     identical(gender_row[j], logical(0)),
                                     NA, gender_row[j])

                    dob <- ifelse(is.na(dob_row[j]) || 
                                  identical(dob_row[j], logical(0)),                    
                                  NA, dob_row[j])

                    dod <- ifelse(is.na(dod_row[j]) || 
                                  identical(dod_row[j], logical(0)), 
                                  NA, dod_row[j])

                    origin <- ifelse(is.na(origin_row[j]) ||
                                     identical(origin_row[j], logical(0)),
                                     NA, origin_row[j])

                    residence <- ifelse(is.na(residence_row[j]) || 
                                        identical(residence_row[j], logical(0)), 
                                        NA, residence_row[j])
                            
                    inst <- ifelse(is.na(inst_row[j]) || 
                                   identical(inst_row[j], logical(0)),
                                   NA, inst_row[j])

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

        percent <- round(i/n_rows*100, 2)
        if(percent %% 10 == 0) print(paste0(percent , "% completed"))

    }
    
    describers
}



###################################### df3.2.r




merge_column <- function(df, df_change, by) {
    # this allows me to merge df with blanks [i.e. no changes]

    # Get column names to modify
    col_change <- names(df_change)[names(df_change) != by]
    names(df_change)[names(df_change) == col_change] <- "modify"
    print(paste0("Column to be changed: ", col_change))

    # Modify columns where row is not "NA"/blank
    df <- merge(df, df_change, by=by, all.x=T, all.y=F)
    df[!is.na(modify), col_change] <- df[!is.na(modify),]$modify
    df$modify <- NULL
    
    return(df)

}
