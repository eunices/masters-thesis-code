closestWordMatch = function(string, stringVector){

  distance = levenshteinSim(string, stringVector)

  stringVector[distance == max(distance)][1] # gets the closest 

}

format_names = function(vec) {

    # Remove punctuations
    vec <- gsub("\\.\\.", "\\.",                     # remove double ..
        gsub(" ", ".",                               # remove spaces
            gsub("[[:punct:]]", "", tolower(vec))    # remove punctuation
        )
    )

    # Convert "funky" alphabets
    vec <- iconv(vec, from = 'UTF-8', to = 'ASCII//TRANSLIT') 

    # Renaming this long name
    is_name_full_name <- grepl("full.name.a.e", vec)

    if (any(is_name_full_name)) {
        vec[which(is_name_full_name)] <- 'full.name' 
    }
    
    vec
}


read_escaped_data_v2 = function(filepath, escape=T) {

    df <- fread(
        filepath, 
        integer64 = 'character', 
        na.strings = c(''), 
        encoding = 'UTF-8'
    )
    
    if(escape) {
        df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
    }

    if("idx" %in% names(df)) {
        df$idx <- as.integer(df$idx)
    }

    numerics <- c("lat_n", "lon_n")
    numerics_edit <- paste0(numerics, "_edit")
    numerics <- c(numerics, numerics_edit)
    numerics <- numerics[numerics %in% names(df)]
    
    if(length(numerics) > 0) {
        df[, c(numerics) := lapply(.SD, as.numeric), .SDcols = numerics]
    }

    df
}


run_describer_split_loop_v2 <- function(
    describers_info, 
    strsplit_cty = "; "
) {

    # Split authors by ;
    describers_info$full.name.of.describer.n <- 
        lapply(describers_info$full.name.of.describer, strsplit, split = "; ")

    describers_info$describer.gender.n <-
        lapply(describers_info$describer.gender, strsplit, split = "")

    describers_info$dob.describer.n <- 
        lapply(describers_info$dob.describer, strsplit, split = "; ")

    describers_info$dod.describer.n <- 
        lapply(describers_info$dod.describer, strsplit, split = "; ")

    describers_info$origin.country.describer.n <- 
        lapply(
            describers_info$origin.country.describer, 
            strsplit, 
            split = strsplit_cty
        )

    describers_info$residence.country.describer.n <- 
        lapply(
            describers_info$residence.country.describer, 
            strsplit,
            split = strsplit_cty 
        )

    describers_info$institution.of.describer.n <- 
        lapply(describers_info$institution.of.describer, strsplit, split = "; ")

    # Create a new row for each author
    describers <- data.frame(
        idx = character(),
        full.name.of.describer.n = character(),
        describer.gender.n = character(), 
        dob.describer.n = character(),
        dod.describer.n = character(), 
        origin.country.describer.n = character(),
        residence.country.describer.n = character(), 
        institution.of.describer.n = character(),
        author.order = integer()
    )

    n_rows <- dim(describers_info)[1]

    for (i in 1:n_rows) {
        
        idx_row <- describers_info[i]$idx
        describer_row <- describers_info[i]$full.name.of.describer.n[[1]][[1]]
        gender_row <- describers_info[i]$describer.gender.n[[1]][[1]]
        dob_row <- describers_info[i]$dob.describer.n[[1]][[1]]
        dod_row <- describers_info[i]$dod.describer.n[[1]][[1]]
        origin_row <- describers_info[i]$origin.country.describer.n[[1]][[1]]
        residence_row <- 
            describers_info[i]$residence.country.describer.n[[1]][[1]]
        inst_row <- describers_info[i]$institution.of.describer.n[[1]][[1]]

        if(!identical(describer_row, character(0))){
            for (j in 1:length(describer_row)) {
                if(is.na(describer_row[j])) {
                    
                    to_merge <- data.frame(
                        idx = idx_row, 
                        full.name.of.describer.n = NA,
                        describer.gender.n = NA, 
                        dob.describer.n = NA,
                        dod.describer.n = NA, 
                        origin.country.describer.n = NA,
                        residence.country.describer.n = NA, 
                        institution.of.describer.n = NA, 
                        author.order = NA
                    )

                } else {

                    gender <- ifelse(
                        is.na(gender_row[j]) || 
                        identical(gender_row[j], logical(0)),
                        NA, gender_row[j]
                    )

                    dob <- ifelse(
                        is.na(dob_row[j]) || 
                              identical(dob_row[j], logical(0)),                    
                        NA, dob_row[j]
                    )

                    dod <- ifelse(
                        is.na(dod_row[j]) || 
                        identical(dod_row[j], logical(0)), 
                        NA, dod_row[j]
                    )

                    origin <- ifelse(
                        is.na(origin_row[j]) ||
                        identical(origin_row[j], logical(0)),
                        NA, origin_row[j]
                    )

                    residence <- ifelse(
                        is.na(residence_row[j]) || 
                        identical(residence_row[j], logical(0)), 
                        NA, residence_row[j]
                    )
                            
                    inst <- ifelse(
                        is.na(inst_row[j]) || 
                        identical(inst_row[j], logical(0)),
                        NA, inst_row[j]
                    )

                    order <- ifelse(j==1, 1, 
                        
                        ifelse(j==length(describer_row) & 
                            length(describer_row) != 2, "L", 
                            
                            ifelse(j==length(describer_row) & 
                                length(describer_row) == 2, "S", j)
                        )
                    )

                    to_merge <- data.frame(
                        idx = idx_row,
                        full.name.of.describer.n = describer_row[j],
                        describer.gender.n = gender,
                        dob.describer.n = dob,
                        dod.describer.n = dod,
                        origin.country.describer.n = origin,
                        residence.country.describer.n = residence,
                        institution.of.describer.n = inst,
                        author.order = order
                    )

                    describers <- rbind(describers, to_merge)

                }
            }

        } else {

            to_merge <- data.frame(
                idx = idx_row, 
                full.name.of.describer.n = NA,
                describer.gender.n = NA, 
                dob.describer.n = NA,
                dod.describer.n = NA, 
                origin.country.describer.n = NA,
                residence.country.describer.n = NA,
                institution.of.describer.n = NA,
                author.order = NA
            )

            describers <- rbind(describers, to_merge)

        }

        percent <- round(i/n_rows*100, 5)
        if(percent %% 10 == 0) print(paste0(percent , "% completed"))

    }
    
    describers
}

replace_edits <- function(clean_manual, df) {

    cols <- names(clean_manual)[grepl("_edit", names(clean_manual))]  

    for (col in cols) {

        cols_subset <- c("idx", col)
        df_new <- 
            clean_manual[!(is.na(get(col)) | get(col) == ""), ..cols_subset]

        col_original <- gsub("_edit", "", col)

        replacement <- df_new[, get(col)]

        df[match(df_new$idx, idx),  col_original] <- replacement

        # Check
        # cbind(
        #     df[match(df_new$idx, idx),  col_original, with = F],
        #     df_new[, get(col)]
        # )

    }

    df

}

rename_names <- function(df, old_name, new_name) {
    if (old_name %in% names(df)) {
        names(df)[which(names(df) == old_name)] <- new_name
    }
    df
}

update_data_with_edits <- function(cfile, df, sep = NA) {
    
    clean_manual <- read_escaped_data_v2(cfile)

    if (!is.na(sep)) {
        clean_manual <- separate_rows(clean_manual, sep, sep = ", ")
    }

    clean_manual <- rename_names(df, "idxes", "idx")

    df <- replace_edits(clean_manual, df)

    df

}

    




