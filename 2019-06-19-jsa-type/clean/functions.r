###################################### clean1.r



replace_df_nas = function(df) {
	replace_na <- c('other,_unknown,_or none', 
				    'other,_unknown,_or_other',
					'other,_unknown,_or_none')

	for (i in 1:length(replace_na)){
		df[, names(df) := lapply(.SD, function(x) gsub(replace_na[i], NA, x))]
	}
	
	return(df)
}




rename_df_names = function(df) {

    # Remove punctuations
    names(df) <- gsub("\\.\\.", "\\.", 
        gsub(" ", ".", gsub("[[:punct:]]", "", 
            tolower(names(df)))))

    # Convert "funky" alphabets
    names(df) <- iconv(names(df), from='UTF-8', to='ASCII//TRANSLIT') 

    # Renaming this long name
    if (any(grepl("full.name.a.e", names(df)))) {
        names(df)[which(grepl("full.name.a.e", names(df)))] <- 'full.name' 
    }

    return(df)
}



###################################### clean3.r



paste_nine = function(numeric){
    char <- strsplit(as.character(numeric), "")[[1]]
    word <- paste0(char[1], "9", char[3], char[4])
    as.character(word)
}



###################################### df1.r



format_short <- function(x){
    auths <- strsplit(x, split="; ")[[1]]
    len <- length(auths)
    auths <- fn[auths]

    if(len==1) {
        string <- auths
    } else if(len==2) {
        string <- paste0(auths[1], " and ", auths[2])
    } else if(len>=3) {
        string <- auths[1]
        for (i in 2:(len-1)) {
            string <- paste0(string, ", ", auths[i])
        }
        string <- paste0(string, ", and ", auths[len])
    }
    string
}



###################################### df2.r


run_collectors_loop <- function(collectors_unique) {

	# Split the "list" columns of the collectors dataset
	# If in doubt on what this function does, print the "collectors_unique" df

	# Initialize dataset
    collectors <- data.frame(idxes=character(),
                             collector.of.type.n=character(),
                             full.name.of.collector.n=character(),
                             collector.gender.n=character(),
                             title.of.collector.n=character(),
                             info.about.collector.n=character())

	# Number of rows
    n_rows = dim(collectors_unique)[1]

    # Create a new row for each author
    for (i in 1:n_rows) {

        idx_row <- collectors_unique[i]$idxes
        collector_row <- collectors_unique[i]$collector.of.type.n[[1]][[1]]
        gender_row <- collectors_unique[i]$collector.gender.n[[1]][[1]]
        fname_row <- collectors_unique[i]$full.name.of.collector.n[[1]][[1]]
        title_row <- collectors_unique[i]$title.of.collector.n[[1]][[1]]
        info_row <- collectors_unique[i]$info.about.collector[[1]][[1]]

		# If entire row is NOT empty: 
        if(!identical(collector_row, character(0))){

            for (j in 1:length(collector_row)) {

				# If first author is empty:
                if(is.na(collector_row[j])) {

                    to_merge <- data.frame(idxes=idx_row,
                                           collector.of.type.n=NA,
                                           full.name.of.collector.n=NA,
                                           collector.gender.n=NA,
                                           title.of.collector.n=NA,
                                           info.about.collector.n=NA)

				# If at least one collector is present: 				
                } else {

                    collector <- ifelse(is.na(collector_row[j]) || 
										identical(collector_row[j], logical(0)),
									    NA, collector_row[j])

                    gender <- ifelse(is.na(gender_row[j]) || 
									 identical(gender_row[j], logical(0)),
									 NA, gender_row[j])

                    fname <- ifelse(is.na(fname_row[j]) || 
									identical(fname_row[j], logical(0)),
								    NA, fname_row[j])

                    title <- ifelse(is.na(title_row[j]) || 
								    identical(title_row[j], logical(0)),
								    NA, title_row[j])

                    info <- ifelse(is.na(info_row[j]) ||
								   identical(info_row[j], logical(0)),
								   NA, info_row[j])

                    to_merge <- data.frame(idxes=idx_row,
                                           collector.of.type.n=collector,
                                           full.name.of.collector.n=fname,
                                           collector.gender.n=gender,
                                           title.of.collector.n=title,
                                           info.about.collector.n=info)

                    collectors <- rbind(collectors, to_merge)

                }
            }

		# If entire row is empty:
        } else {

            to_merge <- data.frame(idxes=idx_row,
                                   collector.of.type.n=NA,
                                   full.name.of.collector.n=NA,
                                   collector.gender.n=NA,
                                   title.of.collector.n=NA,
                                   info.about.collector.n=NA)

            collectors <- rbind(collectors, to_merge)

        }

		# Print progress
        percent <- round(i/n_rows*100, 2)
        if(percent %% 10 == 0) {
            print(paste0(percent , "% completed"))
        }

    }

    return(collectors)
}




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
