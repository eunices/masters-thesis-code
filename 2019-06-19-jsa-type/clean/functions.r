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



