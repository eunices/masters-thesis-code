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



