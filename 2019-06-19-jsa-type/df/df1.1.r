# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating denormalised describer-species dataset.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


source('2019-06-19-jsa-type/df/functions.R')
# TODO: modularise the functions used to create derived datasets


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - create describer raw dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- describer raw dataset"))



# Read data
df <- read_escaped_data(paste0(dir_data, basefile, " filtered_4.3-clean-coll.csv"))
df_s <- read_escaped_data(paste0(dir_data, basefile, " oth_4.3-clean-coll.csv"))




# Subset data
describer_cols <- c("idx", "author", "full.name.of.describer", "describer.gender",
                    "dob.describer", "dod.describer", "origin.country.describer",
                    "residence.country.describer", "institution.of.describer")

describers_info_valid_species <- df[,..describer_cols]
describers_info_synonyms <- df_s[,..describer_cols]
describers_info <- rbind(describers_info_valid_species, describers_info_synonyms)




# Write data
filename_write = paste0(dir_data, basefile, " describers_1.0-all.csv")
write.csv(describers_info[order(author)], filename_write, na='', row.names=F, fileEncoding="UTF-8")





# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section -  individual author species rows 
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': individual author species rows"))



# Read data
describers_info = read_escaped_data(paste0(dir_data, basefile, " describers_1.0-all.csv"))

# A loop was written because currently this doesn't really work!~
# describers <- describers_info %>% separate_rows(full.name.of.describer, 
#                                                 describer.gender, dob.describer,
#                                                 dod.describer, origin.country.describer,
#                                                 residence.country.describer, 
#                                                 institution.of.describer, 
#                                                 sep=";|,", convert=T)




# # =================
# # DONE ONCE ONLY ##
# # =================

if (loop_3=='Y') {

    # Split data
    describers <- data.table(run_describer_split_loop())

    # Clean dob
    describers$dob.describer.n <- gsub("[^0-9]", "", describers$dob.describer.n)
    describers$dob.describer.n <- as.numeric(describers$dob.describer.n)
    describers[dob.describer.n>3000]$dob.describer.n <- NA

    # Clean dod
    describers$dod.describer.n <- gsub("[^0-9]", "", describers$dod.describer.n)
    describers$dod.describer.n <- as.numeric(describers$dod.describer.n)
    describers[dob.describer.n>3000]$dob.describer.n <- NA

    # Clean gender
    describers[!describer.gender.n %in% c("M", "F", "U")]$describer.gender.n <- ""

    # Remove digits from institution
    describers$institution.of.describer.n <- gsub("[[:digit:]]", "", 
        describers$institution.of.describer.n)

    # Write data
    filename_write = paste0(dir_data, basefile, " describers_2.0-denormalised.csv")
    write.csv(describers, filename_write, na='', row.names=F, fileEncoding="UTF-8")
}

