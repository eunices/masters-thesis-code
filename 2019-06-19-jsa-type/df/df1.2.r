# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating describer-based dataset from denormalised data.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

source('2019-06-19-jsa-type/df/functions.R')



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - summarize by species idx for checks of authors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': summarize by species idx for checks on authors"))



# Read data
describers_merged <- 
        read_escaped_data(paste0(dir_data, basefile, " describers_2.0-denormalised.csv"))

# # CHECK:
# unique(describers_merged[full.name.of.describer.n=="Ricardo Ayala Barajas"]$dod.describer.n)

# Summarize by idx
describers_idx <- describers_merged[, idxes:=paste0(idx, collapse=', '), 
                                    by=c('full.name.of.describer.n')]
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


# Order each row for each column and use row with most number of counts
describers_idx$describer.gender.n <- factor(describers_idx$describer.gender.n, 
                                            levels=c("F", "M", "U"), ordered=T)

d1 <- describers_idx[, c("full.name.of.describer.n", "describer.gender.n", "idxes", "idxes_author.order")][
        ,list(count=.N), by=c("full.name.of.describer.n", "describer.gender.n", "idxes", "idxes_author.order")][
                order(full.name.of.describer.n, -count)][
                        !duplicated(full.name.of.describer.n)]; d1$count <- NULL

d2 <- describers_idx[!dob.describer.n %in% c("U", ""), c("full.name.of.describer.n", "dob.describer.n")][
        ,list(count=.N), by=c("full.name.of.describer.n", "dob.describer.n")][
                order(full.name.of.describer.n, -count)][
                        !duplicated(full.name.of.describer.n)]; d2$count <- NULL

d3 <- describers_idx[!dod.describer.n %in% c("U", ""), c("full.name.of.describer.n", "dod.describer.n")][
        ,list(count=.N), by=c("full.name.of.describer.n", "dod.describer.n")][
                order(full.name.of.describer.n, -count)][
                        !duplicated(full.name.of.describer.n)]; d3$count <- NULL

d4 <- describers_idx[!origin.country.describer.n %in% c("U", " "), c("full.name.of.describer.n", "origin.country.describer.n")][
        ,list(count=.N), by=c("full.name.of.describer.n", "origin.country.describer.n")][
                order(full.name.of.describer.n, -count)][
                        !duplicated(full.name.of.describer.n)]; d4$count <- NULL

d5 <- describers_idx[!residence.country.describer.n %in% c("U", " "), c("full.name.of.describer.n", "residence.country.describer.n")][
        ,list(count=.N), by=c("full.name.of.describer.n", "residence.country.describer.n")][
                order(full.name.of.describer.n, -count)][
                        !duplicated(full.name.of.describer.n)]; d5$count <- NULL

d6 <- describers_idx[!institution.of.describer.n %in% c("U", " "), c("full.name.of.describer.n", "institution.of.describer.n")][
        ,list(count=.N), by=c("full.name.of.describer.n", "institution.of.describer.n")][
                order(full.name.of.describer.n, -count)][
                        !duplicated(full.name.of.describer.n)]; d6$count <- NULL

describers_idx <-  merge(d1, d2, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <-  merge(describers_idx, d3, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <-  merge(describers_idx, d4, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <-  merge(describers_idx, d5, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <-  merge(describers_idx, d6, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <- data.table(describers_idx)[order(full.name.of.describer.n),]




# Cleaning the dob
describers_idx$dob.describer.original <- describers_idx$dob.describer.n 
describers_idx$dod.describer.original <- describers_idx$dod.describer.n 

describers_idx$dob.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                       describers_idx$dob.describer.n)




# Cleaning the dod
describers_idx$dod.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                       describers_idx$dod.describer.n)

describers_idx$dob.describer.n <- gsub(";| ", "", describers_idx$dob.describer.n)
describers_idx$dod.describer.n <- gsub(";| ", "", describers_idx$dod.describer.n)

describers_idx$origin.country.describer.original <- describers_idx$origin.country.describer.n
describers_idx$residence.country.describer.original <- describers_idx$residence.country.describer.n




# Cleaning origin country
describers_idx$origin.country.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                   describers_idx$origin.country.describer.n)

describers_idx$origin.country.describer.n <- 
        gsub(";| ", "", describers_idx$origin.country.describer.n) # remove trailing ; 


# Cleaning residence country
describers_idx$residence.country.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                   describers_idx$residence.country.describer.n)

describers_idx$residence.country.describer.n <- 
        gsub(" ", "; ", describers_idx$residence.country.describer.n)

describers_idx$residence.country.describer.n <- 
        gsub("; $", "", describers_idx$residence.country.describer.n) # remove trailing ; 




# Check whether alive
describers_idx$alive <- "N"
describers_idx[grepl("\\[alive in 2019\\]", describers_idx$dod.describer.original)]$alive <- "Y"




# Create index
describers_idx$idx_auth <- 1:dim(describers_idx)[1]



# Make edits with describer_edits.csv
describers_template_edits <- read_escaped_data(paste0(dir_data, "clean/describer_edits.csv")) 
# changed to DL

# Remove rows with all NAs
describers_template_edits <- 
        describers_template_edits[rowSums(is.na(describers_template_edits)) !=
                                  ncol(describers_template_edits)-1, ] 
describers_template_edits$alive_certainty <- NULL
describers_template_edits$residence.country.describer.comment <- NULL

# For each column name, merge changes which are not NA/ blanks
for (i in 2:length(names(describers_template_edits))) {

    col_name <- names(describers_template_edits)[i]
    cols <- c("full.name.of.describer.n", col_name)
    describers_idx <- merge_column(describers_idx, 
                                   describers_template_edits[,..cols], 
                                   "full.name.of.describer.n")

}

describers_idx$full.name.of.describer.n <- gsub('\\"\\"', '\\"', 
                                                describers_idx$full.name.of.describer.n )






# Quick fixes
describers_idx[full.name.of.describer.n=='Francesco ["Franz"] von Biegeleben']$dob.describer.n = "1881"
describers_idx[full.name.of.describer.n=='Francesco ["Franz"] von Biegeleben']$dod.describer.n = "1942"
describers_idx[full.name.of.describer.n=='Francesco ["Franz"] von Biegeleben']$origin.country.describer.n = "IT"
describers_idx[full.name.of.describer.n=='Francesco ["Franz"] von Biegeleben']$residence.country.describer.n = "IT"
describers_idx[full.name.of.describer.n=='Francesco ["Franz"] von Biegeleben']$describer.gender.n = "M"

# Row by row discrepancies
describers_idx[full.name.of.describer.n=="Ismael Alejandro Hinojosa-Díaz", "min"] = "2003"
describers_idx[full.name.of.describer.n=="Johan Christian Fabricius", "max"] = "1804"
describers_idx[full.name.of.describer.n=="Michael Kuhlmann", "min"] = "1998"
describers_idx[full.name.of.describer.n=="Eduardo Andrade Botelho de Almeida", "min"] = "2008" # should be modified in original file
describers_idx[full.name.of.describer.n=="Michael Scott Engel", "min"] = "1995" # should be modified in original file
describers_idx[full.name.of.describer.n=="[Carl Eduard] Adolph Gerstaecker", "full.name.of.describer.n"] = "[Carl Eduard] Adolph Gerstäcker"
describers_idx[full.name.of.describer.n=="Francisco Javier Ortiz-Sanchez", "full.name.of.describer.n"] = "Francisco Javier Ortiz-Sánchez"




# Write file
filename_write = paste0(dir_data, basefile, " describers_3.0-by-author.csv")
write.csv(describers_idx, filename_write, na='', row.names=F, fileEncoding="UTF-8")
