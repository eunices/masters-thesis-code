# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - summarize by species idx for checks of authors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': summarize by species idx for checks on authors"))

describers <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_2.0-denormalised.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')

describers[, names(describers) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

describers_info <- fread(
    paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_1.0-all_edit.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

describers_info[, names(describers_info) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# merging manual edits
# # =================
# # DONE ONCE ONLY ##
# # =================

# check <- fread(
#     paste0(dir, "clean/missing_authors_edit.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

# check[, names(check) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# describers_info2 <- merge(describers_info, check, by='author', suffixes=c('_old', '_new'), all.x=T, all.y=F)
# describers_info2$check <- describers_info2$full.name.of.describer_old == describers_info2$full.name.of.describer_new

# write.csv(describers_info2[check==FALSE | is.na(check),c("idx", "author", "full.name.of.describer_old", "full.name.of.describer_new", "check")], paste0(dir, "clean/describers_info.csv"), na='', row.names=F, fileEncoding="UTF-8")

# describers_info2 <- fread(
#     paste0(dir, "clean/describers_info_edit.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

# describers_info2[, names(describers_info2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# remove_idx <- describers_info2$idx
# describers_info.1 <- describers_info[idx %in% remove_idx]
# describers_info.2 <- describers_info[!idx %in% remove_idx]

# describers_info.1 <- merge(describers_info.1, describers_info2[,c("idx",
#                            "full.name.of.describer_new", "author")], by.x="idx", by.y="idx",
#                            suffixes=c("", "_new"))

# describers_info.1$full.name.of.describer <- describers_info.1$full.name.of.describer_new
# describers_info.1$author <- describers_info.1$author_new
# describers_info.1$author_new <- NULL
# describers_info.1$full.name.of.describer_new <- NULL

# describers_info <- rbind(describers_info.1, describers_info.2)

# write.csv(describers_info[order(type, idx),], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_1.0-all_edit.csv"), na='', row.names=F, fileEncoding="UTF-8")


# Merge back the other columns
describers_merged <- data.table(describers)
cols <- c("idx", "author", "full.name.of.describer", "describer.gender", 
          "dob.describer", "dod.describer",
          "origin.country.describer", "residence.country.describer", "institution.of.describer")
describers_merged[] <- lapply(describers_merged, as.character)
describers_info[] <- lapply(describers_info, as.character)
describers_merged <- merge(describers_merged, describers_info[,..cols], by='idx', all.x=T, all.y=F)

# Summarize by idx
describers_idx <- describers_merged[, idxes:=paste0(idx, collapse=', '), by=c('full.name.of.describer.n')]
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


# Order and leave out blanks
describers_idx$describer.gender.n <- factor(describers_idx$describer.gender.n, levels=c("F", "M", "U"), ordered=T)
d1 <- describers_idx[, c("full.name.of.describer.n", "describer.gender.n", "idxes", "idxes_author.order")][
        order(full.name.of.describer.n, describer.gender.n)][!duplicated(full.name.of.describer.n)]
d2 <- describers_idx[!dob.describer.n %in% c("U", ""), c("full.name.of.describer.n", "dob.describer.n")][
        order(full.name.of.describer.n, dob.describer.n)][!duplicated(full.name.of.describer.n)]
d3 <- describers_idx[!dod.describer.n %in% c("U", ""), c("full.name.of.describer.n", "dod.describer.n")][
        order(full.name.of.describer.n, dod.describer.n)][!duplicated(full.name.of.describer.n)]
d4 <- describers_idx[!origin.country.describer.n %in% c("U", " "), c("full.name.of.describer.n", "origin.country.describer.n")][
        order(full.name.of.describer.n, -origin.country.describer.n)][!duplicated(full.name.of.describer.n)]
d5 <- describers_idx[!residence.country.describer.n %in% c("U", " "), c("full.name.of.describer.n", "residence.country.describer.n")][
        order(full.name.of.describer.n, -residence.country.describer.n)][!duplicated(full.name.of.describer.n)]
d6 <- describers_idx[!institution.of.describer.n %in% c("U", " "), c("full.name.of.describer.n", "institution.of.describer.n")][
        order(full.name.of.describer.n, -institution.of.describer.n)][!duplicated(full.name.of.describer.n)]
describers_idx <-  merge(d1, d2, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <-  merge(describers_idx, d3, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <-  merge(describers_idx, d4, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <-  merge(describers_idx, d5, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <-  merge(describers_idx, d6, all.x=T, all.y=F, by="full.name.of.describer.n")
describers_idx <- data.table(describers_idx)
describers_idx <- describers_idx[order(full.name.of.describer.n),]

# Modify DOB
describers_idx$dob.describer.original <- describers_idx$dob.describer.n 
describers_idx$dod.describer.original <- describers_idx$dod.describer.n 
describers_idx$dob.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                   describers_idx$dob.describer.n)
describers_idx$dod.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                   describers_idx$dod.describer.n)
describers_idx$dob.describer.n <- gsub(";| ", "", describers_idx$dob.describer.n)
describers_idx$dod.describer.n <- gsub(";| ", "", describers_idx$dod.describer.n)

describers_idx$origin.country.describer.original <- describers_idx$origin.country.describer.n
describers_idx$residence.country.describer.original <- describers_idx$residence.country.describer.n
describers_idx$origin.country.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                   describers_idx$origin.country.describer.n)
describers_idx$residence.country.describer.n <- gsub("^[^\\[]]*\\]\\s*|\\[[^\\]*$", "", 
                                   describers_idx$residence.country.describer.n)
describers_idx$origin.country.describer.n <- gsub(";| ", "", describers_idx$origin.country.describer.n)
describers_idx$residence.country.describer.n <- gsub(" ", "; ", describers_idx$residence.country.describer.n)
describers_idx$residence.country.describer.n <- gsub("; $", "", describers_idx$residence.country.describer.n) # remove trailing ; 

# Check whether alive
describers_idx$alive <- "N"
describers_idx[grepl("\\[alive in 2019\\]", describers_idx$dod.describer.original)]$alive <- "Y"

# Single row modifications
describers_idx[full.name.of.describer.n=="Haroldo Toro [Guttierez]"]$dob.describer.n <- ""
describers_idx[full.name.of.describer.n=="Suzanne Willington Tubby Batra"]$dob.describer.n <- "1937"
dob = describers[full.name.of.describer.n=="Wilhelm Albert Schulz", "dod.describer.n"]
dod = describers[full.name.of.describer.n=="Wilhelm Albert Schulz", "dob.describer.n"]
describers[full.name.of.describer.n=="Wilhelm Albert Schulz", "dod.describer.n"] = dod
describers[full.name.of.describer.n=="Wilhelm Albert Schulz", "dob.describer.n"] = dob
describers[full.name.of.describer.n=="Moses Harris", "dod.describer.n"] = "1788"

#  Create index
describers_idx$idx_auth <- 1:dim(describers_idx)[1]

write.csv(describers_idx, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_3.0-by-author.csv"), na='', row.names=F, fileEncoding="UTF-8")
