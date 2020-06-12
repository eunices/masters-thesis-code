# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for wrangling collector-specific fields.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting df2.r"))
print("######################################################")
print("######################################################")
print("######################################################")

source('2019-06-19-jsa-type/init/init.R')
source('2019-06-19-jsa-type/clean/functions.R')


# Libraries
#############

# NONE

# Parameters
#############

loop_2 <- "Y"
# loop_2 <- "N"

# Scripts
#############


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - create collector  raw dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- collector raw dataset"))




# Read data
df = read_escaped_data(paste0(dir_data, basefile, " filtered_1-clean.csv"))
df_s = read_escaped_data(paste0(dir_data, basefile, " oth_1-clean.csv"))




# Subset data
collector_cols <- c("idx",
                    "collector.of.type",
                    "full.name.of.collector",
                    "title.of.collector",
                    "collector.gender",
                    "info.about.collector")

collectors_info <- rbind(df[, ..collector_cols],  df_s[,..collector_cols])

print(paste0("There are ", table(is.na(collectors_info$collector.of.type))[2],
             " idxes with no collector info."))




# Write data
write.csv(collectors_info[order(full.name.of.collector)],
          paste0(dir_data, basefile, " collectors_1.0-all.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - denormalise collector dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- denormalise collector dataset"))



# Read data
collectors_info = read_escaped_data(paste0(dir_data, basefile, " collectors_1.0-all.csv"))




# Get unique collaborators

# Combine by idx
collectors_unique <- collectors_info[, list(idxes=paste0(idx, collapse="; ")), 
                                     by=c("collector.of.type",
                                          "full.name.of.collector",
                                          "title.of.collector",
                                          "collector.gender", 
                                          "info.about.collector")]

# Split authors by ;"
collectors_unique$collector.of.type.n <- 
    lapply(collectors_unique$collector.of.type, strsplit, split=", ")

collectors_unique$full.name.of.collector.n <- 
    lapply(collectors_unique$full.name.of.collector, strsplit, split="; ")

collectors_unique$collector.gender.n <- 
    lapply(collectors_unique$collector.gender, strsplit, split="")

collectors_unique$title.of.collector.n <- 
    lapply(collectors_unique$title.of.collector, strsplit, split="; ")

collectors_unique$info.about.collector.n <- 
    lapply(collectors_unique$info.about.collector, strsplit, split="; ")


if (loop_2 == "Y") {

    # Separate the dataset
    collectors <- run_collectors_loop(collectors_unique)

    # Separate by species id
    collectors <- unique(data.table(collectors %>% separate_rows(idxes)))

    # Summarise species by authors
    collectors <- collectors[, list(idxes=paste0(idxes, collapse="; ")), 
                             by=c("collector.of.type.n",
                                  "full.name.of.collector.n",
                                  "title.of.collector.n",
                                  "collector.gender.n", 
                                  "info.about.collector.n")]

    # Remove any trailing ";"
    collectors$idxes <- gsub("; $", "", collectors$idxes)
    
    # Write file
    write.csv(collectors[order(full.name.of.collector.n)], 
              paste0(dir_data, basefile, " collectors_2.0-denormalised.csv"),
              na='', row.names=F, fileEncoding="UTF-8")

}

# these excludes rows with no collector information





# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean collector dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean collector dataset"))





# Read data (this data is edited)
collectors <- 
    read_escaped_data(paste0(dir_data, basefile, " collectors_2.0-denormalised_edit4.csv"))



# Get collector species rows
collectors <- collectors %>% separate_rows(collector.of.type.n, sep="; ")
collectors <- data.table(collectors %>% separate_rows(idxes, sep="; "))
print(paste0("Number of uncertain species-author rows: ", sum(collectors$uncertain=="y")))
print(paste0("Number of certain species-author rows: ", sum(collectors$uncertain=="")))




# # Iterative cleaning
# cleaned for persons who have collected > 100 type specimens
# collectors_group <- collectors[, list(idxes=paste0(idxes, collapse="; "), N=.N), 
#                                by=c("uncertain",
#                                     "collector.of.type.n",
#                                     "full.name.of.collector.n",
#                                     "title.of.collector.n", 
#                                     "collector.gender.n", 
#                                     "info.about.collector.n")][order(-N)]
#
# write.csv(collectors_group, 
#           paste0(dir_data, basefile, " collectors_2.0-denormalised_edit4.csv"),
#           na='', row.names=F, fileEncoding="UTF-8")




# Group for all
collectors_grouped <- 
    collectors[, 
               list(idxes=paste0(idxes, collapse="; "), N=.N),
               by=c("uncertain",
                    "collector.of.type.n",
                    "full.name.of.collector.n",
                    "title.of.collector.n",
                    "collector.gender.n",
                    "info.about.collector.n")]

# Group by uncertain
collectors_grouped2 <- 
    collectors[uncertain=="y", 
               list(idxes=paste0(idxes, collapse="; "), N=.N),
               by=c("uncertain",
                    "collector.of.type.n",
                    "full.name.of.collector.n",
                    "title.of.collector.n",
                    "collector.gender.n",
                    "info.about.collector.n")]
       
print(paste0("There are ", sum(collectors_grouped2$N), " uncertain rows."))
# Exclude uncertain; approximately 10% of data



# Write data
write.csv(collectors_grouped[order(uncertain, -N, full.name.of.collector.n)], 
          paste0(dir_data, basefile, " collectors_3.0-collectors.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")





# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - merge back into main dataframe
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- merge back into main dataframe"))



# Read data
collectors = read_escaped_data(paste0(dir_data, basefile, " collectors_3.0-collectors.csv"))

# Separate by idxes
collectors <- data.table(collectors %>% separate_rows(idxes))

# Subset columns
collectors <- collectors[, c("collector.of.type.n",
                             "full.name.of.collector.n",
                             "uncertain", 
                             "collector.gender.n",
                             "title.of.collector.n",
                             "idxes")]

# Summarise by species
collectors2 <- 
    collectors[order(uncertain), 
               list(collector.of.type.n=paste0(collector.of.type.n, collapse='; '),
                    full.name.of.collector.n=paste0(full.name.of.collector.n, collapse='; '),
                    uncertain=paste0(uncertain, collapse='; '),
                    collector.gender.n=paste0(collector.gender.n, collapse='; '),
                    title.of.collector.n=paste0(title.of.collector.n, collapse='; ')), 
               by="idxes"]




# Merge back data for dfx1

# Read data
dfx1 <- read_escaped_data(paste0(dir_data, basefile, " filtered_4.2-clean-auth-full-name.csv"))

# Remove irrelevant columns
irrelevant_columns = c('collector.of.type.n_new',
                       'full.name.of.collector.n_new', 
                       'uncertain_new',
                       'collector.gender.n_new',
                       'title.of.collector.n_new')

if(any(names(dfx1) %in% irrelevant_columns)) {
    dfx1$collector.of.type.n_new <- NULL
    dfx1$full.name.of.collector.n_new <- NULL
    dfx1$uncertain_new <- NULL
    dfx1$collector.gender.n_new <- NULL
    dfx1$title.of.collector.n_new <- NULL
}

# Merge data back
dfx1 <- merge(dfx1, collectors2, all.x=T, all.y=F, by.x='idx', by.y='idxes',
              suffixes=c("", "_new"))

# !CHECK if any is NA
table(is.na(dfx1$collector.of.type.n))
table(is.na(dfx1$full.name.of.collector.n))
table(is.na(dfx1$uncertain))
table(is.na(dfx1$collector.gender.n))
table(is.na(dfx1$title.of.collector.n))




# Do the same for dfx2 - merge back data

# Read data
dfx2 <- read_escaped_data(paste0(dir_data, basefile, " oth_4.2-clean-auth-full-name.csv"))

# logic in having df3.r here is a little odd, as cleaning is cyclical
# if collector-variable_new exist in the dataframe, then do not run this code
if(any(names(dfx2) %in% irrelevant_columns)) {
    dfx2$collector.of.type.n_new <- NULL
    dfx2$full.name.of.collector.n_new <- NULL
    dfx2$uncertain_new <- NULL
    dfx2$collector.gender.n_new <- NULL
    dfx2$title.of.collector.n_new <- NULL
}

# Merge data back
dfx2 <- merge(dfx2, collectors2, all.x=T, all.y=F, by.x='idx', by.y='idxes',
              suffixes=c("", "_new"))

# !CHECK if any is NA
table(is.na(dfx2$collector.of.type.n))
table(is.na(dfx2$full.name.of.collector.n))
table(is.na(dfx2$uncertain))
table(is.na(dfx2$collector.gender.n))
table(is.na(dfx2$title.of.collector.n))




# Write data
write.csv(dfx1, paste0(dir_data, basefile, " filtered_4.3-clean-coll.csv"),
          na='', row.names=F, fileEncoding="UTF-8")

write.csv(dfx2, paste0(dir_data, basefile, " oth_4.3-clean-coll.csv"),
          na='', row.names=F, fileEncoding="UTF-8")
