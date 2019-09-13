print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting df3.r"))
print("######################################################")
print("######################################################")
print("######################################################")

source('2019-06-19-ascher-type-data/init.r')

# Libraries
#############

# NONE

# Parameters
#############

# loop_3 <- "Y"
loop_3 <- "N"

# Scripts
#############

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - create collector  raw dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- collector raw dataset"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_2-clean.csv")
df_s <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df_s[, names(df_s) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_2-clean.csv")
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

collector_cols <- c("idx", "collector.of.type", "full.name.of.collector",
          "title.of.collector", "collector.gender", "info.about.collector")

collectors_info <- rbind(df[,..collector_cols],  df_s[,..collector_cols])

print(paste0("There are ", table(is.na(collectors_info$collector.of.type))[2], " idxes with no collector info."))

write.csv(collectors_info[order(full.name.of.collector)], 
          paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_1.0-all.csv"), na='', row.names=F, fileEncoding="UTF-8")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - denormalise collector dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- denormalise collector dataset"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_1.0-all.csv")
collectors_info <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
collectors_info[, names(collectors_info) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

collectors_unique <- data.table(collectors_info %>%
  group_by(collector.of.type, full.name.of.collector,
          title.of.collector, collector.gender, info.about.collector) %>%
  summarise(idxes=paste0(idx,collapse='; ')))

# Split authors by ;
split_comma <- function(x) {
    strsplit(x, split=", ")
}
split_semicolon <- function(x) {
    strsplit(x, split="; ")
}
split_gender <- function(x) {
    strsplit(x, split="")
}

collectors_unique$collector.of.type.n <- lapply(collectors_unique$collector.of.type, split_comma)
collectors_unique$full.name.of.collector.n <- lapply(collectors_unique$full.name.of.collector, split_semicolon)
collectors_unique$collector.gender.n <- lapply(collectors_unique$collector.gender,
                                            split_gender)
collectors_unique$title.of.collector.n <- lapply(collectors_unique$title.of.collector,
                                        split_semicolon)
collectors_unique$info.about.collector.n <- lapply(collectors_unique$info.about.collector,
                                        split_semicolon)

run_loop <- function() {
    
    # Create a new row for each author
    collectors <- data.frame(idxes=character(),
                            collector.of.type.n=character(),
                            full.name.of.collector.n=character(),
                            collector.gender.n=character(),
                            title.of.collector.n=character(),
                            info.about.collector.n=character()
                            )

    n_rows = dim(collectors_unique)[1]
    for (i in 1:n_rows) {
    # for (i in 1:2) {
        idx_row <- collectors_unique[i]$idxes
        collector_row <- collectors_unique[i]$collector.of.type.n[[1]][[1]]
        gender_row <- collectors_unique[i]$collector.gender.n[[1]][[1]]
        fname_row <- collectors_unique[i]$full.name.of.collector.n[[1]][[1]]
        title_row <- collectors_unique[i]$title.of.collector.n[[1]][[1]]
        info_row <- collectors_unique[i]$info.about.collector[[1]][[1]]

        if(!identical(collector_row, character(0))){
            for (j in 1:length(collector_row)) {
                if(is.na(collector_row[j])) {
                    to_merge <- data.frame(idxes=idx_row,
                            collector.of.type.n=NA,
                            full.name.of.collector.n=NA,
                            collector.gender.n=NA,
                            title.of.collector.n=NA,
                            info.about.collector.n=NA)
                } else {

                    collector <- ifelse(is.na(collector_row[j]) || identical(collector_row[j], logical(0)) , NA, collector_row[j])
                    gender <- ifelse(is.na(gender_row[j]) || identical(gender_row[j], logical(0)) , NA, gender_row[j])
                    fname <- ifelse(is.na(fname_row[j]) || identical(fname_row[j], logical(0)) , NA, fname_row[j])
                    title <- ifelse(is.na(title_row[j]) || identical(title_row[j], logical(0)) , NA, title_row[j])
                    info <- ifelse(is.na(info_row[j]) || identical(info_row[j], logical(0)) , NA, info_row[j])

                    to_merge <- data.frame(idxes=idx_row,
                            collector.of.type.n=collector,
                            full.name.of.collector.n=fname,
                            collector.gender.n=gender,
                            title.of.collector.n=title,
                            info.about.collector.n=info)
                    collectors <- rbind(collectors, to_merge)
                }
            }
        } else {
            to_merge <- data.frame(idxes=idx_row,
                            collector.of.type.n=NA,
                            full.name.of.collector.n=NA,
                            collector.gender.n=NA,
                            title.of.collector.n=NA,
                            info.about.collector.n=NA)
            collectors <- rbind(collectors, to_merge)
        }
        percent <- round(i/n_rows*100, 2)
        if(percent %% 25 == 0) {
            print(paste0(percent , "% completed"))
        }
    }
    collectors

}

collectors_unique[200]$idxes
collectors_unique[200]$collector.of.type.n[[1]][[1]]
collectors_unique[200]$full.name.of.collector.n[[1]][[1]]
collectors_unique[200]$collector.gender.n[[1]][[1]]
collectors_unique[200]$info.about.collector.n[[1]][[1]]

if (loop_3 == "Y") {
    collectors <- run_loop()
    collectors <- data.table(collectors %>% separate_rows(idxes))
    collectors <- data.table(collectors %>%
    group_by(collector.of.type.n, full.name.of.collector.n,
          title.of.collector.n, collector.gender.n, info.about.collector.n) %>%
  summarise(idxes=paste0(idxes,collapse='; ')))
#   summarise(idxes=paste0(idxes,collapse='; ')))
    collectors$idxes <- gsub("; $", "", collectors$idxes)
    
    write.csv(collectors[order(full.name.of.collector.n)], 
          paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_2.0-denormalised.csv"), na='', row.names=F, fileEncoding="UTF-8")

}


# these excludes rows with no collector information

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean collector dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean collector dataset"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_2.0-denormalised_edit4.csv")
collectors <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
collectors[, names(collectors) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

collectors <- collectors %>% separate_rows(collector.of.type.n, sep="; ")
collectors <- data.table(collectors %>% separate_rows(idxes, sep="; "))
table(collectors$uncertain=="")
table(collectors$uncertain=="y")

# # Iterative cleaning
# collectors_group <- data.table(collectors %>% 
#     group_by(uncertain, collector.of.type.n,
#                  full.name.of.collector.n,
#                  title.of.collector.n,
#                  collector.gender.n,
#                  info.about.collector.n) %>%
#     summarise(idxes=paste0(idxes,collapse='; '), 
#               N=n()))

# write.csv(collectors_group, paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_2.0-denormalised_edit4.csv"), na='', row.names=F, fileEncoding="UTF-8")

# cleaned for persons who have collected > 100 type specimens


collectors_grouped <- data.table(collectors %>% 
    group_by(uncertain, collector.of.type.n,
                 full.name.of.collector.n,
                 title.of.collector.n,
                 collector.gender.n,
                 info.about.collector.n) %>%
    summarise(idxes=paste0(idxes,collapse='; '),
              N=n()))

# collectors_grouped1 <- data.table(collectors[uncertain=="", ] %>% 
#     group_by(collector.of.type.n,
#                  full.name.of.collector.n,
#                  title.of.collector.n,
#                  collector.gender.n,
#                  info.about.collector.n) %>%
#     summarise(idxes=paste0(idxes,collapse='; '),
#               N=n()))

# Exclude uncertain; approximately 10% of data
collectors_grouped2 <- data.table(collectors[uncertain=="y", ] %>% 
    group_by(collector.of.type.n,
                 full.name.of.collector.n,
                 title.of.collector.n,
                 collector.gender.n,
                 info.about.collector.n) %>%
    summarise(idxes=paste0(idxes,collapse='; '),
              N=n()))
print(paste0("There are ", sum(collectors_grouped2$N), " uncertain rows."))

# collectors_grouped <- merge(collectors_grouped1, collectors_grouped2, 
#       by=c("collector.of.type.n",
#                  "full.name.of.collector.n",
#                  "title.of.collector.n",
#                  "collector.gender.n",
#                  "info.about.collector.n"), all.x=T, all.y=T, suffixes=c("_certain", "_uncertain"))


write.csv(collectors_grouped[order(uncertain, -N, full.name.of.collector.n)], paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_3.0-collectors.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - merge back into main dataframe
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean collector dataset"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_3.0-collectors.csv")
collectors <- fread(filepath, integer64='character', na.strings=c('NA'), encoding='UTF-8')
collectors[, names(collectors) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

collectors <- data.table(collectors %>% separate_rows(idxes))
collectors <- collectors[, c("collector.of.type.n", "full.name.of.collector.n", "uncertain", "collector.gender.n", "title.of.collector.n", "idxes")]
collectors2 <- collectors[order(uncertain)] %>% 
    group_by(idxes) %>%
    summarise(
        collector.of.type.n=paste0(collector.of.type.n,collapse='; '),
        full.name.of.collector.n=paste0(full.name.of.collector.n,collapse='; '),
        uncertain=paste0(uncertain,collapse='; '),
        collector.gender.n=paste0(collector.gender.n, collapse='; '),
        title.of.collector.n=paste0(title.of.collector.n, collapse='; '),
    )
table(duplicated(collectors2$idxes))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.2-clean-auth-full-name.csv")
dfx1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx1[, names(dfx1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.2-clean-auth-full-name.csv")
dfx2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfx2[, names(dfx2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

if(any(names(dfx1) %in% c('collector.of.type.n_new', 'full.name.of.collector.n_new', 'uncertain_new', 'collector.gender.n_new', 'title.of.collector.n_new'))) {
    dfx1$collector.of.type.n_new <- NULL
    dfx1$full.name.of.collector.n_new <- NULL
    dfx1$uncertain_new <- NULL
    dfx1$collector.gender.n_new <- NULL
    dfx1$title.of.collector.n_new <- NULL
    dfx1 <- merge(dfx1, collectors2, all.x=T, all.y=F, by.x='idx', by.y='idxes',
              suffixes=c("", "_new"))
}

table(is.na(dfx1$collector.of.type.n))
table(is.na(dfx1$full.name.of.collector.n))
table(is.na(dfx1$uncertain))
table(is.na(dfx1$collector.gender.n))
table(is.na(dfx1$title.of.collector.n))

# logic in having df3.r here is a little odd, as cleaning is cyclical
# if collector-variable_new exist in the dataframe, then do not run this code
if(any(names(dfx2) %in% c('collector.of.type.n_new', 'full.name.of.collector.n_new', 'uncertain_new', 'collector.gender.n_new', 'title.of.collector.n_new'))) {
    dfx2$collector.of.type.n_new <- NULL
    dfx2$full.name.of.collector.n_new <- NULL
    dfx2$uncertain_new <- NULL
    dfx2$collector.gender.n_new <- NULL
    dfx2$title.of.collector.n_new <- NULL
    dfx2 <- merge(dfx2, collectors2, all.x=T, all.y=F, by.x='idx', by.y='idxes',
              suffixes=c("", "_new"))
}

table(is.na(dfx2$collector.of.type.n))
table(is.na(dfx2$full.name.of.collector.n))
table(is.na(dfx2$uncertain))
table(is.na(dfx2$collector.gender.n))
table(is.na(dfx2$title.of.collector.n))

write.csv(dfx1, 
        paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-col.csv"), na='', row.names=F, fileEncoding="UTF-8")

write.csv(dfx2, 
        paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.3-clean-coll.csv"), na='', row.names=F, fileEncoding="UTF-8")

# note to self. may not be necessary
