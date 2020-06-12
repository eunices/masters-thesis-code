# Information about code:
# This code corresponds to data wrangling code for my MSc thesis.
# This code is for creating an enhanced denormalised describer-species dataset.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

source('2019-06-19-jsa-type/df/functions.R')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - further denormalization of data with cleaned describer data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- 'describers': further denorm. w/ clean describer data"))



# Read data
describers <- read_escaped_data(paste0(dir_data, basefile, " describers_3.0-by-author.csv"))



# Denormalization
describers <- describers %>% separate_rows(idxes, idxes_author.order)




# Joining with dates
dfx1 <- read_escaped_data(paste0(dir_data, basefile, " filtered_4.3-clean-coll.csv"))
dfx2 <- read_escaped_data(paste0(dir_data, basefile, " oth_4.3-clean-coll.csv"))

# Combine data with relevant columns
cols = c("idx", "date.n")
dfx <- rbind(dfx1[, ..cols], dfx2[, ..cols])
dfx <- dfx[!duplicated(idx)]
dfx$idx <- as.numeric(dfx$idx)
describers$idxes <- as.numeric(describers$idxes)

# Merge
describers <- merge(describers, dfx, by.x="idxes", by.y="idx", all.x=T, all.y=F)





# Write data
filename_write = paste0(dir_data, basefile, " describers_4.0-denormalised2.csv")
write.csv(describers, filename_write, na='', row.names=F, fileEncoding="UTF-8")
