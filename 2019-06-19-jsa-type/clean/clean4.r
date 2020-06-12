# Information about code:
# This code corresponds to cleaning code for my MSc thesis.
# A series of other codes are named as clean1|2|3|4|5|6.r
# Cleans genus, species; splits synonyms from valid species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

print("")
print(paste0(Sys.time(), " --- starting clean4.r"))
print("######################################################")

source('2019-06-19-jsa-type/clean/functions.R')


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - splitting dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- splitting dataset"))




# Read data
df = read_escaped_data(paste0(dir_data_raw, basefile, '-idx-3-clean-fields.csv'))




# Split the dataset for valid species and synonyms
df_s <- df[!(idx %in% 1:20669)]
df <- df[idx %in% 1:20669]




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean fields specific to invalid species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean fields specific to invalid species"))




# Duplicate rows
gs <- paste0(df_s$genus, " ", df_s$subgenus, " ", df_s$species, " ", 
             df_s$author.date)
df_s$duplicated.row <- duplicated(gs)
df_s <- df_s[duplicated.row == "FALSE"][order(as.numeric(idx))]




# Clean genus and species relationships
idxdf <- read_escaped_data(paste0(dir_data_raw_clean, "idx-idx_original.csv"))
# copy and pasted from Excel for colour
idxdf$correct_synonym <- gsub('=', '', idxdf$taxonomic_notes)
idxdf[status=="Valid subspecies"]$correct_synonym <- 
    gsub("([A-Za-z]+).*", "\\1", idxdf[status=="Valid subspecies"]$correct_synonym)
idxdf$taxonomic_notes <- NULL

# Idea here is to add the correct synonym genus/species name
# Get max of idx that is green for each row, and correct the species name
idxdf$idx_original <- as.numeric(idxdf$idx_original)
idxdf$idx <- as.numeric(idxdf$idx)
idx_y <- idxdf[colour!="green" | is.na(colour)]
idx_g <- idxdf[colour=="green"]
idx2 <- idx_g[idx_y, on = .(idx_original), roll = Inf, rollends=c(T, T)]
idx2 <- idx2[, c("i.idx", "genus", "species")]
names(idx2) <- c("idx", "genus_new", "correct_synonym")

# Merge data back in
df_s$idx <- as.numeric(df_s$idx)
idx2$idx <- as.numeric(idx2$idx)
df_s <- merge(df_s, idx2, by='idx', all.x=T, all.y=F)
df_s[genus != genus_new]$genus <- df_s[genus != genus_new]$genus_new
# Remove irrelevant columns
df_s$genus_new <- NULL
df_s$taxonomicnotes.subspecies.synonyms.etc <- NULL




# Write data
write.csv(df_s[order(as.numeric(idx))], paste0(dir_data_raw, basefile, " oth_1-clean.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean fields specific to valid species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean fields specific to valid species"))





# Make label duplicated.row
gs <- paste0(df$genus, df$species)
df$duplicated.row <- duplicated(gs)
# Remove duplicated rows
df <- df[!duplicated.row == TRUE]




# Calculate metrics summarised for synonyms
sum <- df_s[, list(N = .N), by=c("genus", "correct_synonym", "status")]
var_count <- dcast(sum, genus + correct_synonym ~ status, value.var="N")
names(var_count) <- c("genus", "species", "N_var", "N_synonyms", "N_ss")
var_count[is.na(var_count)] <- 0
df <- merge(df, var_count, by=c("genus", "species"), all.x=T, all.y=F)
df <- df[order(as.numeric(idx))]



# Write data
write.csv(df[order(as.numeric(idx))], paste0(dir_data_raw, basefile, " filtered_1-clean.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")
    