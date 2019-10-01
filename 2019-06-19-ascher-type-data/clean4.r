# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - splitting dataset
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- splitting dataset"))

filepath <- paste0(dir_data, '2019-05-23-Apoidea world consensus file Sorted by name 2019-idx-3-clean-fields.csv')
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

df_s <- df[!(idx %in% 1:20669)]
df <- df[idx %in% 1:20669]

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean fields specific to invalid species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean fields specific to invalid species"))

# fread dataset

# duplicates rows
gs <- paste0(df_s$genus, " ", df_s$subgenus, " ", df_s$species, " ", 
             df_s$author.date)
df_s$duplicated.row <- duplicated(gs)
df_s <- df_s[duplicated.row == "FALSE"][order(as.numeric(idx))]

# clean genus and species relationships
filepath <- paste0(dir_data, "clean/idx-idx_original.csv")
idxdf <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
idxdf[, names(idxdf) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

idxdf$correct_synonym <- gsub('=', '', idxdf$taxonomic_notes)
idxdf[status=="Valid subspecies"]$correct_synonym <- gsub("([A-Za-z]+).*", "\\1", idxdf[status=="Valid subspecies"]$correct_synonym)
idxdf$taxonomic_notes <- NULL

# max of idx that is green for each row
idxdf$idx_original <- as.numeric(idxdf$idx_original)
idxdf$idx <- as.numeric(idxdf$idx)

idx_y <- idxdf[colour!="green" | is.na(colour)]
idx_g <- idxdf[colour=="green"]
idx2 <- idx_g[idx_y, on = .(idx_original), roll = Inf, rollends=c(T, T)]
idx2 <- idx2[, c("i.idx", "genus", "species")]
names(idx2) <- c("idx", "genus_new", "correct_synonym")

df_s$idx <- as.numeric(df_s$idx)
idx2$idx <- as.numeric(idx2$idx)
df_s <- merge(df_s, idx2, by='idx', all.x=T, all.y=F)

df_s[genus != genus_new]$genus <- df_s[genus != genus_new]$genus_new
df_s$genus_new <- NULL
df_s$taxonomicnotes.subspecies.synonyms.etc <- NULL

write.csv(df_s[order(as.numeric(idx))], 
          paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_1-clean.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - clean fields specific to valid species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- clean fields specific to valid species"))

# duplicates rows
gs <- paste0(df$genus, df$species)
df$duplicated.row <- duplicated(gs)
df <- df[!duplicated.row == TRUE]

# summarizing synonyms
sum <- df_s[, list(N = .N), by=c("genus", "correct_synonym", "status")]
var_count <- dcast(sum, genus + correct_synonym ~ status, value.var="N")
names(var_count) <- c("genus", "species", "N_var", "N_synonyms", "N_ss")
var_count[is.na(var_count)] <- 0
df <- merge(df, var_count, by=c("genus", "species"), all.x=T, all.y=F)
df <- df[order(as.numeric(idx))]

write.csv(df[order(as.numeric(idx))], 
          paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_1-clean.csv"), na='', row.names=F, fileEncoding="UTF-8")
    