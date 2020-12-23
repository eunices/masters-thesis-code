# Purpose: clean dates / journal names / publication

source('2020-08-31-jsa-type-v2/00-init/main.r')
print(paste0(Sys.time(), " ----- clean04.r"))

# Read data --------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
df <- read_escaped_data_v2(file)


# Clean journal names ----------------------------------------------------------

cfile <- paste0(v2_dir_data_raw_clean, "clean04-journal_edit.csv")

if(file.exists(cfile)) {

    df <- update_data_with_edits(cfile, df, "idxes")
    
    df_j_edit <- read_escaped_data_v2(cfile)

    df_j_edit <- df_j_edit[!(journal_edit == "" & is.na(journal_edit))]

    df_j_edit <- separate_rows(df_j_edit, idxes, sep = ", ")

    df_j_edit <- rename_names(df_j_edit, "idxes", "idx")

    df <- replace_edits(df_j_edit, df)

}

df_j <- df[, c("idx", "journal")]

df_j <- df_j[, list(idxes = paste0(idx, collapse = ", ")), by = "journal"]

df_j <- df_j[order(journal)]

cfile <- paste0(v2_dir_data_raw_clean, "clean04-journal.csv")
fwrite(df_j, cfile)


# Clean journal associated info ------------------------------------------------

cfile <- paste0(v2_dir_data_raw_clean, "clean04-journal-info_edit.csv")
if(file.exists(cfile)) df <- update_data_with_edits(cfile, df, "idxes")

df_j <- df[, c("idx", ..jcol)]

df_j <- df_j[, list(idxes = paste0(idx, collapse = ", ")), by = jcol]

df_j <- df_j[order(journal)]


cfile <- paste0(v2_dir_data_raw_clean, "clean04-journal-info.csv")
fwrite(df_j[journal %in% df_j[duplicated(journal)]$journal], cfile) 
# note: check only duplicated journals,
# assuming that the prior check was cleaned properly


# Clean publications -----------------------------------------------------------

cfile <- paste0(v2_dir_data_raw_clean, "clean04-pubs_edit.csv")
if(file.exists(cfile)) {
    
    df_pubs_edit <- read_escaped_data_v2(cfile)

    df_pubs_edit[,  
        names(df_pubs_edit) := lapply(.SD, function(x) gsub("^'", "", x))
    ] 

    df_pubs_edit <- separate_rows(df_pubs_edit, idxes, sep = ", ")

    df_pubs_edit <- rename_names(df_pubs_edit, "idxes", "idx")

    df <- replace_edits(df_pubs_edit, df)

}

df$paper.authors_n <- ifelse(
    is.na(df$paper.editors) | df$paper.editors == "NA",
    df$paper.authors,
    gsub("In\\[i\\] ", "", df$paper.editors)
)

cols <- ppcol[!ppcol %in% c("paper.authors", "paper.editors")]

cols <- c(
    cols[1:2], "paper.authors_n", 
    cols[3:length(cols)]
)

df_pub <- df[, c("idx", ..cols)]

df_pub <- df[, list(idxes = paste0(idx, collapse = ", ")), by = cols]

df_pub[,  names(df_pub) := lapply(.SD, function(x) paste0("'", x))] 

df_pub <- df_pub[order(journal, title, volume, issue)]

cfile <- paste0(v2_dir_data_raw_clean, "clean04-pubs.csv")
fwrite(df_pub, cfile)

df$paper.type_n <- ifelse(
    df$paper.type == "J", "J", "B"
)

# Write data -------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "_6.csv")
fwrite(df, file)