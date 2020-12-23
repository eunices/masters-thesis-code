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


# Tag duplicates ---------------------------------------------------------------
# note: this can only be done after the date (clean04.r) and
# author names (clean02.r) are completed

df$status <- factor(df$status, species_status)

# Tag duplicated species based on unique combination
df$duplicated <- duplicated(
    df[, c("genus", "species", "author", "date", "status")]
)

# Check for non-obvious duplicates

# Get unique combinations of duplicated genus and species
dups <- unique(
    df[duplicated(paste0(genus, " ", species)), c("genus", "species")]
)

# Subset for dups which are synonyms or valid species
dups <- 
    df[
        status %in% c("Synonym", "Valid species") &
        duplicated == FALSE & # excludes those already flagged as T
        paste0(genus, " ", species) %in% paste0(dups$genus, " ", dups$species)
    ]

# Extract date
dups$date <- gsub("\\[[^\\]]*\\]", "", dups$date, perl = TRUE)

# CHECK
dups <- 
    dups[,
         list(n = .N, 
              idxes = paste0(idx, collapse = ", "),
              status = paste0(sort(unique(status)), collapse = "; "),
              date = paste0(sort(unique(date)), collapse = "; "),
              author = paste0(sort(unique(author)), collapse = "; "),
              n_date = length(unique(date)),
              n_author = length(unique(author)),
              file = paste0(sort(unique(file)), collapse = "; ")),
         by = c("genus", "species")][n >= 2]

cfile <- paste0(v2_dir_data_raw_check, "33-34-species-dups.csv")
fwrite(dups, cfile)
# note: no action taken except  for "Valid species; Synonym"

# Duplicates with **valid species and synonym" 
# and no semi-colon in author/date (i.e. 1 author or 1 year),
# use the first as valid as they are likely to be typos

dups_valid <- separate_rows(
    dups[
        (n_date == 1 | n_author == 1) &
        status %in% c("Valid species; Synonym"), 
        c("idxes")],
    idxes, sep = ", "
)

cols <- c("idx", "genus", "species", "status")
dups_valid <- df[idx %in% dups_valid$idxes, ..cols][
    order(genus, species, status)
]

dups_valid$duplicated <- duplicated(
    dups_valid[, c("genus", "species")] 
) # take first duplicated, with valid species taking precedence

df <- merge(
    df, dups_valid[, c("idx", "duplicated")],
    all.x = TRUE, all.y = FALSE,
    by.x = "idx", by.y = "idx",
    suffixes = c("", "_n")
)

df[!is.na(duplicated_n)]$duplicated <- df[!is.na(duplicated_n)]$duplicated_n
df$duplicated_n <- NULL

# CHECK
table(df$duplicated)


# Duplicated subspecies --------------------------------------------------------

# Get unique list of duplicated subspecies
dups <- unique(df[
    grepl("subspecies", status) &
    duplicated(valid.genus.species.subspecies)
]$valid.genus.species.subspecies)

# CHECK
cfile <- paste0(v2_dir_data_raw_check, "35-36-subspecies-dups.csv")
fwrite(
    df[grepl("subspecies", status) & valid.genus.species.subspecies %in% dups][
    , c(
        "file", ..bcol, ..pcol, "valid_subspecies",
        "valid.genus.species.subspecies", "duplicated"
        )
    ][order(genus, species, valid_subspecies),],
    
    cfile    
)


# Write data -------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "_6.csv")
fwrite(df, file)