# Purpose: clean dates / journal names / publication

source('2020-08-31-jsa-type-v2/init/init.r')

file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
df <- read_escaped_data_v2(file)

# Clean dates -------------------------------------------------------------

# Manually
# TODO: incorporate "date_edit", "date.of.type_edit"

cfile <- paste0(v2_dir_data_raw_clean, "clean04-check-date-lag_edit.csv")

if(file.exists(cfile)) {
    df_dates <- read_escaped_data_v2(cfile)
    
}


# Description dates
df[date<1700]$date <- NA

# Collection dates
df[date.of.type.yyyy<1500]$date.of.type.yyyy <- NA

# Lag between dates
cols <- unique(
    c(bcol, "date", "date.of.type", "date.of.type.yyyy", "date.lag"), 
    fromLast = TRUE
)

cfile <- paste0(v2_dir_data_raw_clean, "clean04-check-date-lag.csv")
fwrite(df[date.of.type.yyyy > date, ..cols], cfile)


# Clean journal names ----------------------------------------------------------

cfile <- paste0(v2_dir_data_raw_clean, "clean04-journal_edit.csv")

if(file.exists(cfile)) {
    
    df_j_edit <- read_escaped_data_v2(cfile)
    
    df_j_edit <- df_j_edit[!(journal_edit == "" & is.na(journal_edit))]
    
    df_j_edit <- separate_rows(df_j_edit, idxes, sep = ", ")
    
    df[idx %in% df_j_edit$idxes]$journal <- 
        df_j_edit[match(df[idx %in% df_j_edit$idxes]$idx, idxes)]$journal_edit

}

df_j <- df[, c("idx", "journal")]

df_j <- df_j[, list(
    idxes = paste0(idx, collapse = ", ")
), by = "journal"]

df_j <- df_j[order(journal)]

cfile <- paste0(v2_dir_data_raw_clean, "clean04-journal.csv")
fwrite(df_j, cfile)


# Clean journal associated info ------------------------------------------------

cfile <- paste0(v2_dir_data_raw_clean, "clean04-journal-info_edit.csv")

if(file.exists(cfile)) {
    
    df_j_edit <- read_escaped_data_v2(cfile)
    
    df_j_edit <- df_j_edit[country.of.publication_edit != ""]
    
    df_j_edit <- separate_rows(df_j_edit, idxes, sep = ", ")
    
    df[idx %in% df_j_edit$idxes]$country.of.publication <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$country.of.publication_edit
    
    df[idx %in% df_j_edit$idxes]$city.of.publication <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$city.of.publication_edit

    df[idx %in% df_j_edit$idxes]$paper.type <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$paper.type_edit

}

cols <- c("idx", jcol)
df_j <- df[, ..cols]

df_j <- df_j[, list(
    idxes = paste0(idx, collapse = ", ")
), by = jcol]

df_j <- df_j[order(journal)]

cfile <- paste0(v2_dir_data_raw_clean, "clean04-journal-info.csv")
fwrite(df_j, cfile)


# Clean publications -----------------------------------------------------------

cfile <- paste0(v2_dir_data_raw_clean, "clean04-pubs_edit.csv")

if(file.exists(cfile)) {
    
    df_pubs_edit <- read_escaped_data_v2(cfile)

    df_pubs_edit <- separate_rows(df_j_edit, idxes, sep = ", ")

    df_pubs_edit[,  
        names(df_pubs_edit) := lapply(.SD, function(x) gsub("'", "", x))
    ] 
    
    # Add that data back into df
    df[idx %in% df_j_edit$idxes]$paper.authors <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$paper.authors
    
    df[idx %in% df_j_edit$idxes]$paper.editors <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$paper.editors

    df[idx %in% df_j_edit$idxes]$title <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$title

    df[idx %in% df_j_edit$idxes]$journal <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$journal

    df[idx %in% df_j_edit$idxes]$volume <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$volume

    df[idx %in% df_j_edit$idxes]$issue <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$issue

    df[idx %in% df_j_edit$idxes]$page.numbers.publication <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$page.numbers.publication

    df[idx %in% df_j_edit$idxes]$page.numbers.publication <- 
        df_j_edit[
            match(df[idx %in% df_j_edit$idxes]$idx, idxes)
        ]$paper.type

}

cols <- c("idx", ppcol)
df_pub <- df[, ..cols]

df_pub <- df[, 
    list(idxes = paste0(idx, collapse = ", ")),
    by = ppcol
]

df_pub[,  names(df_pub) := lapply(.SD, function(x) paste0("'", x))] 


df_pub <- df_pub[order(journal, title, volume, issue)]

cfile <- paste0(v2_dir_data_raw_clean, "clean04-pubs.csv")
fwrite(df_pub, cfile)




file <- paste0(v2_dir_data_raw, v2_basefile, "_6.csv")
fwrite(df, file)