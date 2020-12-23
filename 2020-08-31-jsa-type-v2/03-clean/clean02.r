# Purpose: clean author names

source('2020-08-31-jsa-type-v2/00-init/main.r')
print(paste0(Sys.time(), " ----- clean02.r"))

# Read file --------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "_3.csv")
df <- read_escaped_data_v2(file)

df$author <- trimws(df$author)


# Incorporate manual edits -----------------------------------------------------


# Incorporate changes by replacing author rows comma delimiting authors with ";"

# Create this file (no outputs)
cfile <- paste0(v2_dir_data_raw_clean, "clean02-comma_edit.csv")
if(file.exists(cfile)) {
    clean_commas <- read_escaped_data_v2(cfile)

    clean_commas <- separate_rows(clean_commas, idxes, sep = ", ")

    df[idx %in% clean_commas$idxes]$full.name.of.describer <-
        gsub(
            ", ", "; ", 
            df[idx %in% clean_commas$idxes]$full.name.of.describer
        )

}


# Incorporate manual changes of full name of describer

# Create this file (no outputs)
cfile <- paste0(v2_dir_data_raw_clean, "clean02-manual_edit.csv")
if(file.exists(cfile)) {

    clean_manual <- read_escaped_data_v2(cfile)

    clean_manual <- separate_rows(clean_manual, idxes, sep = ", ")

    df[match(clean_manual$idxes, idx)]$full.name.of.describer <- 
        clean_manual$full.name.of.describer

}


# Standardize full author names ------------------------------------------------

# Incorporate full name edits
# 3 columns: full.name.of.describer,
# full.name.of.describer_edit, idxes
cfile <- paste0(v2_dir_data_raw_clean, "clean02-check-auth_edit.csv")
if(file.exists(cfile)) {

    df_auth <- read_escaped_data_v2(cfile)

    # exclude these
    df_auth <- df_auth[
        !(
            grepl("\\[CHECK", full.name.of.describer_edit) | 
            is.na(full.name.of.describer_edit)
         )
    ]

    for (i in 1:dim(df_auth)[1]) { # for each author

        # filter for relevant idxes with authors
        auth_idxes <- unlist(lapply(
            strsplit(df_auth$idxes[i], split = ", "), as.integer
        ))

        # replace each full name with the correct name
        original_name <- ifelse(
            grepl("\\[", df_auth$full.name.of.describer[i]),
            
            gsub("\\]", "\\\\]",
                gsub("\\[", "\\\\[", df_auth$full.name.of.describer[i])
            ),
            
            df_auth$full.name.of.describer[i]
        )

        # modified name        
        modified_name <- df_auth$full.name.of.describer_edit[i]

        check <- df$idx %in% auth_idxes
        df[check]$full.name.of.describer <- gsub(
            original_name, modified_name,
            df[check]$full.name.of.describer 
        )

    }

}

df_auth <- df[, c("idx", "full.name.of.describer")]

df_auth <- separate_rows(df_auth, full.name.of.describer, sep = "; ")

df_auth <- df_auth[, 
    list(idxes = paste0(idx, collapse = ", "), .N), 
    by = "full.name.of.describer"
][order(full.name.of.describer, -N)]

cfile <- paste0(v2_dir_data_raw_clean, "clean02-check-auth.csv")
fwrite(df_auth, cfile)

# Create author lookup file called "lp-surname.csv" from this


# Full name consistent with author ---------------------------------------------

file <- paste0(v2_dir_data_raw_clean, "lp-surname.csv")
lp_surname <- read_escaped_data_v2(file)

# Incorporate changes from df_auth into df
# author_edit and full.name.of.describer_edit
cfile <- paste0(v2_dir_data_raw_clean, "clean02-check-short-auth_edit.csv")
if(file.exists(cfile)) df <- update_data_with_edits(cfile, df)

df_auth <- df[, c("idx", "full.name.of.describer")]

df_auth <- df_auth[order(idx)]

df_auth <- separate_rows(df_auth, full.name.of.describer, sep = "; ")

df_auth <- data.table(df_auth)

df_auth[, order := seq_len(.N), by=c("idx")]

cols <- c("full.name.of.describer", "last.name", "last.name.no.initials")
df_auth <- merge(
    df_auth,
    lp_surname[, ..cols], 
    by = "full.name.of.describer",
    all.x = T, all.y = F
)

df_auth <- df_auth[order(idx, order)]

df_auth[, len := .N, by="idx"]

df_auth$last_auth <- ""
df_auth[len == order & len != 1,]$last_auth <- "L"

# CHECK: varying author lengths
df[idx %in% unique(df_auth[len == 4]$idx), ..bcol]
df[idx %in% unique(df_auth[len == 3]$idx), ..bcol]
df[idx %in% unique(df_auth[len == 2]$idx), ..bcol]

# Combine the surname of authors based on different lengths

df_author1 <- df_auth[len == 1, c("idx", "last.name", "last.name.no.initials")]

df_author2 <- df_auth[len == 2][order(idx, order)]

df_author2 <- df_author2[,
    list(
        idx = idx,
        last.name = paste0(last.name, collapse = " and "),
        last.name.no.initials = paste0(last.name.no.initials, 
                                       collapse = " and ")
    ), by = idx
][,c("idx", "last.name", "last.name.no.initials")]

df_author3 <- df_auth[len >= 3][order(idx, order)]

df_author3[last_auth == "L"]$last.name <-  
    paste0("and ", df_author3[last_auth == "L"]$last.name)

df_author3[last_auth == "L"]$last.name.no.initials <-  
    paste0("and ", df_author3[last_auth == "L"]$last.name.no.initials)

df_author3 <- df_author3[,
    list(
        idx = idx,
        last.name = paste0(last.name, collapse = ", "),
        last.name.no.initials = paste0(last.name.no.initials, 
                                       collapse = ", ")
    ), by = idx
][,c("idx", "last.name", "last.name.no.initials")]

df_author <- rbindlist(list(df_author1, df_author2, df_author3))

df_author <- df_author[order(idx)]
names(df_author) <- c("idx", "author_check", "author_check_no_ini")

cols <- unique(c("idx", "author", "full.name.of.describer", bcol, pcol))
df_auth <- merge(
    df[, ..cols], df_author,
    all.x = T, all.y = F, by = "idx"
)

df_auth$check1 <- df_auth$author == df_auth$author_check
df_auth$check2 <- df_auth$author == df_auth$author_check_no_ini
df_auth$check <- df_auth$check1 | df_auth$check2

cfile <- paste0(v2_dir_data_raw_clean, "clean02-check-short-auth.csv")
fwrite(df_auth[check == FALSE][order(author)], cfile)


# Write data -------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "_4.csv")
fwrite(df, file)