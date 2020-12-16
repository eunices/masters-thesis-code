# Purpose: clean author biodata

source('2020-08-31-jsa-type-v2/00-init/main.r')
print(paste0(Sys.time(), " ----- clean05.r"))


# Read data --------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "_5.csv")
df <- read_escaped_data_v2(file)


# Clean author biodata ---------------------------------------------------------

# Split df into authors with their info (df_d)
file <- paste0(v2_dir_data_raw_tmp, "clean05-describer.csv")
if(file.exists(file)) {

    df_d <- read_escaped_data_v2(file)

} else {

    df_d <- df[, ..dcol]
    df_d <- run_describer_split_loop_v2(df_d, strsplit_cty=" ")
    fwrite(df_d, file)

}

# Using df_d to:

# Clean dob
df_d$dob.describer.n <- gsub("[^0-9]", "", df_d$dob.describer.n)
df_d$dob.describer.n <- as.numeric(df_d$dob.describer.n)
df_d[dob.describer.n > 3000]$dob.describer.n <- NA

# Clean dod
df_d$dod.describer.n <- gsub("[^0-9]", "", df_d$dod.describer.n)
df_d$dod.describer.n <- as.numeric(df_d$dod.describer.n)
df_d[dob.describer.n > 3000]$dob.describer.n <- NA

# Clean gender
df_d[!describer.gender.n %in% c("M", "F", "U")]$describer.gender.n <- ""

# Remove digits from institution
df_d$institution.of.describer.n <- gsub("[[:digit:]]", "", 
    df_d$institution.of.describer.n
)

# Clean origin.country.describer
df_d$origin.country.describer.n <- gsub("\\[|\\]", "",
    df_d$origin.country.describer.n
)

df_d$origin.country.describer.n <- gsub("Latvia", "LG",
    gsub(";.*", "", 
        gsub(":.*", "", 
            df_d$origin.country.describer.n
)))

df_d <- merge(
    df_d, lp_country[, c("DL", "A-3")],
    by.x = "origin.country.describer.n", by.y = "DL",
    all.x = T, all.y = F
)

df_d <- merge(
    df_d, lp_dl[, c("DL", "GID_0_owner")],
    by.x = "origin.country.describer.n", by.y = "DL",
    all.x = T, all.y = F
)

df_d[!is.na(origin.country.describer.n) & 
     !is.na(GID_0_owner), ]$`A-3` <-
    df_d[!is.na(origin.country.describer.n) &
         !is.na(GID_0_owner), ]$`GID_0_owner`

df_d[is.na(`A-3`)]$origin.country.describer.n <- NA

# Clean residence.country.describer
df_d$residence.country.describer.n <- gsub(
    "\\[|\\]", "",
    df_d$residence.country.describer.n
)

df_d$residence.country.describer.n <- gsub(
    "ESP", "EP", gsub("Latvia", "LG",
    gsub(";.*", "", 
        gsub(":.*", "", 
            df_d$residence.country.describer.n
))))

df_d <- merge(
    df_d, lp_country[, c("DL", "A-3")],
    by.x = "residence.country.describer.n", by.y = "DL",
    all.x = T, all.y = F, 
    suffix = c("", "_res")
)

df_d <- merge(
    df_d, lp_dl[, c("DL", "GID_0_owner")],
    by.x = "residence.country.describer.n", by.y = "DL",
    all.x = T, all.y = F, 
    suffix =  c("", "_res")
)

df_d[!is.na(residence.country.describer.n) & 
     !is.na(GID_0_owner_res), ]$`A-3_res` <-
    df_d[!is.na(residence.country.describer.n) &
         !is.na(GID_0_owner_res), ]$`GID_0_owner_res`

df_d[is.na(`A-3`)]$residence.country.describer.n <- NA

# CHECK
table(df_d$residence.country.describer.n)
table(is.na(df_d$residence.country.describer.n))

# Add residence country back into data from df_d 
cols <- c(
    "idx", "author.order",
    "full.name.of.describer.n", 
    "residence.country.describer.n"
)
df_dres <- df_d[, ..cols][order(idx)]

df_dres[author.order=="S"]$author.order <- 2

df_dres[, len := .N, by="idx"]

df_dres[author.order=="L"]$author.order <- df_dres[author.order=="L"]$len

df_dres[!is.na(residence.country.describer.n)]$residence.country.describer.n <-
    paste0(df_dres[
        !is.na(residence.country.describer.n)
    ]$residence.country.describer.n, ":")

df_dres[is.na(residence.country.describer.n)]$residence.country.describer.n <-
    " "

df_dres <- df_dres[order(idx, as.integer(author.order))]

df_dres <- df_dres[, list(
    full.name.of.describer = 
        paste0(full.name.of.describer.n, collapse = "; "),
    residence.country.describer = 
        paste0(residence.country.describer.n, collapse = " ")
    ), 
    by = "idx"]

df <- merge(
    df, df_dres, 
    by = "idx", all.x = T, all.y = F,
    suffixes = c("", "_n")
)

df[
    !is.na(full.name.of.describer_n) & 
    full.name.of.describer_n != full.name.of.describer
]$full.name.of.describer <- df[
    !is.na(full.name.of.describer_n) & 
    full.name.of.describer_n != full.name.of.describer
]$full.name.of.describer_n

df[
    !is.na(residence.country.describer_n) & 
    residence.country.describer_n != residence.country.describer
]$residence.country.describer <- df[
    !is.na(residence.country.describer_n) & 
    residence.country.describer_n != residence.country.describer
]$residence.country.describer_n

df$residence.country.describer_n <- NULL
df$full.name.of.describer_n <- NULL


# Write data -------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "_6.csv")
fwrite(df, file)
# note: _6 has "cleaned" but "inconsistent" author information!


################################################################################

# Create describer table -------------------------------------------------------

# Order each row for each column and use row with most number of counts
df_d$describer.gender.n <- factor(
    df_d$describer.gender.n, 
    levels=c("F", "M", "U"), 
    ordered = TRUE
)


# Most common gender
d1 <- df_d[, 
    c("full.name.of.describer.n", "describer.gender.n")][,
        list(count = .N), 
        by = c("full.name.of.describer.n", "describer.gender.n")][
            order(full.name.of.describer.n, -count)][
                !duplicated(full.name.of.describer.n)]

d1$count <- NULL


# Most common dob
d2 <- df_d[!dob.describer.n %in% c("U", ""), 
    c("full.name.of.describer.n", "dob.describer.n")][,
    list(count = .N), by = c("full.name.of.describer.n", "dob.describer.n")][
        order(full.name.of.describer.n, -count)][
            !duplicated(full.name.of.describer.n)]

d2$count <- NULL


# Most common dod
d3 <- df_d[!dod.describer.n %in% c("U", ""),
    c("full.name.of.describer.n", "dod.describer.n")][,
    list(count = .N), by = c("full.name.of.describer.n", "dod.describer.n")][
        order(full.name.of.describer.n, -count)][
            !duplicated(full.name.of.describer.n)]
            
d3$count <- NULL


# Most common origin.country.describer
d4 <- df_d[!(is.na(origin.country.describer.n) | 
            origin.country.describer.n %in% c("U", " ")),
    c("full.name.of.describer.n", "origin.country.describer.n")][,
    list(count = .N), 
    by = c("full.name.of.describer.n", "origin.country.describer.n")][
        order(full.name.of.describer.n, -count)][
            !duplicated(full.name.of.describer.n)]

d4$count <- NULL


# Most common residence.country.describer
d5 <- df_d[!(is.na(residence.country.describer.n) | 
            residence.country.describer.n %in% c("U", " ")),
    c("full.name.of.describer.n", "residence.country.describer.n")][,
        list(count = .N), 
        by = c("full.name.of.describer.n", "residence.country.describer.n")][
            order(full.name.of.describer.n, -count)][
                !duplicated(full.name.of.describer.n)]
                
d5$count <- NULL

# Most common institution.of.describer
d6 <- df_d[!institution.of.describer.n %in% c("U", " "), 
    c("full.name.of.describer.n", "institution.of.describer.n")][,
        list(count = .N), 
        by = c("full.name.of.describer.n", "institution.of.describer.n")][
            order(full.name.of.describer.n, -count)][
                !duplicated(full.name.of.describer.n)]

d6$count <- NULL

df_ds <-  merge(d1, d2, all.x=T, all.y=F, by="full.name.of.describer.n")
df_ds <-  merge(df_ds, d3, all.x=T, all.y=F, by="full.name.of.describer.n")
df_ds <-  merge(df_ds, d4, all.x=T, all.y=F, by="full.name.of.describer.n")
df_ds <-  merge(df_ds, d5, all.x=T, all.y=F, by="full.name.of.describer.n")
df_ds <-  merge(df_ds, d6, all.x=T, all.y=F, by="full.name.of.describer.n")
df_ds <- data.table(df_ds)[order(full.name.of.describer.n)]

# Add info from v1 data (lp-auth-bio.csv)
file <- paste0(v2_dir_data_raw_clean, "lp-auth-bio-v1.csv")
lp_auth_b <- read_escaped_data_v2(file)
names(lp_auth_b)

authors <- unique(df_ds$full.name.of.describer.n)
columns <- unique(names(df_ds))

for(c in 2:length(columns)) { # exclude first  column
    col <- columns[c] 

    if (!col %in% c("alive")) {
        print(paste0("**********", col))
        for (a in 1:length(authors)) {
            auth <- authors[a]
            
            old <- lp_auth_b[full.name.of.describer.n_v2 == auth][[col]]
            new <- df_ds[full.name.of.describer.n == auth][[col]]

            if(is.na(new)) {
                if(!is.na(old)) {
                    print(paste0(old, " replaces ", new))
                    df_ds[full.name.of.describer.n == auth,][[col]] <- old
                }
            }

        }

    }

}

df_ds <- merge(
    df_ds, lp_auth_b[, c("full.name.of.describer.n_v2", "alive")],
    by.x = "full.name.of.describer.n", by.y = "full.name.of.describer.n_v2",
    all.x = TRUE, all.y = FALSE
)


# Incorporate manual information
cfile <- paste0(v2_dir_data_raw_clean, "clean05-auth-biodata_edit.csv")

if(file.exists(cfile)) {

    df_ds_edit <- read_escaped_data_v2(cfile)

    authors <- unique(c_df_ds$full.name.of.describer.n)
    columns <- unique(names(df_ds))

    for(c in 2:length(columns)) { # each column, excluding 1st
        
        col <- columns[c]

        if (!col %in% c("idxes", "idxes_author.order", "alive")) {
            print(paste0("********** COLUMN: ", col))

            for (a in 1:length(authors)) { # each author
                
                auth <- authors[a]
                old <- df_ds[full.name.of.describer.n == auth][[col]]
                new <- df_ds_edit[full.name.of.describer.n == auth][[col]]

                # replace new value with old
                if(!is.na(new)) { # if new is not blank
                    if(is.na(old) | new != old) { # if new and old not same
                        # print(paste0(new, " replaces ", old))
                        df_ds[full.name.of.describer.n == auth][[col]] <- old
                    }

                }

            }

        }

    }

}


# Add info manually if missing still
c_df_ds <-  df_ds[!complete.cases(df_ds), ]

cfile <- paste0(v2_dir_data_raw_clean, "clean05-auth-biodata.csv")
fwrite(c_df_ds, cfile)


names(df_ds) <- gsub("\\.n$", "", names(df_ds))


# Write describer data ---------------------------------------------------------
file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_1.csv")
fwrite(df_ds, file)

