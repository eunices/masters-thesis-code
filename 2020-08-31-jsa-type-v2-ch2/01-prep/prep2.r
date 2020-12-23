# Summarize offsets (publication and number of PTEs) per year
# for each biogeographic realm/ group

source('2020-08-31-jsa-type-v2-ch2/01-prep/init.r')
print(paste0(Sys.time(), " --- make dataset for offset"))



# Read data
input_filepath <- paste0(dir_model_folder, "data.csv")
data <- read_escaped_data_v2(input_filepath)



# Count offset

# By publication

if (model_params$te <= 1) {

    # Get publications
    pub <- get_pub()
    pub <- separate_rows(pub, idxes, sep = ", ")

    names(pub)[which(names(pub) == "date")] <- "year"
    pub$idx <- as.numeric(pub$idx)

    data$valid_species_id <- as.numeric(data$valid_species_id)

    df_pub <- merge(
        data[, c("valid_species_id", "group")], pub, 
        by.x = "valid_species_id", by.y = "idx", 
        all.x = T, all.y = F
    )

    ppcol_n_rm_date <- ppcol_n[!ppcol_n %in% "date"]
    df_pub <- unique(df_pub[, c("group", "year", ..ppcol_n_rm_date)])

    n_pub <- df_pub[, list(N=.N), by=c("group", "year")]

    n_pub_template <- expand.grid(
        year = min(as.numeric(n_pub$year)):max(as.numeric(n_pub$year)),
        group = unique(n_pub$group)
    )

    n_pub <- data.table(merge(
        n_pub_template,
        n_pub,
        all.x = T, all.y = T,
        by = c("year", "group")
    ))

    n_pub[is.na(n_pub)] <- 0

    n_pub <- n_pub[order(group, year)]

    n_pub <- n_pub[year <= cutoff_ch2]

    write.csv(
        n_pub, paste0(dir_model_folder, "offset.csv"), 
        row.names = F, na = "", fileEncoding = "UTF-8"
    )

}

# By number of PTEs

if (model_params$te == 2) {
    
    # Based on species, get full names of authors
    df <- separate_rows(data, species_authority, sep = "; ")

    # Get active duration of authors
    des <- get_des()
    des <- des[, c("full.name.of.describer", "min", "max_corrected")]

    # Tabulate total number of active authors for each group each year
    n_des <- merge(
        df, des,
        by.x = "species_authority", by.y = "full.name.of.describer",
        all.x = T, all.y = F
    )

    n_des <- n_des[, c("species_authority", "group", "min", "max_corrected")]
    n_des <- unique(n_des)
    n_des <- n_des[!(is.na(min) | is.na(max_corrected))]

    seq <- mapply(
        function(a, b) seq(a, b),
        a = n_des$min, 
        b = n_des$max_corrected
    )

    n_des$year <- seq
    n_des <- data.table(n_des %>% unnest(year))
    n_des$year <- as.integer(n_des$year)
    n_des$min <- NULL
    n_des$max_corrected <- NULL

    n_des <- n_des[, list(N = .N) , by = c("group", "year")]

    n_des <- n_des[year <= cutoff_ch2]

    fwrite(n_des, paste0(dir_model_folder, "offset.csv"), na = "")

}


# ggplot(n_des) + 
#     geom_line(aes(x=years, y=N_des)) +
#     facet_grid(rows = vars(group))
































