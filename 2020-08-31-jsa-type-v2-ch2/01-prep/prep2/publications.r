source(paste0(dir_script_ed, '/01-prep/prep2/util.r'))

get_yearly_publications <- function(data) {

    pub <- get_pub()
    pub <- separate_rows(pub, idxes, sep = ", ")
    names(pub)[which(names(pub) == "idxes")] <- "idx"

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

    n_pub <- df_pub[
        !is.na(year) | year != "NA", 
        list(N=.N), 
        by=c("group", "year")
    ]

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
    n_pub
}