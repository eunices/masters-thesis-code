source(paste0(dir_script_ed, '/01-prep/prep2/util.r'))

get_yearly_describers <- function(data) {

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
    n_des
}