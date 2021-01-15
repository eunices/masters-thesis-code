source(paste0(dir_script_ed, '/01-prep/prep1/util.r'))

df <- get_species_raw_data()

join <- df[, c("idx", "full.name.of.describer", "date", "genus")][
    order(as.numeric(idx))]

top10 <- c(
    "andrena", "lasioglossum", "megachile", 
    "bombus", "hylaeus", "nomada", "coelioxys",
    "anthophora", "colletes", "perdita"
)

join <- join[tolower(genus) %in% top10]

# Log relevant statistics
write_ending_log(join, filepath_log)

# Format data and write as data.csv
format_data(join, dir_model_folder)
