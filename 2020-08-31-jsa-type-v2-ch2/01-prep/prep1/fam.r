source(paste0(dir_script_ed, '/01-prep/prep1/util.r'))

# Read data
df <- get_species_raw_data()

join <- df[, c("idx", "full.name.of.describer", "date", "family")][
    order(as.numeric(idx))
]

# Log relevant statistics
write_ending_log(join, filepath_log)

# Format data and write as data.csv
format_data(join, dir_model_folder)
