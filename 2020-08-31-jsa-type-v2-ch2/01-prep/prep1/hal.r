source(paste0(dir_script_ed, '/01-prep/prep1/util.r'))

df <- get_species_raw_data()

join <- df[, c("idx", "full.name.of.describer", "date", "genus")][
    order(as.numeric(idx))
]

halictidae <- c("lasioglossum", "lipotriches", "sphecodes",
                "patellapis", "halictus", "dufourea", "augochloropsis",
                "nomia", "augochlora", "neocorynura")

join <- join[tolower(genus) %in% halictidae]


# Log relevant statistics
write_ending_log(join, filepath_log)

# Format data and write as data.csv
format_data(join, dir_model_folder)
