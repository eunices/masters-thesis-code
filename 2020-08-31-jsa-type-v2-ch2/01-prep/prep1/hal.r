source('2020-08-31-jsa-type-v2-ch2/01-prep/prep1/util.r')

df <- get_df()
df <- df[tolower(status) == "valid species"]

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
