source('2020-08-31-jsa-type-v2/00-init/main.r')
print(paste0(Sys.time(), " ----- format.r"))

filename <- paste0(v2_basefile, ".csv")

if (filename %in% list.files(v2_dir_data_raw, "*.\\.csv")) {
    file <- paste0(v2_dir_data_raw, filename)
}

df <- read_escaped_data_v2(file)

df[] <- lapply(df, gsub, pattern='[\r\n]', replacement=' ')

names(df) <- format_names(names(df))

output <- paste0(v2_dir_data_raw, v2_basefile, "_1.csv")
df <- df[, 1:110]
fwrite(df, output, na = "")
