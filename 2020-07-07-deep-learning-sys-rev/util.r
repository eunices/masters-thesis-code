source("2020-07-07-deep-learning-sys-rev/libraries.r")

get_files_in_folder <- function(dir, year) {
    year <- 2019
    files <- list.files(dir, pattern = paste0(year))
    files <- paste0(dir, files)
    files
}

read_files <- function(files, encoding) {
    # note: chinese characters could not be read properly
    li <- lapply(files, fread, encoding = encoding, stringsAsFactors = FALSE)
    df <- rbindlist(li, fill = TRUE)
    # escape ""
    df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
    df[!duplicated(df)]
}

format_scopus_authors = function(authors) {

    # reformat authors
    authors <- gsub(",", ";", authors)
    authors <- gsub("\\.", "", authors)

    string <- strsplit(authors, "; ")[[1]]
    string <- unlist(lapply(string, clean_author))
    
    paste0(string, collapse = "; ")

}

clean_author = function(word) {

    # after first word add ", "
    words <- strsplit(word, " ")[[1]]
    surname <- words[1]
    first_name <- gsub("-", "",words[2:length(words)])
    words <- paste0(surname, ", ", first_name)
    
    words
}