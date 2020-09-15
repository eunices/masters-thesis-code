source('2020-08-31-jsa-type-v2/init/init.r')


get_df <- function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
    read_escaped_data_v2(file)
}

get_pub <- function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
    df <- read_escaped_data_v2(file)
    df <- df[duplicated == FALSE &
             status %in% c("Valid species", "Synonym"), 
             list(idxes = paste0(idx, collapse = ", ")),
             by = ppcol]
}


get_des <- function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_2.csv")
    read_escaped_data_v2(file)
}


get_dis <- function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "-distribution.csv")
    read_escaped_data_v2(file)
}

get_raw_data = function() {
    file <- paste0(dir_data_raw, basefile, ".csv")
    read_escaped_data_v2(file)
}


get_n_active_describers_by_year = function() {
    file <- paste0(dir_data_raw, basefile, "-describer-effort.csv")
    read_escaped_data_v2(file)
}

get_describer_network = function() {
    file <- paste0(dir_data_raw, basefile, "-describer-network.csv")
    read_escaped_data_v2(file)
}