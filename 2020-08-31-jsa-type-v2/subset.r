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

# Lookup files

get_lp_statoid = function() {
    fread(
        'data/lookup/2019-05-29-statoid-country-codes.csv', 
        na = c(''), encoding = 'UTF-8'
    )
}

get_lp_biome = function() {
    fread(
        'data/lookup/2019-10-14-biome-broad-cat.csv', 
        na = c(''), encoding = 'UTF-8'
    )
}

# Shapefiles


get_shp_biogeo = function() {
    filepath_input_biogeo = 
        'data/geo_processed/teow/official/wwf_terr_ecos_dissolved.shp'
    st_read(filepath_input_biogeo, quiet=T)
}

get_shp_biomes = function() {
    filepath_input_biomes = 
        'data/geo/0_manual/Ecoregions2017/Ecoregions2017.shp'
    st_read(filepath_input_biomes, quiet=T)
}