source('2020-08-31-jsa-type-v2/00-init/main.r')


get_df <- function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "_8.csv")
    read_escaped_data_v2(file)
}

get_pub <- function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "_8.csv")
    df <- read_escaped_data_v2(file)
    df <- df[
        duplicated == FALSE &
        status %in% c("Valid species", "Synonym"), 
        list(idxes = paste0(idx, collapse = ", ")),
        by = ppcol_n
    ]
    df
}


get_des <- function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "-describer_2.csv")
    df <- read_escaped_data_v2(file)
    df[, 12:49] <- lapply(df[,12:49], as.numeric)
    df
}


get_dis <- function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "-distribution.csv")
    read_escaped_data_v2(file)
}

get_raw_data = function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, ".csv")
    read_escaped_data_v2(file)
}


get_n_active_describers_by_year = function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "-describer-effort.csv")
    df <- read_escaped_data_v2(file)
    df[] <- lapply(df, as.numeric)
    df
}

get_describer_network = function() {
    file <- paste0(v2_dir_data_raw, v2_basefile, "-describer-network.csv")
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

get_lp_pop = function() {
    folder = "data/2020-05-05-population-growth/"
    filepath = paste0(folder, "WorldPopulationAnnual12000years_interpolated_HYDEandUNto2015.csv")
    lp_pop = fread(filepath)
    names(lp_pop) <- c("year", "pop")
    lp_pop
}

get_lp_col = function() {
    folder = "data/2019-10-30-colonial-history/"
    filepath = paste0(folder, "coldata110_edit.csv")
    lp_col = fread(filepath)
    lp_col
}

get_lp_adj_countries = function() {
    folder = "data/lookup/"
    filepath = paste0(folder, '2019-11-01-country-adjacent_edit.csv')
    lp_adj_countries = fread(filepath,  encoding="UTF-8") 
    lp_adj_countries
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

