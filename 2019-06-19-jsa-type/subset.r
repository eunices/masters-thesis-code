# Information about code:
# This code corresponds to getting the various datasets for my MSc thesis.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Setup
source('2019-06-19-jsa-type/init/var.R')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Load clean data frames (useful columns only)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Variables
# to_subset <- "Y"
to_subset <- "N"

# Load data.frame variable names
var <- data.table(read.xlsx2(paste0(dir_data, '2019-09-16-metadata.xlsx'), 
                             sheetIndex=1, stringsAsFactors=F), as.data.frame=T)
names(var)
var$order <- as.integer(var$order)
var_f <- var[!grepl("remove", key_status)]
# var_f <- var[!grepl("remove|derived", key_status)]


# Subset useful columns
c_main <- c('idx', 'genus', 'species', 'status', 
           'author', 'full.name.of.describer', 
           'original.genus', 'original.subgenus', 'original.epithet', 
           'date', 'date.n') # essential info

c_loc <- c('type.locality.verbatim', 'type.locality.updated',
          'lat', 'lon', 'source.of.latlon', 
          'type.country', 'type.state', 'elev.m')

c_coll_i <- c('date.of.type', 'date.of.type.string', 'date.of.type.dd',
              'date.of.type.mm', 'date.of.type.yyyy',
              'collector.of.type', 'title.of.collector', 'full.name.of.collector',
              'collector.gender', 'info.about.collector')

c_tax <- c('family', 'subfamily', 'subgenus', 'subtribe', 'tribe') 

c_nat_hist <- c('type.sex', 'host.plant.of.type', 'host.plant', 'nest.substrate',
                'host.insect.or.prey', 'sociality', 'pollen.transport')

c_oth <- c("flag", "duplicated.row")

c_coll_i_n <- c('date.of.type', 'date.of.type.string', 'date.of.type.dd',
                'date.of.type.mm', 'date.of.type.yyyy',
                'collector.of.type.n', 'title.of.collector.n', 'full.name.of.collector.n',
                'collector.gender.n', 'info.about.collector.n', 'uncertain')

c_des_i <- c("author", "full.name.of.describer", "describer.gender", 
             "dob.describer", "dod.describer", "origin.country.describer",
             "residence.country.describer", "institution.of.describer")

c_pub_i <- c( "paper.authors", "title", "journal", "volume", "issue", 
             "page.numbers.publication", 
             "paper.type", "country.of.publication", "city.of.publication")

# Load dataframes
construct_filepath <- function(filename) {paste0(dir_data, filename)}
subset_df <- function(filepath, table, write=T) {

    # fn_df2 <- "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.3-clean-coll.csv"
    print(paste0("Reading ", table, " from ", filepath))

    df <- fread(construct_filepath(filepath), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')
    vars <- var_f[table_name==table][order(order)]

    classes <- sapply(df, class)
    df <- as.data.table(lapply(df, function(x) { 
        if(is.character(x)) {
            gsub("[\r\n]", " ", gsub('\\"\\"', '\\"', x))
        } else {x}
    }))
    cols <- c(vars$column)
    df <- df[, ..cols]

    if (write == T) {
        w_filepath <- paste0('final/', toupper(table), ".csv")
        if('idx' %in% names(df)) {
            df <- df[order(idx)]
        }
        write.csv(df, construct_filepath(w_filepath), na='', row.names=F, fileEncoding="UTF-8")
        print(paste0(Sys.time(), " --- data written to ", construct_filepath(w_filepath)))
    } else {
        df
    }
}

tn <- unique(var$table_name)

get_df1 <- function(write=T) {
    fn_df1 <- paste0(basefile, " filtered_4.3-clean-coll.csv")
    subset_df(fn_df1, "species", write=write)
}

get_df2 <- function(write=T) {
    fn_df2 <- paste0(basefile, " oth_4.3-clean-coll.csv")
    subset_df(fn_df2, "invalid_species", write=write)
}

get_pub <- function(write=T) {
    fn_pub <- paste0(basefile, " pub_1.0-clean.csv")
    subset_df(fn_pub, "publication", write=write)
}

get_des <- function(write=T) {
    fn_des <- paste0(basefile, " describers_5.0-describers-final.csv")
    subset_df(fn_des, "describer", write=write)
}

get_col <- function(write=T) {
    fn_col <- paste0(basefile, " collectors_3.0-collectors.csv")
    subset_df(fn_col, "collector", write=write)
}

get_dis <- function(write=T) {
    fn_dis <- paste0(basefile, " filtered_5-species-cty2-cty.csv")
    subset_df(fn_dis, "distribution", write=write)
}

write_datasets <- function() {
    get_df1()
    get_df2()
    get_pub()
    get_des()
    get_col()
    get_dis()
}

if (to_subset=="Y") {
    write_datasets()
}

# TEST: 
# df <- subset_df(fn_df2, "invalid_species", write=F)
# df[idx==27576]

########################################################

# Non "subset_df" datasets
# These are not in the ERD, but are derived datasets nonetheless used

get_n_active_describers_by_year = function() {
    file = " describers_6.0-active-by-year.csv"
    filepath_describers_by_year <- paste0(dir_data, basefile, file)
     fread(filepath_describers_by_year, 
           integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
}

get_species_country_distribution = function() {
    file = ' filtered_5-species-cty2-cty.csv'
    filepath_input_regions <- paste0(dir_data, basefile, file)
    fread(filepath_input_regions, na=c(''), encoding='UTF-8')
}

get_describer_network = function() {
    file = " describers_7.0-author-networks.csv"
    nw = fread(paste0(dir_data, basefile, file),
               integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
    nw[, names(nw) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
    # fread does not escape double quotes
    nw
}

get_species_denormalised = function() {
    filepath <- ' describers_4.0-denormalised2.csv'
    dat <- fread(paste0(dir_data, basefile, filepath), encoding="UTF-8", 
                 stringsAsFactors=F, na=c(""))
    dat[, names(dat) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
    # fread does not escape double quotes
    dat
}

########################################################

# Lookup files
get_lp_statoid = function() {
    fread('data/lookup/2019-05-29-statoid-country-codes.csv', na=c(''), encoding='UTF-8')
}

get_lp_pop = function() {
    folder = "data/2020-05-05-population-growth/"
    filepath = paste0(folder, "WorldPopulationAnnual12000years_interpolated_HYDEandUNto2015.csv")
    lp_pop = fread(filepath)
    names(lp_pop) <- c("year", "pop")
    lp_pop
}

get_lp_biome = function() {
    fread('data/lookup/2019-10-14-biome-broad-cat.csv', na=c(''), encoding='UTF-8')
}

get_lp_col = function() {
    fread('data/2019-10-30-colonial-history/coldata110_edit.csv',  encoding="UTF-8")
}

get_lp_adj_countries = function() {
    fread('data/lookup/2019-11-01-country-adjacent_edit.csv',  encoding="UTF-8") 
}

########################################################

# Shapefiles filepath

get_shp_biogeo = function() {
    filepath_input_biogeo = 'data/geo_processed/teow/official/wwf_terr_ecos_dissolved.shp'
    st_read(filepath_input_biogeo, quiet=T)
}

get_shp_biomes = function() {
    filepath_input_biomes = 'data/geo/0_manual/Ecoregions2017/Ecoregions2017.shp'
    st_read(filepath_input_biomes, quiet=T)

}



########################################################


# What the various flag means for geocoding:

no_ll <- c("UNKNOWN_LOCALITY", "NO_COUNTRY", "COUNTRY_ONLY", "COUNTRY_ONLY_UNCERTAIN_STATE")
# no lat and lon

coarse_ll <- c("COUNTRY_AND_PRI_DIV_ONLY_LRG_DIV", "COUNTRY_AND_PRI_DIV_ONLY_SML_DIV")
# coarse lat and lon; lrg div lat lon is probably unreliable

geocoded_ll <- c("GEOCODED_GOOGLE_MAPS_API",
                 "LOCALITY_MANUALLY_CHECKED_AMENDED_LOCALITY_GEOCODED_AGAIN")
# geocoded by google maps; however may be erronoeous or not so accurate
# (only to be used in analyses of 1 deg but country reliable)

manual_ll <- c("LOCALITY_MANUALLY_CHECKED_LAT_LONG_ADDED",
               "MAY_2019_DATASET_LAT_LON_ERRONEOUS_ADDED_MANUALLY")
# most reliable as manually added using various sources like geohack, google maps

ignore_ll_mismatch <- c("IGNORE_COUNTRY_DISCREPANCY_ERRONEOUS_GADM_BOUNDARY")
# use country field as GADM boundaries are wrong

