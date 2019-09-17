source('2019-06-19-ascher-type-data/init.r')

# Libraries
library(xlsx)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Load clean data frames (useful columns only)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Load data.frame variable names
var <- data.table(read.xlsx2(paste0(dir, '2019-09-16-metadata.xlsx'), sheetIndex=1, stringsAsFactors=F), as.data.frame=T)
names(var)
var$order <- as.integer(var$order)
tn <- unique(var$table_name); print(tn)
var2 <- var[!grepl("remove|derived", key_status)]
var_df1 <- var2[table_name=="species"][order(order)]; dim(var_df1)
var_df2 <- var2[table_name=="invalid_species"][order(order)]; dim(var_df2)
var_pub <- var2[table_name=="publication"][order(order)]; dim(var_pub)
var_des <- var2[table_name=="describer"][order(order)]; dim(var_des)
var_col <- var2[table_name=="collector"][order(order)]; dim(var_col)

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
        "dob.describer", "dod.describer", "origin.country.describer", "residence.country.describer", "institution.of.describer")

c_pub_i <- c( "paper.authors", "title", "journal", "volume", "issue", 
            "page.numbers.publication", 
            "paper.type", "country.of.publication", "city.of.publication")

# Load dataframes
fn_df1 <- "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-coll.csv"
# fn_df2 <- "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.3-clean-coll.csv"

filepath <- function(filename) {paste0(dir, filename)}
df1 <- fread(filepath(fn_df1), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')

classes <- sapply(df1, class)
df1 <- as.data.table(lapply(df1, function(x) { if(is.character(x)) gsub('\\"\\"', '\\"', x) else x }))
cols <- c(var_df1$column)
df1 <- df1[, ..cols]

write.csv(df1[order(idx)], paste0(dir, "final/SPECIES.csv"), na='', row.names=F, fileEncoding="UTF-8")


# What the various flag means for geocoding
no_ll <- c("UNKNOWN_LOCALITY", "NO_COUNTRY", "COUNTRY_ONLY", "COUNTRY_ONLY_UNCERTAIN_STATE")
# no lat and lon
coarse_ll <- c("COUNTRY_AND_PRI_DIV_ONLY_LRG_DIV", "COUNTRY_AND_PRI_DIV_ONLY_SML_DIV")
# coarse lat and lon; lrg div lat lon is probably unreliable
geocoded_ll <- c("GEOCODED_GOOGLE_MAPS_API",
                 "LOCALITY_MANUALLY_CHECKED_AMENDED_LOCALITY_GEOCODED_AGAIN")
# geocoded by google maps; however may be erronoeous or not so accurate (only to be used in analyses of 1 deg but country reliable)
manual_ll <- c("LOCALITY_MANUALLY_CHECKED_LAT_LONG_ADDED",
               "MAY_2019_DATASET_LAT_LON_ERRONEOUS_ADDED_MANUALLY")
# most reliable as manually added using various sources like geohack, google maps
ignore_ll_mismatch <- c("IGNORE_COUNTRY_DISCREPANCY_ERRONEOUS_GADM_BOUNDARY")
# use country field as GADM boundaries are wrong

