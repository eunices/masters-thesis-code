# Variables
cutoff <- 2019
data_version <- "v3"                    # data version
v2_basefile <- '2020-11-10-Apoidea'     # data date (correspond w/ version)
v2_dir_ref <- '2020-08-31-jsa-type-v2/' # script folder

# Data
v2_data <- "data/"
v2_lookup <- paste0(v2_data, "lookup/")
v2_dir_data <- paste0(v2_data, "2019-05-23-ascher-bee-data/")

# Output data
v2_dir_data_analysis <- paste0(v2_dir_data, "analysis/", data_version, "/")
v2_dir_data_webapp <- paste0(v2_dir_data_analysis, "web-app/")
v2_dir_data_ch1 <- paste0(v2_dir_data_analysis, 'ch1/')
v2_dir_data_ch2 <- paste0(v2_dir_data_analysis, 'ch2/')
v2_dir_data_ch3_coauth <- paste0(v2_dir_data_analysis, 'ch3-coauth/')
v2_dir_data_ch3_flow <- paste0(v2_dir_data_analysis, 'ch3-flow/')
v2_dir_data_ch3_gender <- paste0(v2_dir_data_analysis, 'ch3-gender/')

v2_dir_data_webapp_img <- paste0(v2_dir_data_webapp, "/img/")

# Raw data
v2_dir_data_raw <- paste0(v2_dir_data, data_version, "/")
v2_dir_data_raw_raw <- paste0(v2_dir_data_raw, "raw/")
v2_dir_data_raw_clean <- paste0(v2_dir_data_raw, "clean/")
v2_dir_data_raw_check <- paste0(v2_dir_data_raw, "check/")
v2_dir_data_raw_tmp <- paste0(v2_dir_data_raw, "tmp/")


# Script folder
v2_dir_ref_str <- gsub("/", "", v2_dir_ref)
v2_dir_shiny <- paste0(v2_dir_ref_str, '-shiny/')
v2_dir_ch1 <- paste0(v2_dir_ref_str, '-ch1/')
v2_dir_ch2 <- paste0(v2_dir_ref_str, '-ch2/')
v2_dir_ch3a <- paste0(v2_dir_ref_str, '-ch3-coauth/')
v2_dir_ch3b <- paste0(v2_dir_ref_str, '-ch3-flow/')
v2_dir_ch3c <- paste0(v2_dir_ref_str, '-ch3-gender/')



# Other 
source('keys.R')
source(paste0(v2_dir_ref, '00-init/util.r'))
source(paste0(v2_dir_ref, '00-init/libraries.r'))

# Initialize google api for geocoding
register_google(key = geocode_api)

# If data dir does not exist, create it
data_dirs <- c(
    v2_dir_data, v2_dir_data_analysis, 
    v2_dir_data_webapp, v2_dir_data_webapp_img, v2_dir_data_ch1,
    v2_dir_data_ch2, v2_dir_data_ch3_coauth, v2_dir_data_ch3_flow, 
    v2_dir_data_ch3_gender,
    v2_dir_data_raw, v2_dir_data_raw_raw, v2_dir_data_raw_clean,
    v2_dir_data_raw_check, v2_dir_data_raw_tmp, 
    
    v2_dir_shiny
)

lapply(data_dirs, function(folder) {
  if(!dir.exists(folder)) dir.create(folder)
})

# Columns for quick ref
bcol <- c("idx", "genus", "species", "date", "author", "status")

jcol <- c("journal", "country.of.publication",
           "city.of.publication", "paper.type")

pcol <- c("title", "journal")

ppcol <- c("date", "paper.authors", "paper.editors",
           "title", "journal", "volume", "issue",
           "page.numbers", "paper.type")

ppcol_n <- c("date", "paper.authors_n",
             "title", "journal", "volume", "issue",
             "paper.type_n")

dcol <- c("idx", "author", "full.name.of.describer", 
          "describer.gender", "dob.describer",
          "dod.describer", "origin.country.describer",
          "residence.country.describer", "institution.of.describer")


# Factors

species_status <- c(
    "Valid species", "Synonym", "Valid subspecies", "Nominate subspecies",
    "Morphospecies"
)

# Read lookup files
f_lp_country <- paste0(v2_lookup, "2019-05-29-statoid-country-codes.csv")
lp_country <- fread(f_lp_country, na.strings = "")

f_lp_dl <- paste0(v2_lookup, "2019-09-26-location-codes.csv")
lp_dl <- fread(f_lp_dl, na.strings = "")


# Geospatial layers
wgs84 <- 4326

v2_data_geo <- "data/geo/"
v2_data_geo_m <- paste0(v2_data_geo, "0_manual/")
v2_data_geo_s <- paste0(v2_data_geo, "1_separate/")

v2_data_geo_p <- "data/geo_processed/"

# Read geospatial layers
if(!exists("v_ecoregions")) {
  print("Reading v_ecoregions")
  f_v_ecoregions <- paste0(v2_data_geo_m, "Ecoregions2017/Ecoregions2017.shp")
  v_ecoregions <- st_read(f_v_ecoregions, quiet = TRUE)
}

if(!exists("v_continent")) {
  print("Reading v_continent")
  f_v_continent <- paste0(v2_data_geo_p, "gadm/gadm36_0_utf8_continents.shp")
  v_continent <- st_read(f_v_continent, quiet = TRUE)
}

