source('2019-07-15-edie-et-al/init_a.r')

# TODO: ecoregions shp file
# TODO: estimate compute power 

# libraries
library(sf)

# filepaths
filepath_input_regions <- 'data/2019-05-23-ascher-bee-data/2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty2-cty.csv'

# read lookup files
filepath_input_biogeo <- 'data/geo_processed/teow/official/wwf_terr_ecos_dissolved.shp'
filepath_input_biomes <- 'data/geo/0_manual/Ecoregions2017/Ecoregions2017_dissolved.shp'
lookup_cty <- fread('data/lookup/2019-05-29-statoid-country-codes.csv', na=c(''), encoding='UTF-8')

# split dataset according to different parameters as format.csv
cols_std <- c("idx", "full.name.of.describer", "date.n")
cols_ll  <- c("idx", "lat", "lon",
              "type.country.n", "type.state.n",
              "type.country.n.full", "type.state.n.full",
              "type.country.n", "type.state.n",
              "type.locality.verbatim", "type.locality.updated",
              "flag", "source.of.latlon.n",
              "full.name.of.describer" , "date.n")

if(model_params$ll == "Y") {
    df <- get_df1(write=F); dimdf <- dim(df)
}

if (model_params$dataset == "GL") { # global

    join <- df[, ..cols_std]; rm(df)

} else if (model_params$dataset %in% c("LT", "BG", "BM")) { # with regions

    if (model_params$ll == "Y") { # using lat lon

        df <- df[, ..cols_ll]

        # derive new columns for those with lat lon
        is_latlon_absent <- (is.na(df$lat) | is.na(df$lon))
        is_cty_absent <- df$type.country.n == ""
        join_ll <- df[!is_latlon_absent, ]
        join_cty <- df[is_latlon_absent & !is_cty_absent, ]

        if (model_params$dataset %in% c("BG", "BM")) {

            file_input <- ifelse(model_params$dataset=="BG", file_input_biogeo, 
                ifelse(model_params$dataset=="BM"), filepath_input_biomes, "")
            lupsup <- sf::st_read(filepath_input, quiet=T)

            ll <- sf::st_as_sf(join_ll,
                               coords = c('lon', 'lat'),
                               crs = "+init=epsg:4326")
            rm(join_ll)

            join_shp <- sf::st_join(ll, lupshp, join = st_intersects)
            join_shp <- data.table(data.frame(join_shp))[order(as.numeric(idx))]
            join_shp$geometry <- as.character(join_shp$geometry)

            if (model_params$dataset == "BG") {
                names(join_shp)[which(names(join_shp)=="REALM_EDIT")] <- "biogeo_wwf"
            } else if (model_params$dataset == "BM") {
                names(join_shp)[which(names(join_shp)=="BIOME_NAME")] <- "ecoregions2017_biome"
            }

            # derive new columns for those with country/ country + state only
            cols_lup <- ifelse(model_params$dataset=="BG", c("DL", "biogeo_wwf"), 
                ifelse(model_params$dataset=="BM"), c("DL", "ecoregions2017_biome"), "") # TODO: 
            lookup <- ifelse(model_params$dataset=="BG", 
                lookup_cty[prop_area_biogeo_wwf >= 0.6, cols_lup], 
                    ifelse(model_params$dataset=="BM"), 
                        lookup_cty[prop_area_biome_ecor2017 >= 0.6, cols_lup], "") # TODO: 

            join_cty <- merge(join_cty, lookup, 
                            by.x="type.country.n", by.y="DL", all.x=T, all.y=F)
            join_cty <- ifelse(model_params$dataset=="BG", 
                join_cty[!is.na(biogeo_wwf),], ifelse(model_params$dataset == "BM",
                    join_cty[!is.na(ecoregions2017_biome),], ""))

        } else if (model_params$dataset == "LT") {

            ltrop <- 23.436740; lsubtrop <- 35.000000; ltemp <- 35.000000; lpol <- 66.563250
            join_ll$type <- ""
            # join_ll[abs(lat) < ltrop, c("type")] <- "Tropical"
            # join_ll[abs(lat) >= ltrop & abs(lat) < lsubtrop, c("type")] <- "Subtropical"
            # join_ll[abs(lat) >= lsubtrop & abs(lat) < lpol, c("type")] <- "Temperate"
            # join_ll[abs(lat) >= lpol, c("type")] <- "Polar"
            join_ll[abs(lat) < ltrop, c("type")] <- "Tropical"
            join_ll[abs(lat) >= ltrop, c("type")] <- "Not tropical"

            # lookup <- lookup_cty[prop_area_biogeo_wwf >= 0.6, c("DL", "Latitude_type")]
            lookup <- lookup_cty[prop_area_biogeo_wwf >= 0.6, c("DL", "Latitude_type2")]
            join_cty <- merge(join_cty, lookup, by.x="type.country.n", by.y="DL", all.x=T, all.y=F)
            join_cty <- join_cty[!is.na(Latitude_type2),]
        }
        
        # combine datasets
        join <- rbind(join_shp, join_cty, fill=T)
        join$lat <- NULL; join$lon <-  NULL

    } else if (model_params$ll == "N") { # not using lat lon

        dat <- fread(filepath_input_regions, na=c(''), encoding='UTF-8')

        if (model_params$dataset == "LT") { # latitude - tropical or not
            cols <- c(cols_std, "Latitude_type2")
            join <- unique(dat[, ..cols]) # remove duplicates

        } else if (model_params$dataset == "BG") { # biogeographic realms
            cols <- c(cols_std, "biogeo_wwf")
            join <- unique(dat[, ..cols]) # remove duplicates

        # commented out because this should not be allowed
        # } else if (model_params$dataset == "BM") { # biomes
        #     cols <- c(cols_std, "ecoregions2017_biome")
        #     join <- unique(dat[, ..cols]) # remove duplicates
        }

    }

}


write.csv(join, paste0(dir_analysis_edie_tmp, 'format.csv'), row.names=F)
