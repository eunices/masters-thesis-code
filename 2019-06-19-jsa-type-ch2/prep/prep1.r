# filepaths
filepath_input_regions <- paste0(dir_data, basefile, ' filtered_5-species-cty2-cty.csv')

# read lookup files
filepath_input_biogeo <- 'data/geo_processed/teow/official/wwf_terr_ecos_dissolved.shp'
filepath_input_biomes <- 'data/geo/0_manual/Ecoregions2017/Ecoregions2017.shp'
lookup_cty <- fread('data/lookup/2019-05-29-statoid-country-codes.csv', na=c(''), encoding='UTF-8')
lookup_bm <- fread('data/lookup/2019-10-14-biome-broad-cat.csv', na=c(''), encoding='UTF-8')

# split dataset according to different parameters as format.csv
cols_std <- c("idx", "full.name.of.describer", "date.n")
cols_ll  <- c("idx", "lat", "lon",
              "type.country.n.full", "type.state.n.full",
              "type.country.n", "type.state.n",
              "type.locality.verbatim", "type.locality.updated",
              "flag", "source.of.latlon.n",
              "full.name.of.describer" , "date.n")

# model_params = list(ll="Y", dataset="BM") # for testing purposes
if (model_params$ll == "Y") {
    df <- get_df1(write=F); dimdf <- dim(df)
}

if (model_params$dataset == "GL") { # global
    
    join <- df[, ..cols_std]; rm(df)

} else if (model_params$dataset %in% c("LT", "BG", "BM", "BN")) { # with regions

    if (model_params$ll == "Y") { # using lat lon

        df <- df[, ..cols_ll]

        # derive new columns for those with lat lon
        is_latlon_absent <- (is.na(df$lat) | is.na(df$lon))
        is_cty_absent <- df$type.country.n == ""
        join_ll <- df[!is_latlon_absent, ]
        join_cty <- df[is_latlon_absent & !is_cty_absent, ]

        if (model_params$dataset %in% c("BG", "BM")) {

            file_input <- ifelse(model_params$dataset=="BG", filepath_input_biogeo, 
                ifelse(model_params$dataset=="BM", filepath_input_biomes, ""))
            lupsup <- sf::st_read(file_input, quiet=T)

            ll <- sf::st_as_sf(join_ll,
                               coords = c('lon', 'lat'),
                               crs = "+init=epsg:4326")
            rm(join_ll)

            join_shp <- sf::st_join(ll, lupsup, join = st_intersects)
            coords <- data.table(st_coordinates(join_shp))
            join_shp$lon <- coords$X; join_shp$lat <- coords$Y; rm(coords)
            join_shp <- data.table(data.frame(join_shp))[order(as.numeric(idx))]
            join_shp$geometry <- NULL
            join_shp <- join_shp[!duplicated(idx)]

            if (model_params$dataset == "BG") {
                names(join_shp)[which(names(join_shp)=="REALM_EDIT")] <- "biogeo_wwf"
                
                # derive new columns for those with country/ country + state only
                lookup <- lookup_cty[prop_area_biogeo_wwf >= 0.6, c("DL", "biogeo_wwf")]
                join_cty <- merge(join_cty, lookup, by.x="type.country.n", by.y="DL", all.x=T, all.y=F)
                join_cty <- join_cty[!is.na(biogeo_wwf),]

            } else if (model_params$dataset == "BM") {
                names(join_shp)[which(names(join_shp)=="BIOME_NAME")] <- "ecoregions2017_biome"
                cols_eco <- c(cols_ll, "ecoregions2017_biome")

                join <- join_shp[, ..cols_eco]
                # cannot lookup by country

                # # for NA, get nearest polygon (script takes a long time >3h, so persisted)
                # get_nearest <- 
                #     join[is.na(ecoregions2017_biome), c("idx", "lat", "lon")] 
                # get_nearest <- 
                #     sf::st_as_sf(get_nearest, coords = c('lon', 'lat'), crs = "+init=epsg:4326")

                # nearestBM <- list()
                # for (i in 1:dim(get_nearest)[1]) {
                #     print(paste0("Getting nearest biome for index", i))
                #     nearestBM[i] <- 
                #         as.character(lupsup[which.min(st_distance(lupsup, get_nearest[i,])),]$BIOME_NAME)
                # }
                # get_nearest$BIOME_NAME <- sapply(nearestBM, function(x) x[[1]])
                # st_geometry(get_nearest) <- NULL
                # write.csv(data.frame(get_nearest), 
                #           paste0(dir_data, 'ch2/2019-10-14-nearest-loc.csv'), 
                #           row.names=F)

                # Above script takes a long time (>3 h), thus its persisted
                to_join_nearest <- fread(paste0(dir_data, '/ch2/2019-10-14-nearest-loc.csv'))

                # Join the "nearest" location to those not intersecting with biomes
                join <- join(join, to_join_nearest[, c("idx", "BIOME_NAME")], by="idx")
                join[is.na(ecoregions2017_biome)]$ecoregions2017_biome <- 
                    join[is.na(ecoregions2017_biome)]$BIOME_NAME; join$BIOME_NAME <- NULL
                
                # Coarse categories
                join <- merge(join, lookup_bm,
                              by.x="ecoregions2017_biome", by.y="BIOME_NAME",
                              all.x=T, all.y=F)
                
                join <- join[BIOME_CAT != "N/A"]

            }
                
            rm(lupsup)

        } else if (model_params$dataset == "LT") {

            ltrop <- 23.436740; lsubtrop <- 35.000000; ltemp <- 35.000000; lpol <- 66.563250
            join_ll$type <- ""
            join_ll[abs(lat) < ltrop, c("type")] <- "Tropical"
            join_ll[abs(lat) >= ltrop, c("type")] <- "Not tropical"
            join_shp <- join_ll; rm(join_ll)

            # lookup <- lookup_cty[prop_area_biogeo_wwf >= 0.6, c("DL", "Latitude_type")]
            lookup <- lookup_cty[prop_area_biogeo_wwf >= 0.6, c("DL", "Latitude_type2")]
            join_cty <- merge(join_cty, lookup, by.x="type.country.n", by.y="DL", all.x=T, all.y=F)
            join_cty <- join_cty[!is.na(Latitude_type2),]
        }

        if (model_params$dataset %in% c("LT", "BG", "BN")) {
            join <- rbind(join_shp, join_cty, fill=T) # combine datasets
        }
        
        # subset columns required
        custom_col <- ifelse(model_params$dataset == "LT", "type", 
            ifelse((model_params$dataset == "BG") |  (model_params$dataset == "BN"), "biogeo_wwf", 
                ifelse(model_params$dataset == "BM", "BIOME_CAT", "")))
        cols_ll_final <- c(cols_std,  custom_col)
        join <- join[, ..cols_ll_final]

    } else if (model_params$ll == "N") { # not using lat lon

        dat <- fread(filepath_input_regions, na=c(''), encoding='UTF-8')

        if (model_params$dataset == "LT") { # latitude - tropical or not
            cols <- c(cols_std, "Latitude_type2")
            join <- unique(dat[, ..cols]) # remove duplicates

        } else if ((model_params$dataset == "BG") |  (model_params$dataset == "BN")) { # biogeographic realms
            cols <- c(cols_std, "biogeo_wwf")
            join <- unique(dat[, ..cols]) # remove duplicates
            if (model_params$dataset == "BN") {
                join <- join[biogeo_wwf != "AN",]# remove antarctica
            }

        # commented out because this should not be allowed - "BM" not by country GLOBAL.MAPPER
        }
    }
}

if (model_params$dataset == "FA") {
    df <- get_df1(write=F)
    join <- df[, c("idx", "full.name.of.describer", "date.n", "family")][order(as.numeric(idx))]
}

if (model_params$dataset == "GE") {
    df <- get_df1(write=F)
    join <- df[, c("idx", "full.name.of.describer", "date.n", "genus")][order(as.numeric(idx))]
    join <- join[tolower(genus) %in% c("andrena", "lasioglossum", "megachile", 
                                       "bombus", "hylaeus", "nomada", "coelioxys",
                                       "anthophora", "colletes", "perdita")]
}
if(model_params$dataset == "HA") {
    df <- get_df1(write=F)
    join <- df[, c("idx", "full.name.of.describer", "date.n", "genus")][order(as.numeric(idx))]
    join <- join[genus %in% c("Lasioglossum", "Lipotriches", "Sphecodes",
                              "Patellapis", "Halictus", "Dufourea", "Augochloropsis",
                              "Nomia", "Augochlora", "Neocorynura")]
}

write.csv(join, paste0(dir_model_folder, 'format.csv'), row.names=F, na="", fileEncoding = "UTF-8")

# Format the dataframe into actual dataset
input_filepath <- paste0(dir_model_folder, "format.csv", na="")
data <- fread(input_filepath, na=c(''), encoding='UTF-8')

if (model_params$dataset == "GL") { # global
    data1 <- cbind(data, group=1); data2 <- cbind(data, group=2) # duplicate groups
    data <- rbind(data1, data2)
}

# Renaming headers
names(data) <- c("valid_species_id", "species_authority", "year" , "group")
data <- data[!is.na(group)] # remove NAs
data <- data[year<=2018]    # ensure date is <=2018
write.csv(data, paste0(dir_model_folder, "data.csv"), row.names=F, na="", fileEncoding = "UTF-8")

rm(data, join)
