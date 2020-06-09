####################################################################################################

# Filepaths

# Dataset
filepath_input_regions <- paste0(dir_data, basefile, ' filtered_5-species-cty2-cty.csv')

# Shp files
filepath_input_biogeo <- 'data/geo_processed/teow/official/wwf_terr_ecos_dissolved.shp'
filepath_input_biomes <- 'data/geo/0_manual/Ecoregions2017/Ecoregions2017.shp'

# Lookup files
lookup_cty <- fread('data/lookup/2019-05-29-statoid-country-codes.csv', na=c(''), encoding='UTF-8')
lookup_cty_subset <- lookup_cty[prop_area_biogeo_wwf >= 0.6,] # subset for biogeo areas >=.6
lookup_bm <- fread('data/lookup/2019-10-14-biome-broad-cat.csv', na=c(''), encoding='UTF-8')

# Extra data lookup files 
filepath_nearest_biome_for_missing <- paste0(dir_data, '/ch2/2019-10-14-nearest-loc.csv')
# [this process took >3h so it was persisted instead]


####################################################################################################

# Parameters
# Columns associated with the different dataset
cols_std <- c("idx", "full.name.of.describer", "date.n")
cols_ll  <- c("idx", "lat", "lon",
              "type.country.n.full", "type.state.n.full",
              "type.country.n", "type.state.n",
              "type.locality.verbatim", "type.locality.updated",
              "flag", "source.of.latlon.n",
              "full.name.of.describer" , "date.n")

# Thresholds for latitude
ltrop <- 23.436740; lsubtrop <- 35.000000; ltemp <- 35.000000; lpol <- 66.563250


####################################################################################################

# Wrangle dataset with spatial joins and lookup tables

if (model_params$ll == "Y") { # Using lat/lon

    # Using raw species data if using lat/lon
    df <- get_df1(write=F); dimdf <- dim(df)

} else if (model_params$ll == "N") { # Using country distribution

    # Using the country distribution dataset if not using lat/lon
    dat <- fread(filepath_input_regions, na=c(''), encoding='UTF-8')

}

if (model_params$dataset == "GL") { # Analyse globally
    
    # Subset relevant columns
    join <- df[, ..cols_std]; rm(df)

} else if (model_params$dataset %in% c("LT", "BG", "BM", "BN")) { # Analyse by region grouping

    if (model_params$ll == "Y") { # Using lat/lon

        # Subset relevant columns
        df <- df[, ..cols_ll]

        # Checks
        is_latlon_absent <- (is.na(df$lat) | is.na(df$lon)) # for lat lon
        is_cty_absent <- df$type.country.n == ""            # for country

        # Write to log file
        conn <- file(filepath_log, "a")
        write(paste0("Number of rows with lat lon missing: ", sum(is_latlon_absent), " of ", dimdf[1], 
                     " (", round(sum(is_latlon_absent)/dimdf[1]*100,2), "%)"), conn, sep="\n", append=T)
        write(paste0("Number of rows with lat lon missing & country present: ", 
                     sum(is_latlon_absent & !is_cty_absent), " of ", dimdf[1], 
                     " (", round(sum(is_latlon_absent & !is_cty_absent)/dimdf[1]*100,2), "%)"), 
              conn, sep="\n", append=T)
        write(paste0("Number of rows with lat lon missing & country missing: ", 
                     sum(is_latlon_absent & is_cty_absent), " of ", dimdf[1], 
                     " (", round(sum(is_latlon_absent & is_cty_absent)/dimdf[1]*100,2), "%)\n"), 
              conn, sep="\n", append=T)
        close(conn)


        # Subset dataset based on checks
        join_ll <- df[!is_latlon_absent, ]                      # with lat/lon
        join_cty <- df[is_latlon_absent & !is_cty_absent, ]     # without lat/lon, w/ country

        

        if (model_params$dataset %in% c("BG", "BM")) {

            # Read shapefile 
            file_input <- ifelse(model_params$dataset=="BG", filepath_input_biogeo, 
                ifelse(model_params$dataset=="BM", filepath_input_biomes, ""))
            shp_grp <- sf::st_read(file_input, quiet=T)

            # Create sf object from lat/lon
            ll <- sf::st_as_sf(join_ll,
                               coords = c('lon', 'lat'),
                               crs = "+init=epsg:4326")
            rm(join_ll)

            # Make spatial join
            join_shp <- sf::st_join(ll, shp_grp, join=st_intersects)

            # Get coordinates
            coords <- data.table(st_coordinates(join_shp))
            # And update the sf object with lat/lon
            join_shp$lon <- coords$X; join_shp$lat <- coords$Y; rm(coords)

            # Convert back to data.table
            join_shp <- data.table(data.frame(join_shp))[order(as.numeric(idx))]
            join_shp$geometry <- NULL
            join_shp <- join_shp[!duplicated(idx)]


            if (model_params$dataset == "BG") { # for biogeographic grouping

                # Rename column "REALM_EDIT" to "biogeo_wwf"
                names(join_shp)[which(names(join_shp)=="REALM_EDIT")] <- "biogeo_wwf"

                # Create "biogeo_wwf" with country or country, state only
                join_cty <- merge(join_cty, lookup_cty_subset[, c("DL", "biogeo_wwf")], 
                                  by.x="type.country.n", by.y="DL", 
                                  all.x=T, all.y=F)

                # Remove those that are NA
                join_cty <- join_cty[!is.na(biogeo_wwf),]


            } else if (model_params$dataset == "BM") {

                # Rename column "BIOME_NAME" to "ecoregions2017_biome"
                names(join_shp)[which(names(join_shp)=="BIOME_NAME")] <- "ecoregions2017_biome"

                # Subset dataset with relevant columns
                cols_eco <- c(cols_ll, "ecoregions2017_biome")
                join <- join_shp[, ..cols_eco] # NOTE: cannot lookup by country unlike for "BG"

                # # for NA, get nearest polygon/biome (script takes a long time >3h, so persisted)
                # get_nearest <- join[is.na(ecoregions2017_biome), c("idx", "lat", "lon")] 
                # get_nearest <- st_as_sf(get_nearest, coords = c('lon', 'lat'), crs = "+init=epsg:4326")

                # nearestBM <- list()
                # for (i in 1:dim(get_nearest)[1]) {
                #     print(paste0("Getting nearest biome for index", i))
                #     nearest = shp_grp[which.min(st_distance(shp_grp, get_nearest[i,])),]
                #     nearest = as.character(nearest$BIOME_NAME)
                #     nearestBM[i] <- nearest
                # }

                # get_nearest$BIOME_NAME <- sapply(nearestBM, function(x) x[[1]])
                # st_geometry(get_nearest) <- NULL
                # write.csv(data.frame(get_nearest), filepath_nearest_biome_for_missing, row.names=F)

                # Above script takes a long time (>3 h), thus its persisted
                # Nearest polygon/biome
                to_join_nearest <- fread(filepath_nearest_biome_for_missing)

                # Join the "nearest" location to those not intersecting with biomes
                join <- join(join, to_join_nearest[, c("idx", "BIOME_NAME")], by="idx")
                join[is.na(ecoregions2017_biome)]$ecoregions2017_biome <- 
                    join[is.na(ecoregions2017_biome)]$BIOME_NAME
                join$BIOME_NAME <- NULL
                
                # Coarse categories
                join <- merge(join, lookup_bm,
                              by.x="ecoregions2017_biome", by.y="BIOME_NAME",
                              all.x=T, all.y=F)

                # Remove those that are NA
                join <- join[BIOME_CAT != "N/A"]

            }
            
            # Remove spatial join file
            rm(shp_grp)

        } else if (model_params$dataset == "LT") { # Grouping by latitude
            
            # Initiate new column "type" to indicate whether it is tropical or not
            join_ll$type <- ""
            join_ll[abs(lat) < ltrop, c("type")] <- "Tropical"
            join_ll[abs(lat) >= ltrop, c("type")] <- "Not tropical"
            join_shp <- join_ll; rm(join_ll)

            # Lookup countries
            join_cty <- merge(join_cty, lookup_cty[, c("DL", "Latitude_type2")], 
                              by.x="type.country.n", by.y="DL", all.x=T, all.y=F)

            # Remove those which are NA
            join_cty <- join_cty[!is.na(Latitude_type2),]
        }


        # Combine datasets
        if (model_params$dataset %in% c("LT", "BG", "BN")) {
            join <- rbind(join_shp, join_cty, fill=T)
        }
        
        # Subset dataset with columns required
        custom_col <- ifelse(model_params$dataset == "LT", "type", 
            ifelse((model_params$dataset == "BG") |  (model_params$dataset == "BN"), "biogeo_wwf", 
                ifelse(model_params$dataset == "BM", "BIOME_CAT", "")))
        cols_ll_final <- c(cols_std,  custom_col)
        join <- join[, ..cols_ll_final]

    } else if (model_params$ll == "N") { # not using lat lon

        if (model_params$dataset == "LT") {

            # Using "dat"
            cols <- c(cols_std, "Latitude_type2")
            join <- unique(dat[, ..cols]) # remove duplicates

        } else if ((model_params$dataset == "BG") |  (model_params$dataset == "BN")) {

            # Using "dat"
            cols <- c(cols_std, "biogeo_wwf")
            join <- unique(dat[, ..cols]) # remove duplicates
            if (model_params$dataset == "BN") join <- join[biogeo_wwf != "AN",] # remove Antarctica

        }
    }
}

# Group by family
if (model_params$dataset == "FA") {
    df <- get_df1(write=F)
    join <- df[, c("idx", "full.name.of.describer", "date.n", "family")][order(as.numeric(idx))]
}

# Group by genus
if (model_params$dataset == "GE") {
    df <- get_df1(write=F)
    join <- df[, c("idx", "full.name.of.describer", "date.n", "genus")][order(as.numeric(idx))]
    join <- join[tolower(genus) %in% c("andrena", "lasioglossum", "megachile", 
                                       "bombus", "hylaeus", "nomada", "coelioxys",
                                       "anthophora", "colletes", "perdita")]
}

# Only Halictidae, group by genus
if(model_params$dataset == "HA") {
    df <- get_df1(write=F)
    join <- df[, c("idx", "full.name.of.describer", "date.n", "genus")][order(as.numeric(idx))]
    join <- join[genus %in% c("Lasioglossum", "Lipotriches", "Sphecodes",
                              "Patellapis", "Halictus", "Dufourea", "Augochloropsis",
                              "Nomia", "Augochlora", "Neocorynura")]
}

# Write file
write.csv(join, paste0(dir_model_folder, 'format.csv'), row.names=F, na="", fileEncoding = "UTF-8")

# Write to log file
n_removed = dimdf[1] - length(unique(join$idx))
n_removed_percentage = n_removed/dimdf[1]*100
conn <- file(filepath_log, "a")
write(paste0("Number of species not assigned to group: ", n_removed, " of ", dimdf[1], 
             " (", round(n_removed_percentage,2), "%)\n"), conn, sep="\n", append=T)
close(conn)


####################################################################################################

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
