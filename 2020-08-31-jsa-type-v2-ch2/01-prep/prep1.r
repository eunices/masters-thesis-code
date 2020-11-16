# Tabulate species assigned biogeographic realm based on type locality 
# and present data with year

################################################################################


source('2020-08-31-jsa-type-v2-ch2/00-init/init-p.r')
print(paste0(Sys.time(), " --- make dataset"))

# Parameters

# Columns associated with the different dataset

cols_std <- c("idx", "full.name.of.describer", "date")

cols_ll  <- c("idx", "lat_n", "lon_n", "type.country_n.A3", 
              "type.locality.verbatim", "type.locality.updated",
              "full.name.of.describer", "date")

# Thresholds for latitude

ltrop <- 23.436740; lsubtrop <- 35.000000; 
ltemp <- 35.000000; lpol <- 66.563250

################################################################################

# Read files

# Lookup files

lookup_cty <- get_lp_statoid()
lookup_cty_subset <- lookup_cty[prop_area_biogeo_wwf >= 0.6,] # subset

lookup_bm <- get_lp_biome()

# Extra data lookup files 

filepath_nearest_biome_for_missing <- paste0(
    v2_dir_data_ch2, 
    '2020-11-03-nearest-loc.csv'
)
# note: this process took >3h so it was persisted instead

# Read data
if (model_params$ll == "Y") { # using lat/lon
    df <- get_df()
    df <- df[tolower(status) == "valid species"]
    dimdf <- dim(df) # using raw species dataset
} else if (model_params$ll == "N") { # using country distribution
    dat <- get_dis() # using country distribution dataset
}

# Read shapefiles 
if (model_params$dataset == "BG") {
    shp_grp <- get_shp_biogeo()
} else if (model_params$dataset == "BM") {
    shp_grp <- get_shp_biomes()
}

################################################################################

# Wrangle data/ spatial joins

if (model_params$dataset == "GL") { 
    
    # Analyse globally
    
    # Subset relevant columns
    join <- df[, ..cols_std]
    rm(df)

} else if (model_params$dataset %in% c("LT", "BG", "BM", "BN")) { 
    
    # Analyse by region grouping

    if (model_params$ll == "Y") { # Using lat/lon

        # Subset relevant columns
        df <- df[, ..cols_ll]

        # Checks
        is_latlon_absent <- (is.na(df$lat_n) | is.na(df$lon_n))
        is_cty_absent <- 
            df$type.country_n.A3 == "" | is.na(df$type.country_n.A3)


        # Write to log file
        conn <- file(filepath_log, "a")
        
        write(paste0(
            "Number of rows with lat lon missing: ", sum(is_latlon_absent), 
            " of ", dimdf[1], 
            " (", round(sum(is_latlon_absent)/dimdf[1]*100,2), "%)"
        ), conn, sep = "\n", append = T)
        
        write(paste0(
            "Number of rows with lat lon missing & country present: ", 
            sum(is_latlon_absent & !is_cty_absent), " of ", dimdf[1], 
            " (", round(sum(is_latlon_absent & !is_cty_absent)/dimdf[1]*100,2), 
            "%)"
        ), conn, sep = "\n", append = T)

        write(paste0(
            "Number of rows with lat lon missing & country missing: ", 
            sum(is_latlon_absent & is_cty_absent), " of ", dimdf[1], 
            " (", round(sum(is_latlon_absent & is_cty_absent)/dimdf[1]*100,2), 
            "%)\n"
        ), conn, sep="\n", append=T)

        close(conn)


        # Subset dataset based on checks
        
        # with lat/lon
        join_ll <- df[!is_latlon_absent, ]  

        # without lat/lon, w/ country
        join_cty <- df[is_latlon_absent & !is_cty_absent, ]     

        
        if (model_params$dataset %in% c("BG", "BM")) {

            # Create sf object from lat/lon
            ll <- st_as_sf(
                join_ll, 
                coords = c('lon_n', 'lat_n'),
                crs = "+init=epsg:4326"
            )
            rm(join_ll)

            # Make spatial join
            join_shp <- sf::st_join(ll, shp_grp, join = st_intersects)

            # Get coordinates
            coords <- data.table(st_coordinates(join_shp))

            # Update sf object with lat/lon
            join_shp$lon <- coords$X
            join_shp$lat <- coords$Y
            rm(coords)

            # Convert back to data.table
            join_shp <- data.table(data.frame(join_shp))[order(as.numeric(idx))]
            join_shp$geometry <- NULL
            join_shp <- join_shp[!duplicated(idx)]

            if (model_params$dataset == "BG") { # for biogeographic grouping

                # Rename column "REALM_EDIT" to "biogeo_wwf"
                names(join_shp)[which(names(join_shp)=="REALM_EDIT")] <- 
                    "biogeo_wwf"

                # Create "biogeo_wwf" with country or country, state only
                join_cty <- merge(
                    join_cty, lookup_cty_subset[, c("A-3", "biogeo_wwf")], 
                    by.x = "type.country_n.A3", by.y = "A-3", 
                    all.x = T, all.y = F
                )

                # Remove those that are NA
                join_cty <- join_cty[!is.na(biogeo_wwf),]

            } else if (model_params$dataset == "BM") {

                # Rename column "BIOME_NAME" to "ecoregions2017_biome"
                names(join_shp)[which(names(join_shp)=="BIOME_NAME")] <- 
                    "ecoregions2017_biome"

                # Subset dataset with relevant columns
                cols_eco <- c(cols_ll, "ecoregions2017_biome", "lat", "lon")
                cols_eco <- names(join_shp)[names(join_shp) %in% cols_eco]
                join <- join_shp[, ..cols_eco] 
                # note: cannot lookup by country unlike for "BG"


                ################################################################
                # # PERSISTED DATA

                # # for NA, get nearest polygon/biome 
                # # note: script takes a long time >3h, so persisted)
                # get_nearest <- 
                #   join[is.na(ecoregions2017_biome), c("idx", "lat", "lon")] 

                # get_nearest <- 
                #   st_as_sf(
                #        get_nearest, 
                #        coords = c('lon', 'lat'), crs = "+init=epsg:4326"
                #   )

                # nearestBM <- list()
                # for (i in 1:dim(get_nearest)[1]) {
                #     print(paste0(Sys.time(), " -- nearest biome for index ", i))
                #     v <- get_nearest[i,]
                #     nearest <- shp_grp[which.min(st_distance(shp_grp, v)),]
                #     nearest <- as.character(nearest$BIOME_NAME)
                #     nearestBM[i] <- nearest
                # }

                # get_nearest$BIOME_NAME <- 
                #   sapply(nearestBM, function(x) x[[1]])

                # st_geometry(get_nearest) <- NULL
                
                # write.csv(
                #    data.frame(get_nearest), 
                #    filepath_nearest_biome_for_missing, 
                #    row.names = F
                # )

                ################################################################

                # Above script takes a long time (>3 h), thus its persisted
                # Nearest polygon/biome
                to_join_nearest <- fread(filepath_nearest_biome_for_missing)

                # Join the "nearest" location to those not intersecting with biomes
                join <- join(
                    join, 
                    to_join_nearest[, c("idx", "BIOME_NAME")], 
                    by = "idx"
                )
                
                join[is.na(ecoregions2017_biome)]$ecoregions2017_biome <- 
                    join[is.na(ecoregions2017_biome)]$BIOME_NAME
                
                join$BIOME_NAME <- NULL
                
                # Coarse categories
                join <- merge(
                    join, lookup_bm,
                    by.x="ecoregions2017_biome", by.y="BIOME_NAME",
                    all.x=T, all.y=F
                )

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
            join_cty <- merge(
                join_cty, lookup_cty[, c("DL", "Latitude_type2")], 
                by.x="type.country.n", by.y="DL", 
                all.x=T, all.y=F
            )

            # Remove those which are NA
            join_cty <- join_cty[!is.na(Latitude_type2),]
        }


        # Combine datasets
        if (model_params$dataset %in% c("LT", "BG", "BN")) {
            join <- rbind(join_shp, join_cty, fill=T)
        }
        
        # Subset dataset with columns required
        custom_col <- 
            ifelse(model_params$dataset == "LT", "type", 
                ifelse((model_params$dataset == "BG") | 
                    (model_params$dataset == "BN"), "biogeo_wwf", 
                        ifelse(model_params$dataset == "BM", "BIOME_CAT", "")))

        cols_ll_final <- c(cols_std,  custom_col)

        join <- join[, ..cols_ll_final]

    } else if (model_params$ll == "N") { # not using lat lon

        if (model_params$dataset == "LT") {

            # Using "dat"
            cols <- c(cols_std, "Latitude_type2")
            join <- unique(dat[, ..cols]) # remove duplicates

        } else if (
            (model_params$dataset == "BG") | 
            (model_params$dataset == "BN")
        ) {

            # Using "dat"
            cols <- c(cols_std, "biogeo_wwf")
            
            # remove duplicates
            join <- unique(dat[, ..cols]) 
            
            # remove Antarctica
            if (model_params$dataset == "BN") join <- join[biogeo_wwf != "AN",] 

        }
    }
}

# Group by family
if (model_params$dataset == "FA") {
    df <- get_df()
    df <- df[tolower(status) == "valid species"]

    join <- df[, c("idx", "full.name.of.describer", "date", "family")][
        order(as.numeric(idx))]
}

# Group by genus
if (model_params$dataset == "GE") {

    df <- get_df()
    df <- df[tolower(type) == "valid species"]

    join <- df[, c("idx", "full.name.of.describer", "date", "genus")][
        order(as.numeric(idx))]
    
    top10 <- c("andrena", "lasioglossum", "megachile", 
               "bombus", "hylaeus", "nomada", "coelioxys",
               "anthophora", "colletes", "perdita")

    join <- join[tolower(genus) %in% top10]
}

# Only Halictidae, group by genus
if(model_params$dataset == "HA") {

    df <- get_df()
    df <- df[tolower(status) == "valid species"]

    join <- df[, c("idx", "full.name.of.describer", "date", "genus")][
        order(as.numeric(idx))]

    halictidae <- c("lasioglossum", "lipotriches", "sphecodes",
                    "patellapis", "halictus", "dufourea", "augochloropsis",
                    "nomia", "augochlora", "neocorynura")

    join <- join[tolower(genus) %in% halictidae]
}

# Write to log file
n_removed = dimdf[1] - length(unique(join$idx))
n_removed_percentage = n_removed/dimdf[1]*100
conn <- file(filepath_log, "a")

write(
    paste0("Number of species not assigned to group: ", n_removed, " of ",
           dimdf[1], " (", round(n_removed_percentage,2), "%)\n"), 
    conn, sep="\n", append=T)

close(conn)


# Format data 
data <- join

if (model_params$dataset == "GL") { # global

    # duplicate groups
    data1 <- cbind(data, group = 1)
    data2 <- cbind(data, group = 2) 
    data <- rbind(data1, data2)

}

# Renaming headers

names(data) <- c("valid_species_id", "species_authority", "year" , "group")
data <- data[!is.na(group)]     # remove NAs
data <- data[year <= cutoff_ch2]    # ensure date is <= cut off

fwrite(
    data, paste0(dir_model_folder, "data.csv"), na = ""
)

rm(data, join)
