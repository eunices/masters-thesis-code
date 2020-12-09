source('2020-08-31-jsa-type-v2-ch2/01-prep/prep1/params.r')


assess_latlon <- function(df, filepath_log ) {

    print("----------------- Assessing lat/lon quality")

    df <- df[tolower(status) == "valid species"]
    dimdf <- dim(df) # using raw species dataset
    
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

    list(join_ll = join_ll, join_cty = join_cty)

}


create_latitude_lines <- function(lat, lat_splits) {
    # For latitude
    
    lines <- list()
    counter <- 0
    for (i in names(lat)) {
        counter = counter + 1
        line_splits = lat_splits[[i]]

        if (is.null(line_splits)) {
            lines[[i]] = lat[[i]]
        } else {
            
            min <- lat[[counter]]
            max <- lat[[(counter+1)]]
            segment <- (max - min) / line_splits

            for (n in 0:(line_splits-1)) {
                name <- paste0(i, n+1)
                lines[[name]] <- min + (segment * n)
            }
        }
    }

    reference_names <- c("trop", names(lines))
    list(lines = lines, names = reference_names)
}


assign_latitude <- function(join_ll, ref) {
    # For latitude

    join_ll$latitude <- ""
    join_ll$lat_n <- as.numeric(join_ll$lat_n)
    counter <- 0
    for (i in names(ref$lines)) {
        counter <- counter + 1

        for (j in c("positive", "negative")) {
            ref_line_lower <- ifelse(counter == 1, 0, ref$lines[[(counter)]])
            ref_line_upper <- ifelse(
                counter == length(ref$lines), 90, ref$lines[[(counter + 1)]]
            )

            if (j == "negative") {
                store1 <- ref_line_lower
                store2 <- ref_line_upper
                ref_line_lower <- -store2
                ref_line_upper <- -store1
                rm(store1, store2)
            }

            if (j == "positive") {
                join_ll[
                    lat_n >= 0 &
                    lat_n >= ref_line_lower & lat_n < ref_line_upper
                ]$latitude <- paste0("N_", ref$names[counter])
            } else {
                join_ll[
                    lat_n < 0 &
                    lat_n >= ref_line_lower & lat_n < ref_line_upper
                ]$latitude <- paste0("S_", ref$names[counter])
            }

        }

    }

    # Finish up for the last line
    join_ll[
        lat_n >= 0 & latitude == "" & lat_n > ref$lines$pol
    ]$latitude <- "N_pol"

    join_ll[
        lat_n < 0 & latitude == "" & lat_n > -ref$lines$temp3
    ]$latitude <- "S_pol"

    # join_ll[, 
    #     list(min=min(lat_n), max=max(lat_n), .N), by="latitude"
    # ][order(latitude)]

    join_ll
}


join_spatially <- function(join_ll, shp_grp) {
    print("----------------- Join spatially")

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

    # For data sets which are joined to biogeographic realm shp file,
    # rename column "REALM_EDIT" to "biogeo_wwf"
    names(join_shp)[which(names(join_shp)=="REALM_EDIT")] <- 
        "biogeo_wwf"

    # For data sets which are joined to biome shp file,
    # rename column "BIOME_NAME" to "ecoregions2017_biome"
    names(join_shp)[which(names(join_shp)=="BIOME_NAME")] <- 
        "ecoregions2017_biome"

    join_shp
}


lookup_for_no_ll_biogeo <- function(join_cty) {
    # Only for biogeo, not biome

    print("----------------- Lookup join based on country")

    # Create "biogeo_wwf" with country or country, state only
    join_cty <- merge(
        join_cty, lookup_cty_subset_biogeo[, c("A-3", "biogeo_wwf")], 
        by.x = "type.country_n.A3", by.y = "A-3", 
        all.x = T, all.y = F
    )

    # Remove those that are NA
    join_cty <- join_cty[!is.na(biogeo_wwf),]

    join_cty
}


lookup_for_no_ll_lt <- function(join_cty) {
    # For latitude

    join_cty <- merge(
        join_cty, lookup_cty_subset_ll[, 
            c("A-3", "centroid_lat", "centroid_lon")
        ], 
        by.x = "type.country_n.A3", by.y = "A-3", 
        all.x = T, all.y = F
    )

    # Remove those that are NA
    join_cty <- join_cty[!is.na(centroid_lat),]

    # Rename column from centroid_lat to lat_n
    join_cty$lat_n <- join_cty$centroid_lat
    join_cty$centroid_lat <- NULL

    # Rename column from centroid_lon to lon_n
    join_cty$lon_n <- join_cty$centroid_lon
    join_cty$centroid_lon <- NULL

    join_cty

}


persist_nearest_shp <- function(join, shp_grp, filepath_nearest_loc) {

    print("----------------- Persist nearest shp")

    # For biome (BM)

    ###############################################################
    # PERSISTED DATA

    # for NA, get nearest polygon/biome 
    # note: script takes a long time >3h, so persisted)
    ###############################################################

    get_nearest <- 
      join[is.na(ecoregions2017_biome), c("idx", "lat", "lon")] 

    get_nearest <- 
      st_as_sf(
           get_nearest, 
           coords = c('lon', 'lat'), crs = "+init=epsg:4326"
      )

    nearestBM <- list()
    for (i in 1:dim(get_nearest)[1]) {
        print(paste0(Sys.time(), " -- nearest biome for index ", i))
        v <- get_nearest[i,]
        nearest <- shp_grp[which.min(st_distance(shp_grp, v)),]
        nearest <- as.character(nearest$BIOME_NAME)
        nearestBM[i] <- nearest
    }

    get_nearest$BIOME_NAME <- 
      sapply(nearestBM, function(x) x[[1]])

    st_geometry(get_nearest) <- NULL

    write.csv(
       data.frame(get_nearest), 
       filepath_nearest_loc, 
       row.names = F
    )

    ################################################################
}


join_nearest_biome <- function(join, filepath_nearest_loc) {

    print("----------------- Join nearest biome")

    # For biomes only!

    # Nearest polygon/biome
    to_join_nearest <- fread(filepath_nearest_loc)
    # note: this is persisted in "persist_nearest_shp"

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


write_ending_log <- function(join, filepath_log) {

    print("----------------- Write to log")
    
    # Write to log file
    n_remaining = length(unique(join$idx))
    conn <- file(filepath_log, "a")

    write(
        paste0("Number of remaining rows: ", n_remaining), 
        conn, sep="\n", append=T
    )

    close(conn)

}


format_data <- function(join, dir_model_folder) {

    print("----------------- Write data.csv")
    
    # Format data 
    data <- join

    # Renaming headers

    names(data) <- c("valid_species_id", "species_authority", "year" , "group")
    data <- data[!is.na(group)]     # remove NAs
    data <- data[year <= cutoff_ch2]    # ensure date is <= cut off

    fwrite(
        data, paste0(dir_model_folder, "data.csv"), na = ""
    )

    rm(data, join)

}


