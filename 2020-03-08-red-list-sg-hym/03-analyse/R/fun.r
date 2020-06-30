# These will converted into package functions
source(paste0("2020-03-08-red-list-sg-hym/03-analyse/R/", "init.r"))

library(sf)
library(data.table)

# For package
# TODO: modularise code further
# TODO: put into package format
# TODO: write tests
# TODO: setup github actions for CI

# With wendy's data later: 
# TODO: process data first (dealing with missing no)
# TODO: try the code on wendy's data
# TODO: write code to lapply for species maps/ tabulate % in each category 
# TODO: write intro

#-------------------------------------------------------------------------------------------------

#' Classify parks based on threshold
#' 
#' \code{classify_parks} returns a sf object of parks with habitat column.
#' 
#' This function takes the Singapore park gpkg and reclassifies the geospatial files
#' based on rules defined by minimum park size \code{min_park_size}, minimum area for unmanaged
#' trees \code{min_unmanaged_tree_area}, minimum percentage of vegetation 
#' \code{min_vegetation_percentage}.
#' 
#' @param min_park_size Minimum park size to be filtered for, in m^2.
#' @param min_unmanaged_tree_area Minimum unmanaged trees area to be classified as secondary forest
#' habitat, in m^2. 
#' @param min_vegetation_percentage Minimum percentage of vegetation for parks to be classified as
#' secondary forest habitat, in m^2.
#' 
#' @return An sf object that contains the Singapore park geospatial layer.
#' 
#' @export
classify_parks <- function(min_park_size = 10000, 
						   min_unmanaged_tree_area = 10000, 
						   min_vegetation_percentage = 80,
						   v_parks_all = NA) {

	# Read park layer
	if (is.na(v_parks_all)) {

		p <- st_read(g_parks_all)
		# p <- get_data_and_assign(v_parks_all)

	} else {

		p <- v_parks_all
		rm(v_parks_all)

	}


	# Parameters
	threshold_park_size <- min_park_size
	threshold_unmanaged_trees <- min_unmanaged_tree_area / 5  # 5m^2/pixel
	threshold_vegetation <- min_vegetation_percentage


	# Boolean checks
	isLarge <- p$AREA_SQ_M > threshold_park_size
	
	isMangrove <- p$mangrove > 1

	isForestCover <- p$veg_canopy_unmanaged >= threshold_unmanaged_trees &
		p$prop_veg > threshold_vegetation


	# Assignment
	p[!isLarge,]$habitat <- "SMALL GREEN SPACE"                                   # Small parks
	p[isLarge & isMangrove,]$habitat <- "MANGROVE"                                # Mangrove
	p[isLarge & !isMangrove & !isForestCover,]$habitat <- "URBAN/SEMI-URBAN"      # Urban/semi-urban
	p[isLarge & !isMangrove & isForestCover,]$habitat <- "YOUNG SECONDARY FOREST" # Young secondary


	# Print reuslts
	print(table(isLarge))
	print(table(p$habitat))

	p
}


#-------------------------------------------------------------------------------------------------

#' Assign IUCN status based on occurrence records.
#' 
#' \code{generate_iucn_categories} returns a data.table of species and corresponding IUCN status.
#'
#' This function assigns each species to an IUCN status, using a rule-based method following a flow
#' chart. It takes in species occurrence records \code{df_species} and its corresponding 
#' projection (defaults to WGS84) \code{df_species_epsg} and coordinate label names 
#' \code{coord_columns}, and vector layers which are objects in a list \code{vector_layers}, 
#' comprising of islands \code{vector_layers$v_islands}, nature reserves 
#' \code{vector_layers$v_parks_nat_res}, parks \code{vector_layers$v_parks_all}, unmanaged
#' greenery \code{vector_layers$v_greenery}, administrative planning areas
#' \code{vector_layers$v_planning_areas}. 
#' 
#' @param df_species This is the species occurrence data.frame. It must contain coordinates, as 
#' well as identifier column "id", species name "species", collection date "collection_date",
#' and worker/alate status "type" at the minimal. 
#' @param df_species_epsg This is EPSG code (integer) that specifies the projection of the 
#' coordinates of df_species. It defaults to WGS84 (EPSG 4326).
#' @param coord_columns This is the coordinate names of df_species, which defaults to "X" and "Y" 
#' for longitude and latitude respectively, in the WGS84 context.
#' @param vector_layers This is a list that presents a series of sf objects, which are to be used 
#' as spatial joins to the df_species. This includes the islands, parks, nature reserves, unmanaged
#' greenery and administrative planning areas. If they are NA, the default layers from the package 
#' will be applied. Each of these layers must have a "NAME" column which identifies the site name
#' and will be used to count the number of unique sites.
#' 
#' @return This returns a data.table of species and assigned IUCN statuses. 
#' 
#' @export 
generate_iucn_table <- function(df_species, 

								date_cut_off = as.Date("1960-01-01"),
								coord_columns = c("X", "Y"),
								identifier_columns = c("id", "species", "type"),
								collection_date_column = "collection_date",
								
								df_species_epsg = 4326,

								vector_layers = list(v_islands = NA, 
												      v_parks_nat_res = NA,
												      v_parks_all = NA,
												      v_greenery = NA,
												      v_planning_areas = NA)) {

	df_habitat <- 
		generate_habitat(df_species, identifier_columns, coord_columns)

	df_habitat_sp_mat <- 
		generate_habitat_sp_matrix(df_habitat, collection_date_column, date_cut_off)

	df_bool <- 
		generate_boolean_check_vars(df_species, date_cut_off)

	df_iucn <- 
		generate_iucn_status(df_bool, df_habitat_sp_mat)

	df_iucn
	
}


create_species_sf_obj <- function(df_species,
								  coord_columns,
							      df_species_epsg = 4326) {

	# Constants
	epsg_svy21 <- 3414


	# Filter records with lat/lon
	df_species_latlon <- df_species[!is.na(get(coord_columns[1])) &
								    !is.na(get(coord_columns[2]))]

	# Change species coordinates to sf object
	v_species <- st_as_sf(df_species, coords=coord_columns) 
	# needs to have columns "species", "collection_date", "type", and coord columns

	if(is.na(st_crs(v_species))) v_species <- st_set_crs(v_species, df_species_epsg)
	
	v_species <- st_transform(v_species, epsg_svy21)
	

	# Filter results without lat/lon
	df_species_nolatlon <- df_species[is.na(get(coord_columns[1])) |
								      is.na(get(coord_columns[2]))]


	# Returns those with lat/lon as sf object, those without as data.table
	list(v_species = v_species, df_species_nolatlon = df_species_nolatlon)

}


generate_initial_habitat <- function(v_species, 
									 vector_layers,
									 identifier_columns) {

	# Islands from Teo Siyang's base layers
	if (is.na(vector_layers$v_islands)) {
		v_islands <- st_read(g_islands)
		# v_islands <- get_data_and_assign(v_islands)
		
	} else {
		v_islands <- vector_layers$v_islands
	} 

	# Parks (natural reserves) from Ascher et al. (2020)
	if (is.na(vector_layers$v_parks_nat_res)) {
		v_parks_nat_res <- st_read(g_parks_nat_res)
		# v_parks_nat_res <- get_data_and_assign(v_parks_nat_res)
		
	} else {
		v_parks_nat_res <- vector_layers$v_parks_nat_res
	}

	# Parks (all) using SCDP parks layer, from Data.gov.sg
	if (is.na(vector_layers$v_parks_all)) {
		v_parks_all <- st_read(g_parks_all)
		# v_parks_all <- get_data_and_assign(v_parks_all)

	} else {
		v_parks_all <- vector_layers$v_parks_all
	}

	# Greenery in Singapore (contiguous unmanaged tree cover >=1 ha based on Gaw et al. [2019])
	if (is.na(vector_layers$v_greenery)) {
		v_greenery <- st_read(g_greenery)
		# v_greenery <- get_data_and_assign(v_greenery)
		
	} else {
		v_greenery <- vector_layers$v_greenery
	}

	# URA planning areas (2014) from Data.gov.sg
	if(is.na(vector_layers$v_planning_areas)) {
		v_planning_areas <- st_read(g_planning_areas)
		# v_planning_areas <- get_data_and_assign(v_planning_areas)

	} else {
		v_planning_areas <- vector_layers$v_planning_areas
	}


	# Get data.table from each spatial join table									 
	df_islands <- get_data_from_sp_sf(st_join(v_species, v_islands))               # islands
	df_parks_nat_res <- get_data_from_sp_sf(st_join(v_species, v_parks_nat_res))   # parks, nat res
	df_parks_all <- get_data_from_sp_sf(st_join(v_species, v_parks_all))           # parks, others
	df_greenery <- get_data_from_sp_sf(st_join(v_species, v_greenery))             # greenery
	df_planning_areas <- get_data_from_sp_sf(st_join(v_species, v_planning_areas)) # planning areas

									
	# Create one dataset from all the spatial join datasets by merging
	df_habitat <- merge(df_islands,
						df_parks_nat_res,
						by = identifier_columns, 
				        all.x = T, all.y = T,
						suffixes = c("_island", "_parks_nr"))


	df_habitat <- merge(df_habitat, 
					    df_parks_all,
						by = identifier_columns, 
				     	all.x = T, all.y = T, 
						suffixes = c("", "_parks_all"))

	names(df_habitat)[which(names(df_habitat)=="site_name")] <- "site_name_parks_all"


	df_habitat <- merge(df_habitat,
					    df_greenery,
						by = identifier_columns, 
					    all.x = T, all.y = T, 
						suffixes=c("", "_greenery"))

	names(df_habitat)[which(names(df_habitat)=="site_name")] <- "site_name_greenery"


	df_habitat <- merge(df_habitat,
						df_planning_areas,
						by = identifier_columns,
					    all.x = T, all.y = T, 
						suffixes = c("", "_pa"))

	names(df_habitat)[which(names(df_habitat)=="site_name")] <- "site_name_pa"
	

	data.table(df_habitat)

}


generate_final_habitat <- function(df_habitat, 
							       identifier_columns) {

	# Original columns
	original_cols <- names(df_habitat)


	# Create the final habitat variable for each record
	# base on hierachy of island > parks (nat. res.) > 
	# parks (others) > greenery > planning area > none

	# For habitat,
	df_habitat$habitat_final <- 
		"NONE"

	df_habitat[!is.na(site_name_pa)]$habitat_final <- 
		"PLANNING AREA"

	df_habitat[!is.na(site_name_greenery)]$habitat_final <-  # outside of parks/ nat. res.
		"GREENERY"

	df_habitat[!is.na(site_name_parks_all)]$habitat_final <- 
		paste0("PARKS (ALL) -- ", df_habitat[!is.na(site_name_parks_all)]$habitat)

	df_habitat[!is.na(site_name_parks_nr)]$habitat_final =
		"PARKS (NATURE RESERVE)"

	df_habitat[!is.na(site_name_island)]$habitat_final <- 
		"ISLAND"


	# For site name,
	df_habitat$site_name_final <- "NONE"

	df_habitat[!is.na(site_name_pa)]$site_name_final <- 
		df_habitat[!is.na(site_name_pa)]$site_name_pa 

	df_habitat[!is.na(site_name_greenery)]$site_name_final <- 
		df_habitat[!is.na(site_name_greenery)]$site_name_greenery  

	df_habitat[!is.na(site_name_parks_all)]$site_name_final <- 
		df_habitat[!is.na(site_name_parks_all)]$site_name_parks_all 

	df_habitat[!is.na(site_name_parks_nr)]$site_name_final <- 
		df_habitat[!is.na(site_name_parks_nr)]$site_name_parks_nr

	df_habitat[!is.na(site_name_island)]$site_name_final <- 
		df_habitat[!is.na(site_name_island)]$site_name_island



	# Create the broader habitat types for the IUCN assessment
	df_habitat <- match_habitat_to_broad_iucn_habitat(df_habitat, "habitat_final")


	# Create note column
	df_habitat$note <- "HABITAT ASSIGNMENT: SPATIAL JOIN"

	# Remove irrelevant columns and return
	relevant_cols <- c(identifier_columns, 
					   "site_name_final", 
					   "habitat_final", 
					   "habitat_IUCN", 
					   "note")
	df_habitat[, ..relevant_cols]

}


generate_final_habitat_nll <- function(df_species_nolatlon, 
									   identifier_columns,
									   habitat_name = "habitat") {

	
	# Original columns
	original_cols <- names(df_species_nolatlon)

	# Create dummy dataset
	cols <- c(identifier_columns, "site_name_final", "habitat_final", "habitat_IUCN", "note")
	df_dummy <- initialize_empty_data_table(cols)

	# If habitat column is not NA, 
	if (!is.na(habitat_name)) {

		# If habitat column defined is present,
		if (habitat_name %in% names(df_species_nolatlon)) {

			# Remove records with no habitat
			df <- df_species_no_latlon[!( get(habitat_name) == "" | is.na(get(habitat_name)) )]
			rm(df_species_no_latlon)

			# Assign habitat column to habitat_IUCN column
			df[, habitat_final := get(habitat_name)]

			# Create dummy variables for manual checking
			df[, site_name_final := ""]
			df[, note := "HABITAT ASSIGNMENT: MANUAL"]

			return(df)

		} else {

			return(df_dummy)
		
		}		

	} else {

		return(df_dummy)

	}

}


generate_habitat <- function(df_species,

							 identifier_columns,
							 coord_columns,

							 df_species_epsg = 4236,
							 
							 vector_layers = list(v_islands = NA, 
												  v_parks_nat_res = NA,
												  v_parks_all = NA,
												  v_greenery = NA,
												  v_planning_areas = NA)) {


	# Separate records with and without lat/lon

	# Those with lat/lon as sf object
	# Those without lat/lon as data.table

	sf_li <- create_species_sf_obj(df_species, coord_columns, df_species_epsg)
	v_species <- sf_li$v_species                       # used in coming up with site species matrix
	df_species_nolatlon <- sf_li$df_species_nolatlon   # check manually to assign habitat and site



	# For records with lat/lon,

	# Make spatial joins between species lat/lon and vector layers
	# Returns data.table not sf object
	df_initial_habitat <- generate_initial_habitat(v_species, vector_layers, identifier_columns)

	# Generate final site name/ habitat for each record
	df_final_habitat <- generate_final_habitat(df_initial_habitat, identifier_columns)



	# For records without lat/lon,

	# Generate final habitat for each record based on habitat column
	df_final_habitat_nll <- generate_final_habitat_nll(df_species_nolatlon, 
													   identifier_columns,
									  			       habitat_name = "habitat")


	# Combine records with and without lat/lon
	df_final_habitat_all <- rbind(df_final_habitat, df_final_habitat_nll)
	
	# Merge back to original data
	df_final_habitat_all <- merge(df_species,
								  df_final_habitat_all,
								  by = identifier_columns, 
								  all.x = T, all.y = T)


	# Return all records for manual checking
	df_final_habitat_all[order(id)]

}


generate_habitat_sp_matrix <- function(df_habitat,
									   collection_date_column = "collection_date", 
									   date_cut_off = as.Date("1960-01-01")) {
	
	# Generate habitat matrix
	# by counting number of unique sites of  habitat for each species


	# Subset data from cut-off date
	df_habitat <- df_habitat[get(collection_date_column) >= date_cut_off]


	# Subset relevant columns
	cols <- c("species", "site_name_final", "habitat_IUCN")
	df_habitat_sp_mat <- df_habitat[, ..cols]
	

	# Only take unique sites
	df_habitat_sp_mat <- unique(df_habitat_sp_mat)


	# Subset only those falling within these categories
	categories_habitat_IUCN <- c("Urban/semi-urban", 
								 "Young secondary", 
								 "Primary/ mature secondary")

	df_habitat_sp_mat <- df_habitat_sp_mat[habitat_IUCN %in% categories_habitat_IUCN]

	df_habitat_sp_mat$habitat_IUCN <- factor(df_habitat_sp_mat$habitat_IUCN, 
										  levels = categories_habitat_IUCN)


	# Reshape data and count number of sites by habitat_IUCN
	df_habitat_sp_mat <- dcast(df_habitat_sp_mat, 
				               species~habitat_IUCN,
					           fun.aggregate = length,
					           value.var = "species")


	# Format habitat names
	names(df_habitat_sp_mat) <- gsub("__", "_", 
					              tolower(
									  gsub("[^[:alnum:]\\-\\.\\s]", "_", names(df_habitat_sp_mat))
									  	 )
								 )

	names(df_habitat_sp_mat)[2:length(names(df_habitat_sp_mat))] <- 
		paste0("n_sites.", names(df_habitat_sp_mat)[2:length(names(df_habitat_sp_mat))])


	# Return matrix
	df_habitat_sp_mat

}


generate_boolean_check_vars <- function(df_species,
									    date_cut_off = as.Date("1960-01-01")) {

	# Create matrix for specimen last record date, N specimens, N reproductive specimens
	# used in boolean checks for red list (first half)
	# based on cut off date, singleton/doubleton of reproductive caste

	df_bool <- df_species[, list(coll_date_last = max(collection_date), 
							  	  n_specimens_non_repro = .N,
							  	  n_specimens_repro = sum(type=="reproductive")), by="species"]

	df_bool$coll_since_cut_off <- ifelse(df_bool$coll_date_last < date_cut_off, "n", "y")

	df_bool

}


generate_iucn_status <- function(df_bool, 
								 df_habitat_sp_mat,
								 site_cut_off = 2) {

	# Rule based assignment following flow chart

	# Combining these datasets
	df_iucn <- merge(df_bool,
				     df_habitat_sp_mat,
					 by = "species", 
					 all.x = T, all.y = T)
	# Note that specimens with records only earlier than the cut-off date will have 0 site
	# occurrences in the df_habitat_sp_mat



	# Get variables with site names
	cols_site <- names(df_iucn)[grep("n_sites", names(df_iucn))]

	# Replace NA by 0
	for (i in 1:length(cols_site)) {
		df_iucn[is.na(get(cols_site[i])), cols_site[i]] <- 0
	}

	# Tabulate total number of sites
	df_iucn$n_sites_total <- rowSums(df_iucn[, ..cols_site])     



	# Boolean checks

	# Date check
	isRecordedSinceMurphy <- df_iucn$coll_since_cut_off == "y"

	# Singleton double check
	isSingletonOrDoubletonReproductive <- 
		df_iucn$n_specimens_repro <= 2 & df_iucn$n_specimens_non_repro <= 0

	# Site check
	isRecordedInTwoOrLessSites <- df_iucn$n_sites_total <= site_cut_off

	# AOO checks
	isAOOinPrimaryMatSec <- df_iucn$n_sites.primary_mature_secondary >= 1
	isAOOinYoungSec <- df_iucn$n_sites.young_secondary >= 1
	isAOOinUrban <- df_iucn$n_sites.urban_semi_urban >= 1

	isAreaOfOccupancyInYoungSecAndPriMatSecOnly <- 
		isAOOinPrimaryMatSec & isAOOinYoungSec & !isAOOinUrban

	isAreaOfOccupancyInYoungSecOnly <- 
		!isAOOinPrimaryMatSec & isAOOinYoungSec & !isAOOinUrban

	isAreaOfOccupancyInPriMatSecOnly <- 
		isAOOinPrimaryMatSec & !isAOOinYoungSec & !isAOOinUrban


	# Creating the final dataset template based on flowchart
	df_final <- data.table(species=df_iucn$species, 
						   isRecordedSinceMurphy,
						   isSingletonOrDoubletonReproductive,
						   isRecordedInTwoOrLessSites,
						   isAOOinUrban,
						   isAreaOfOccupancyInYoungSecAndPriMatSecOnly,
						   isAreaOfOccupancyInYoungSecOnly,
						   isAreaOfOccupancyInPriMatSecOnly)


	# Assessing IUCN statuses using boolean checks
	isDataDeficient1 <- !isRecordedSinceMurphy & isSingletonOrDoubletonReproductive
	isToBeManuallyDefined <- !isRecordedSinceMurphy & !isSingletonOrDoubletonReproductive

	isCriticallyEndangered <- isRecordedSinceMurphy & 
		isRecordedInTwoOrLessSites & 
		isAreaOfOccupancyInPriMatSecOnly

	isDataDeficient2 <- isRecordedSinceMurphy & 
		isRecordedInTwoOrLessSites & 
		!isAreaOfOccupancyInPriMatSecOnly

	isLeastConcern <- isRecordedSinceMurphy & 
		!isRecordedInTwoOrLessSites & 
		isAOOinUrban

	isNearThreatened <- isRecordedSinceMurphy & 
		!isRecordedInTwoOrLessSites & 
		isAreaOfOccupancyInYoungSecAndPriMatSecOnly

	isVulnerable <- isRecordedSinceMurphy & 
		!isRecordedInTwoOrLessSites &
		isAreaOfOccupancyInYoungSecOnly

	isEndangered <- isRecordedSinceMurphy & 
		!isRecordedInTwoOrLessSites & 
		isAreaOfOccupancyInPriMatSecOnly


	# Appending statuses to dataset
	df_final$category_iucn <- "No status"
	df_final[isDataDeficient1]$category_iucn <- "Data Deficient"
	df_final[isToBeManuallyDefined]$category_iucn <- "! MANUAL CHECK (DD/NE)"
	df_final[isCriticallyEndangered]$category_iucn <- "Critically Endangered"
	df_final[isDataDeficient2]$category_iucn <- "Data Deficient"
	df_final[isLeastConcern]$category_iucn <- "Least Concern"
	df_final[isNearThreatened]$category_iucn <- "Near Threatened"
	df_final[isVulnerable]$category_iucn <- "Vulnerable"
	df_final[isEndangered]$category_iucn <- "Endangered"

	# Summarise results and return
	df_final
	
}


#-------------------------------------------------------------------------------------------------

generate_site_names = function() {

	# Column to be used to define site name
	cols <- c("NAME")

	# Get data from each of the geospatial layers
	df_greenery <- get_data_from_sf(get_data_and_assign("v_greenery"))[, ..cols]
	df_greenery$habitat_final <- "GREENERY"

	df_islands <- get_data_from_sf(get_data_and_assign("v_islands"))[, ..cols]
	df_islands$habitat_final <- "ISLAND"

	parks_cols <- c(cols, "habitat")
	df_parks_all <- get_data_from_sf(get_data_and_assign("v_parks_all"))[, ..parks_cols]
	df_parks_all$habitat_final <- paste0("PARKS (ALL) -- ", df$habitat)
	df_parks_all$habitat <- NULL

	df_parks_nat_res <- get_data_from_sf(get_data_and_assign("v_parks_nat_res"))[, ..cols]
	df_parks_nat_res$habitat_final <- "PARKS (NATURE RESERVE)"

	df_planning_area <- get_data_from_sf(get_data_and_assign("v_planning_areas"))[, ..cols]
	df_planning_area$habitat_final <- "PLANNING AREA"

	# Combine these datasets
	df_habitat_sites <- 
		rbind(df_greenery, df_islands, df_parks_all, df_parks_nat_res, df_planning_area)

	# Create the habitat_IUCN variable
	df_habitat_sites <- match_habitat_to_broad_iucn_habitat(df_habitat_sites, "habitat_final")

	df_habitat_sites
	
}