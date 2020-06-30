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
#' \code{coord_names}, and vector layers which are objects in a list \code{vector_layers}, 
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
#' @param coord_names This is the coordinate names of df_species, which defaults to "X" and "Y" for 
#' longitude and latitude respectively, in the WGS84 context.
#' @param vector_layers This is a list that presents a series of sf objects, which are to be used 
#' as spatial joins to the df_species. This includes the islands, parks, nature reserves, unmanaged
#' greenery and administrative planning areas. If they are NA, the default layers from the package 
#' will be applied. Each of these layers must have a "NAME" column which identifies the site name
#' and will be used to count the number of unique sites.
#' 
#' @return This returns a data.table of species and assigned IUCN statuses. 
#' 
#' @export 
generate_iucn_categories <- function(df_species, 
								     df_species_epsg,

									 coord_names = c("X", "Y"),

									 identifier_columns = c("id", 
									 						"species",
															"collection_date",
															"type"),

								     vector_layers = list(v_islands = NA, 
									 				      v_parks_nat_res = NA,
									 				      v_parks_all = NA,
									 				      v_greenery = NA,
									 				      v_planning_areas = NA)) {

	# Generate habitat matrix
	df_habitat <- generate_habitat(df_species, vector_layers, identifier_columns)

	# Generate habitat matrix (count unique sites) for each species
	df_habitat_sp_mat <- generate_habitat_sp_matrix(df_final_habitat)

	# Generate boolean checks
	df_bool <- generate_boolean_check_vars(df_species)

	# Generate IUCN status
	df_iucn <- generate_iucn_status(df_habitat_mat, df_bool)

	df_iucn
	
}


create_species_sf_obj <- function(df_species,
							      df_species_epsg,
								  coord_names) {

	# Constants
	epsg_svy21 <- 3414

	# Assign an index for merging
	df_species$id <- 1:dim(df_species)[1] 

	# Filter records with lat/lon
	df_species_latlon <- df_species[!is.na(get(coord_names[1])) &
								    !is.na(get(coord_names[2]))]

	# Change species coordinates to sf object
	v_species <- st_as_sf(df_species, coords=coord_names) 
	# needs to have columns "species", "collection_date", "type", and coord columns

	if(is.na(st_crs(v_species))) v_species <- st_set_crs(v_species, df_species_epsg)
	
	v_species <- st_transform(v_species, epsg_svy21)
	


	# Filter results without lat/lon
	df_species_nolatlon <- df_species[is.na(get(coord_names[1])) |
								      is.na(get(coord_names[2]))]



	list(v_species = v_species, 
		 df_species_nolatlon = df_species_nolatlon)

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
	df_islands <- get_data_from_sf(st_join(v_species, v_islands))               # islands
	df_parks_nat_res <- get_data_from_sf(st_join(v_species, v_parks_nat_res))   # parks, nat res
	df_parks_all <- get_data_from_sf(st_join(v_species, v_parks_all))           # parks, others
	df_greenery <- get_data_from_sf(st_join(v_species, v_greenery))             # greenery
	df_planning_areas <- get_data_from_sf(st_join(v_species, v_planning_areas)) # planning areas

									
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

	# Create the final habitat variable for each record
	# base on hierachy of island > parks (nat. res.) > 
	# parks (others) > greenery > planning area > none

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


	# Create the final site name variable
	df_habitat$site_name_final <- "NONE"

	df_habitat[!is.na(site_name_pa)]$site_name_final <- 
		df_habitat[!is.na(site_name_pa)]$site_name_pa 

	df_habitat[!is.na(site_name_greenery)]$habitat_final <- 
		df_habitat[!is.na(site_name_greenery)]$site_name_greenery  

	df_habitat[!is.na(site_name_parks_all)]$site_name_final <- 
		df_habitat[!is.na(site_name_parks_all)]$site_name_parks_all 

	df_habitat[!is.na(site_name_parks_nr)]$site_name_final <- 
		df_habitat[!is.na(site_name_parks_nr)]$site_name_parks_nr

	df_habitat[!is.na(site_name_island)]$site_name_final <- 
		df_habitat[!is.na(site_name_island)]$site_name_island


	# Remove irrelevant columns
	subsetted_columns <- c(identifier_columns, c("site_name_final", "habitat_final"))

	df_habitat <- df_habitat[, ..subsetted_columns]


	# Create the broader habitat types for the IUCN assessment
	df_habitat$habitat_IUCN <- ""

	# Urban/ semi-urban
	df_habitat[grepl("PLANNING AREA", toupper(habitat_final))]$habitat_IUCN <- 
		"Urban/semi-urban"

	df_habitat[grepl("URBAN/SEMI-URBAN", toupper(habitat_final))]$habitat_IUCN <- 
		"Urban/semi-urban"

	# Young secondary
	df_habitat[grepl("GREENERY", toupper(habitat_final))]$habitat_IUCN <- 
		"Young secondary"

	df_habitat[grepl("ISLAND", toupper(habitat_final))]$habitat_IUCN <- 
		"Young secondary"

	df_habitat[grepl("YOUNG SECONDARY FOREST", toupper(habitat_final))]$habitat_IUCN <- 
		"Young secondary"

	# Primary/ mature secondary
	df_habitat[grepl("MANGROVE", toupper(habitat_final))]$habitat_IUCN <- 
		"Primary/ mature secondary"

	df_habitat[grepl("NATURE RESERVE", toupper(habitat_final))]$habitat_IUCN <- 
		"Primary/ mature secondary"


	cols <- unique(c(identifier_columns, "site_name_final", "habitat_IUCN"))
	df_habitat[, ..cols]

}


generate_habitat <- function(df_species,
						         vector_layers, 
							     identifier_columns) {

	# Create species sf object
	sf_li <- create_species_sf_obj(df_species, df_species_epsg, coord_names)
	v_species <- sf_li$v_species                       # used in coming up with site species matrix
	df_species_nolatlon <- sf_li$df_species_nolatlon   # check manually to assign habitat and site

	# Create spatial joins between species lat/lon and vector layers
	# returns a data.table not sf object
	df_initial_habitat <- generate_initial_habitat(v_species, vector_layers, identifier_columns)

	# Generate final site name/ habitat for each record
	df_final_habitat <- generate_final_habitat(df_initial_habitat, identifier_columns)

	df_final_habitat

}


generate_habitat_sp_matrix <- function(df_habitat_final) {
	
	# Generate habitat matrix
	# by counting number of unique sites of  habitat for each species

	cols <- c("species", "site_name_final", "habitat_IUCN")
	df_habitat_mat <- unique(df_habitat_final[, ..cols])

	df_habitat_mat <- dcast(df_habitat_mat, 
				            species~habitat_IUCN,
					        fun.aggregate = length,
					        value.var = "site_name_final")

	# Format habitat names
	names(df_habitat_mat) <- gsub("__", "_", 
					              tolower(
									  gsub("[^[:alnum:]\\-\\.\\s]", "_", names(df_habitat_mat))
									  	 )
								 )

	names(df_habitat_mat)[2:length(names(df_habitat_mat))] <- 
		paste0("n_sites_", names(df_habitat_mat)[2:length(names(df_habitat_mat))])

	df_habitat_mat

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


generate_iucn_status <- function(df_habitat,
								 df_bool,
								 site_cut_off = 2) {

	# Rule based assignment following flow chart

	# Combining these datasets
	df_iucn <- merge(df_habitat,
				     df_boolean,
					 by = "species", 
					 all.x = T, all.y = T)

	cols_site <- names(df_iucn)[grep("n_sites", names(df_iucn))] # renaming col names

	df_iucn$n_sites_total <- rowSums(df_iucn[, ..cols_site])     # tabulating row sums


	# Boolean checks

	# Date check
	isRecordedSinceMurphy <- df_iucn$coll_since_cut_off == "y"

	# Singleton double check
	isSingletonOrDoubletonReproductive <- 
		df_iucn$n_specimens_repro <= 2 & df_iucn$n_specimens_non_repro <= 0

	# Site check
	isRecordedInTwoOrLessSites <- df_iucn$n_sites_total <= site_cut_off

	# AOO checks
	isAOOinPrimaryMatSec <- df_iucn$n_sites_primary_mature_secondary >= 1
	isAOOinYoungSec <- df_iucn$n_sites_young_secondary >= 1
	isAOOinUrban <- df_iucn$n_sites_urban_semi_urban >= 1

	isAreaOfOccupancyInAllThree <- 
		isAOOinPrimaryMatSec & isAOOinYoungSec & isAOOinUrban

	isAreaOfOccupancyInUrbanOnly <- 
		!isAOOinPrimaryMatSec & !isAOOinYoungSec & isAOOinUrban

	isAreaOfOccupancyInAllThreeOrUrban <- 
		isAreaOfOccupancyInAllThree | isAreaOfOccupancyInUrbanOnly

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
						   isAreaOfOccupancyInAllThreeOrUrban,
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
		isAreaOfOccupancyInAllThreeOrUrban

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
	df_final$category_IUCN <- "No status"
	df_final[isDataDeficient1]$category_IUCN <- "Data Deficient"
	df_final[isToBeManuallyDefined]$category_IUCN <- "! MANUAL CHECK (DD/NE)"
	df_final[isCriticallyEndangered]$category_IUCN <- "Critically Endangered"
	df_final[isDataDeficient2]$category_IUCN <- "Data Deficient"
	df_final[isLeastConcern]$category_IUCN <- "Least Concern"
	df_final[isNearThreatened]$category_IUCN <- "Near Threatened"
	df_final[isVulnerable]$category_IUCN <- "Vulnerable"
	df_final[isEndangered]$category_IUCN <- "Endangered"

	# Summarise results and return
	df_final

}


#-------------------------------------------------------------------------------------------------
