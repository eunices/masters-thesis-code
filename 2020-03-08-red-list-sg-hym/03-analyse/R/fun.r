
source(paste0("2020-03-08-red-list-sg-hym/03-analyse/R/", "init.r"))

library(sf)
library(data.table)


# TODO: put into package format
# TODO: write tests

# TODO: how to deal with missing data
# TODO: generate random points

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
classify_parks = function(min_park_size=10000, min_unmanaged_tree_area=10000, 
						  min_vegetation_percentage=80) {

	p = st_read(g_parks_all)

	# Parameters
	threshold_size = min_park_size
	threshold_unmanaged_trees = min_unmanaged_tree_area / 5  # 5m^2/pixel
	threshold_vegetation = min_vegetation_percentage

	# Boolean checks
	isLarge = p$AREA_SQ_M > threshold_size
	isMangrove = p$mangrove > 1
	isForestCover = p$veg_canopy_unmanaged >= threshold_unmanaged_trees &
		p$prop_veg > threshold_vegetation

	# Assignment
	p[!isLarge,]$habitat = "SMALL GREEN SPACE"                                   # Small parks
	p[isLarge & isMangrove,]$habitat = "MANGROVE"                                # Mangrove
	p[isLarge & !isMangrove & !isForestCover,]$habitat = "URBAN/SEMI-URBAN"      # Urban/semi-urban
	p[isLarge & !isMangrove & isForestCover,]$habitat = "YOUNG SECONDARY FOREST" # Young secondary

	print(table(isLarge))
	print(table(p$habitat))

	return(p)
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
generate_iucn_categories = function(df_species, df_species_epsg, coord_names=c("X", "Y"), 
								    vector_layers=list(v_islands=NA, 
													   v_parks_nat_res=NA,
													   v_parks_all=NA,
													   v_greenery=NA,
													   v_planning_areas=NA)) {

	# Load layers
	if(is.na(vector_layers$v_islands)) {
		v_islands = st_read(g_islands)
	} else {
		v_islands = vector_layers$v_islands
	} 
	if(is.na(vector_layers$v_parks_nat_res)) {
		v_parks_nat_res = st_read(g_parks_nat_res)
	} else {
		v_parks_nat_res = vector_layers$v_parks_nat_res
	}
	if(is.na(vector_layers$v_parks_all)) {
		v_parks_all = st_read(g_parks_all)
	} else {
		v_parks_all = vector_layers$v_parks_all
	}
	if(is.na(vector_layers$v_greenery)) {
		v_greenery = st_read(g_greenery)
	} else {
		v_greenery = vector_layers$v_greenery
	}
	if(is.na(vector_layers$v_planning_areas)) {
		v_planning_areas = st_read(g_planning_areas)
	} else {
		v_planning_areas = vector_layers$v_planning_areas
	}

	# Parameters
	identifier_columns = c("id", "species", "collection_date", "type")

	# Create species sf  object
	v_species = create_species_sf_obj(df_species, df_species_epsg, coord_names)

	# Create spatial joins
	df_islands = get_data_from_sf(st_join(v_species, v_islands))               # islands
	df_parks_nat_res = get_data_from_sf(st_join(v_species, v_parks_nat_res))   # parks, nat res
	df_parks_all = get_data_from_sf(st_join(v_species, v_parks_all))           # parks, others
	df_greenery = get_data_from_sf(st_join(v_species, v_greenery))             # greenery
	df_planning_areas = get_data_from_sf(st_join(v_species, v_planning_areas)) # planning areas

	# Generate inital habitat dataset 
	df_habitat = generate_initial_habitat(df_islands, 
				 						  df_parks_nat_res,
				 	                      df_parks_all, 
				 						  df_greenery,
				 	                  	  df_planning_areas,
   										  identifier_columns)

	# Generate final site name/ habitat
	df_habitat = generate_final_habitat(df_habitat, identifier_columns)

	# Generate IUCN status
	df_iucn = generate_iucn_status(df_habitat)

	return(df_iucn)
}


create_species_sf_obj = function(df_species, df_species_epsg, coord_names) {

	# Constants
	epsg_svy21 = 3414

	# Change species coordinates to sf object
	v_species = st_as_sf(df_species, coords=coord_names) 
	# needs to have columns "species", "collection_date", "type", and coord columns
	if(is.na(st_crs(v_species))) v_species = st_set_crs(v_species, df_species_epsg)
	v_species = st_transform(v_species, epsg_svy21)
	v_species$id = 1:dim(v_species)[1] # assign an index for merging

	return(v_species)
}


generate_initial_habitat = function(df_islands, df_parks_nat_res,
									df_parks_all, df_greenery,
									df_planning_areas, identifier_columns) {

	# Create one dataset from all the spatial joins
	df_habitat = merge(df_islands, df_parks_nat_res, by=identifier_columns, 
				   all.x=T, all.y=T, suffixes=c("_island", "_parks_nr"))
	df_habitat = merge(df_habitat, df_parks_all, by=identifier_columns, 
					all.x=T, all.y=T, suffixes=c("", "_parks_all"))
	names(df_habitat)[which(names(df_habitat)=="site_name")] = "site_name_parks_all"
	df_habitat = merge(df_habitat, df_greenery, by=identifier_columns, 
					all.x=T, all.y=T, suffixes=c("", "_greenery"))
	names(df_habitat)[which(names(df_habitat)=="site_name")] = "site_name_greenery"
	df_habitat = merge(df_habitat, df_planning_areas, by=identifier_columns,
					all.x=T, all.y=T, suffixes=c("", "_pa"))
	names(df_habitat)[which(names(df_habitat)=="site_name")] = "site_name_pa"
	
	return(data.table(df_habitat))
}


generate_final_habitat = function(df_habitat, identifier_columns) {

	# Create the final habitat variable
	df_habitat$habitat_final = 
		"NONE"
	df_habitat[!is.na(site_name_pa)]$habitat_final = 
		"PLANNING AREA"
	df_habitat[!is.na(site_name_greenery)]$habitat_final =  # outside of parks/ nat. res.
		"GREENERY"
	df_habitat[!is.na(site_name_parks_all)]$habitat_final = 
		paste0("PARKS (ALL) -- ", df_habitat[!is.na(site_name_parks_all)]$habitat)
	df_habitat[!is.na(site_name_parks_nr)]$habitat_final =
		"PARKS (NATURE RESERVE)"
	df_habitat[!is.na(site_name_island)]$habitat_final = 
		"ISLAND"

	# Create the final site name variable
	df_habitat$site_name_final = "NONE"
	df_habitat[!is.na(site_name_pa)]$site_name_final = 
		df_habitat[!is.na(site_name_pa)]$site_name_pa 
	df_habitat[!is.na(site_name_greenery)]$habitat_final =
		df_habitat[!is.na(site_name_greenery)]$site_name_greenery  
	df_habitat[!is.na(site_name_parks_all)]$site_name_final = 
		df_habitat[!is.na(site_name_parks_all)]$site_name_parks_all 
	df_habitat[!is.na(site_name_parks_nr)]$site_name_final = 
		df_habitat[!is.na(site_name_parks_nr)]$site_name_parks_nr
	df_habitat[!is.na(site_name_island)]$site_name_final = 
		df_habitat[!is.na(site_name_island)]$site_name_island

	# Remove irrelevant columns
	subsetted_columns = c(identifier_columns, c("site_name_final", "habitat_final"))
	df_habitat = df_habitat[, ..subsetted_columns]

	# Create the broader habitat types for the IUCN assessment
	df_habitat$habitat_IUCN = ""

	# Urban/ semi-urban
	df_habitat[grepl("PLANNING AREA", toupper(habitat_final))]$habitat_IUCN = 
		"Urban/semi-urban"
	df_habitat[grepl("URBAN/SEMI-URBAN", toupper(habitat_final))]$habitat_IUCN = 
		"Urban/semi-urban"

	# Young secondary
	df_habitat[grepl("GREENERY", toupper(habitat_final))]$habitat_IUCN = 
		"Young secondary"
	df_habitat[grepl("ISLAND", toupper(habitat_final))]$habitat_IUCN = 
		"Young secondary"
	df_habitat[grepl("YOUNG SECONDARY FOREST", toupper(habitat_final))]$habitat_IUCN = 
		"Young secondary"

	# Primary/ mature secondary
	df_habitat[grepl("MANGROVE", toupper(habitat_final))]$habitat_IUCN = 
		"Primary/ mature secondary"
	df_habitat[grepl("NATURE RESERVE", toupper(habitat_final))]$habitat_IUCN = 
		"Primary/ mature secondary"

	return(df_habitat)
}


generate_iucn_status = function(df_habitat) {

	# Rule based assignment following flow chart

	# Constant
	date_cut_off = as.Date("1960-01-01")
	
	# Specimens last record date and number of specimens
	df_iucn1 = df_habitat[, list(coll_date_last = max(collection_date), 
								coll_since_1960 = ifelse(max(collection_date)<date_cut_off, "n", "y"),
								n_specimens_non_repro = .N,
								n_specimens_repro = sum(type=="reproductive")), by="species"]

	# Number of unique sites for habitats
	df_iucn2 = unique(df_habitat[, c("species", "site_name_final", "habitat_IUCN")])
	df_iucn2 = dcast(df_iucn2, species~habitat_IUCN, fun.aggregate=length, value.var="site_name_final")
	names(df_iucn2) = gsub("__", "_", tolower(gsub("[^[:alnum:]\\-\\.\\s]", "_", names(df_iucn2))))
	names(df_iucn2)[2:length(names(df_iucn2))] = 
		paste0("n_sites_", names(df_iucn2)[2:length(names(df_iucn2))])

	# Combining these datasets
	df_iucn = merge(df_iucn1, df_iucn2, by="species", all.x=T, all.y=T)
	cols_site = names(df_iucn)[grep("n_sites", names(df_iucn))] # renaming col names
	df_iucn$n_sites_total = rowSums(df_iucn[, ..cols_site])     # tabulating row sums

	# Boolean checks
	isRecordedSinceMurphy = df_iucn$coll_since_1960 == "y"
	isSingletonOrDoubletonReproductive = df_iucn$n_specimens_repro <= 2 & df_iucn$n_specimens_non_repro <= 0
	isRecordedInTwoOrLessSites = df_iucn$n_sites_total <= 2

	isAOOinPrimaryMatSec = df_iucn$n_sites_primary_mature_secondary >= 1
	isAOOinYoungSec = df_iucn$n_sites_young_secondary >= 1
	isAOOinUrban = df_iucn$n_sites_urban_semi_urban >= 1

	isAreaOfOccupancyInAllThree = isAOOinPrimaryMatSec & isAOOinYoungSec & isAOOinUrban
	isAreaOfOccupancyInUrbanOnly = !isAOOinPrimaryMatSec & !isAOOinYoungSec & isAOOinUrban

	isAreaOfOccupancyInAllThreeOrUrban = isAreaOfOccupancyInAllThree | isAreaOfOccupancyInUrbanOnly
	isAreaOfOccupancyInYoungSecAndPriMatSecOnly = isAOOinPrimaryMatSec & isAOOinYoungSec & !isAOOinUrban
	isAreaOfOccupancyInYoungSecOnly = !isAOOinPrimaryMatSec & isAOOinYoungSec & !isAOOinUrban
	isAreaOfOccupancyInPriMatSecOnly = isAOOinPrimaryMatSec & !isAOOinYoungSec & !isAOOinUrban

	# Creating the final dataset based on flowchart
	df_final = data.table(species=df_iucn$species, 
						isRecordedSinceMurphy,
						isSingletonOrDoubletonReproductive,
						isRecordedInTwoOrLessSites,
						isAreaOfOccupancyInAllThreeOrUrban,
						isAreaOfOccupancyInYoungSecAndPriMatSecOnly,
						isAreaOfOccupancyInYoungSecOnly,
						isAreaOfOccupancyInPriMatSecOnly)

	# Assessing IUCN statuses

	## Boolean checks
	isDataDeficient1 = !isRecordedSinceMurphy & isSingletonOrDoubletonReproductive
	isToBeManuallyDefined = !isRecordedSinceMurphy & !isSingletonOrDoubletonReproductive
	isCriticallyEndangered = isRecordedSinceMurphy & 
		isRecordedInTwoOrLessSites & 
		isAreaOfOccupancyInPriMatSecOnly
	isDataDeficient2 = isRecordedSinceMurphy & 
		isRecordedInTwoOrLessSites & 
		!isAreaOfOccupancyInPriMatSecOnly
	isLeastConcern = isRecordedSinceMurphy & 
		!isRecordedInTwoOrLessSites & 
		isAreaOfOccupancyInAllThreeOrUrban
	isNearThreatened = isRecordedSinceMurphy & 
		!isRecordedInTwoOrLessSites & 
		isAreaOfOccupancyInYoungSecAndPriMatSecOnly
	isVulnerable = isRecordedSinceMurphy & 
		!isRecordedInTwoOrLessSites &
		isAreaOfOccupancyInYoungSecOnly
	isEndangered = isRecordedSinceMurphy & 
		!isRecordedInTwoOrLessSites & 
		isAreaOfOccupancyInPriMatSecOnly

	## Appending statuses to dataset
	df_final$category_IUCN = "No status"
	df_final[isDataDeficient1]$category_IUCN = "Data Deficient"
	df_final[isToBeManuallyDefined]$category_IUCN = "! MANUAL CHECK (DD/NE)"
	df_final[isCriticallyEndangered]$category_IUCN = "Critically Endangered"
	df_final[isDataDeficient2]$category_IUCN = "Data Deficient"
	df_final[isLeastConcern]$category_IUCN = "Least Concern"
	df_final[isNearThreatened]$category_IUCN = "Near Threatened"
	df_final[isVulnerable]$category_IUCN = "Vulnerable"
	df_final[isEndangered]$category_IUCN = "Endangered"

	# Summarise results and return
	return(df_final[, c("species", "category_IUCN")])
}

