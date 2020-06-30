match_habitat_to_broad_iucn_habitat <- function(df_habitat,
											    habitat_col = "habitat_final") {
    
    df_habitat$habitat_IUCN <- ""


	# Those that are in geospatial layers,

	# Urban/ semi-urban
	df_habitat[grepl("PLANNING AREA", toupper(get(habitat_col)))]$habitat_IUCN <- 
		"Urban/semi-urban"

	df_habitat[grepl("URBAN/SEMI-URBAN", toupper(get(habitat_col)))]$habitat_IUCN <- 
		"Urban/semi-urban"

	# Young secondary
	df_habitat[grepl("GREENERY", toupper(get(habitat_col)))]$habitat_IUCN <- 
		"Young secondary"

	df_habitat[grepl("ISLAND", toupper(get(habitat_col)))]$habitat_IUCN <- 
		"Young secondary"

	df_habitat[grepl("YOUNG SECONDARY FOREST", toupper(get(habitat_col)))]$habitat_IUCN <- 
		"Young secondary"

	# Primary/ mature secondary
	df_habitat[grepl("MANGROVE", toupper(get(habitat_col)))]$habitat_IUCN <- 
		"Primary/ mature secondary"

	df_habitat[grepl("NATURE RESERVE", toupper(get(habitat_col)))]$habitat_IUCN <- 
		"Primary/ mature secondary"


	# Those that are not in geospatial layers,
	## Nothing yet ##


	# Return dataset

    df_habitat

}