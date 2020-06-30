# These are util functions
library(sf)

get_data_from_sp_sf <- function (sf_object) {

	data <- get_data_from_sf(sf_object)


	if ("habitat" %in% names(data)) {

		cols <- c("id", "species", "collection_date", "type", "NAME", "habitat")

	} else {

		cols <- c("id", "species", "collection_date", "type", "NAME")
	}

	data <- data[, ..cols]
	names(data)[which(names(data)=="NAME")] <- "site_name"

	data[order(as.integer(data$id)),]

}

get_data_from_sf <- function(sf_object) {
	st_geometry(sf_object) = NULL
	data.table(sf_object)
}


get_data_and_assign <- function(...) {

	e <- new.env()
	name <- data(..., envir = e)[1]

	e[[name]]

}

initialize_empty_data_table <- function(column_names) {
	data.table(NA)[, `:=` (eval(column_names), NA)][!is.na(V1)][, V1:=NULL]
}