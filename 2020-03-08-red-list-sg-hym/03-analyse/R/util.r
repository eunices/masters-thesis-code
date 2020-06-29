# These are util functions
library(sf)

get_data_from_sf = function (sf_object) {

	st_geometry(sf_object) = NULL


	if ("habitat" %in% names(sf_object)) {

		cols <- c("id", "species", "collection_date", "type", "NAME", "habitat")

	} else {

		cols <- c("id", "species", "collection_date", "type", "NAME")
	}

	data <- sf_object[, cols]
	names(data)[which(names(data)=="NAME")] <- "site_name"


	data[order(as.integer(data$id)),]

}

get_data_and_assign = function(...) {

	e <- new.env()
	name <- data(..., envir = e)[1]

	e[[name]]

}