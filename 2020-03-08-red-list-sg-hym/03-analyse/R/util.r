library(sf)

get_data_from_sf = function (sf_object) {

	st_geometry(sf_object) = NULL
	if("habitat" %in% names(sf_object)) {
		cols = c("id", "species", "collection_date", "type", "NAME", "habitat")
	} else {
		cols = c("id", "species", "collection_date", "type", "NAME")
	}
	data = sf_object[, cols]
	names(data)[which(names(data)=="NAME")] = "site_name"

	return(data[order(as.integer(data$id)),])
}