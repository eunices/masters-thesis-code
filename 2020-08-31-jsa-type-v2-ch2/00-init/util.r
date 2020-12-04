read_escaped_data_v2 = function(filepath, escape=T) {

    df <- fread(
        filepath, 
        integer64 = 'character', 
        na.strings = c(''), 
        encoding = 'UTF-8'
    )
    
    if(escape) {
        df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
    }

    if("idx" %in% names(df)) {
        df$idx <- as.integer(df$idx)
    }

    df
}


parse_model_identifier <- function(string) {

    #' Parses model identifier into a list of parameters

    #' Parses a specific model identifier into a list of parameter.
    #'
    #' @param string The string that is to be decomposed into a list of
    #' parameters.

    model_params <- list()
    string <- strsplit(string, "-")[[1]]

    for (i in 1:length(string)){

        if (i == 1) {

            model_params$dataset <- substr(string[i], 1, 2)
            model_params$ll <- substr(string[i], 3, 3)          
            # "Y" for using lat lon / "N" for using country distribution

        } else {                                

            first_letter <- substr(string[i], 1, 1)
			parameter <- as.numeric(substr(string[i], 2, nchar(string[i])))

            if (first_letter == "E" ) { # taxonomic effort
                model_params$te <- parameter
                  
            }

            if (first_letter == "C") { # chains
                model_params$chains <- parameter
            }

            if (first_letter == "I") { # iterations
                model_params$iter <- parameter
            } 

            if (first_letter == "A") { # adapt delta
                model_params$ad <- parameter
            } 

            if (first_letter == "T") { # tree depth
                model_params$td <- parameter
            } 

			if (first_letter == "F") {
				model_params$fc <- parameter # forecast duration
			}

			if (first_letter == "V") {
				model_params$va <- parameter # number of year to be left out
			}

        }
    }

    model_params
}

# Test
# string <- "BMY-C4-I5000-A0.99-T15"
# parse_model_identifier(string)


initialize_model_params <- function(model_params, custom=NA) {

    #' Parses list of model params and initializes necessary folders
    #'
    #' Parses list of model params into a model identifier and
    #' initializes necessary folders. Returns the model folder directory.
    #' @model_params list The list that contains a list of model parameters.

	model_identifier <- paste0(
		model_params$dataset, model_params$ll, "-",
		"E", as.character(model_params$te), "-",
		"C", as.character(model_params$chains), "-",
		"I", as.character(model_params$iter), "-",
		"A", as.character(model_params$ad), "-",
		"T", as.character(model_params$td), "-",
		"F", as.character(model_params$fc), "-",
        "V", as.character(model_params$va)
        
	)
  
	# Define model folder
	folder <- dir_analysis_edie_model

	dir_model_folder <- paste0(folder, "/", 
							   model_identifier, "/")
							   
	if(!is.na(custom)) dir_model_folder <- custom

	# Create folders
	dir.create(dir_model_folder)

	dir.create(file.path(dir_model_folder, 'output'))

	# Create log files
	filepath_log <- paste0(dir_model_folder, "/model.log")
	if (!file.exists(filepath_log)) file.create(filepath_log)

	warnings_log <- paste0(dir_model_folder, "/warnings.log")
	if (!file.exists(warnings_log)) file.create(warnings_log)

	c(dir_model_folder, 
	  filepath_log,
	  warnings_log,
	  model_identifier)

} 


write_to_log <- function(w, warn_log_fp) {

    #' Writes warning to warning logfile

    #' Writes warning to logfile in specified path.
	#' 
    #' @param w Warning output from a try-catch block.
    #' @param warn_log_fp Warning log filepath. Should be a .log file. 

    write(conditionMessage(w), file = warn_log_fp, append = TRUE)
}

