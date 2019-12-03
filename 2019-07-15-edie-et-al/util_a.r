parse_model_identifier <- function(string) {

    #' Parses model identifier into a list of parameters

    #' Parses a specific model identifier into a list of parameter.
    #' @param string The string that is to be decomposed into a list of parameters.

    model_params <- list()
    string <- strsplit(string, "-")[[1]]

    for (i in 1:length(string)){
        if (i == 1) {
            model_params$dataset <- substr(string[i], 1, 2)
            model_params$ll <- substr(string[i], 3, 3)
        } else {
            first_letter <- substr(string[i], 1, 1)
            if (first_letter == "E" ) { # taxonomic effort
                model_params$te <- as.numeric(substr(string[i], 2, nchar(string[i])))
            }
            if(first_letter == "C") { # chains
                model_params$chains <- as.numeric(substr(string[i], 2, nchar(string[i])))
            }
            if (first_letter == "I") { # iterations
                model_params$iter <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } 
            if (first_letter == "A") { # adapt delta
                model_params$ad <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } 
            if (first_letter == "T") { # tree depth
                model_params$td <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } 
        }
    }
    model_params
}

# Test
# string <- "BMY-C4-I5000-A0.99-T15"
# parse_model_identifier(string)


write_to_log <- function(w, warn_log_fp) {

    #' Writes warning to warning logfile.

    #' Writes warning to logfile in specified path. 
    #' @param w Warning output from a try-catch block.
    #' @param warn_log_fp Warning log filepath. Should be a .log file. 

    write(conditionMessage(w), file=warn_log_fp, append=T)
}


