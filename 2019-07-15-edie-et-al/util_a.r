parse_model_identifier <- function(string) {
    model_params <- list()
    string <- strsplit(string, "-")[[1]]

    for (i in 1:length(string)){
        if (i == 1) {
            model_params$dataset <- substr(string[i], 1, 2)
            model_params$ll <- substr(string[i], 3, 3)
        } else {
            first_letter <- substr(string[i], 1, 1)
            if(first_letter == "C") {
                model_params$chains <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } else if (first_letter == "I") {
                model_params$iter <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } else if (first_letter == "A") {
                model_params$ad <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } else if (first_letter == "T") {
                model_params$td <- as.numeric(substr(string[i], 2, nchar(string[i])))
            }
        }
    }
    model_params
}

# Test
# string <- "BMY-C4-I5000-A0.99-T15"
# parse_model_identifier(string)
