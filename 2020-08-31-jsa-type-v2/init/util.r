closestWordMatch = function(string, stringVector){

  distance = levenshteinSim(string, stringVector)

  stringVector[distance == max(distance)][1] # gets the closest 

}

format_names = function(vec) {

    # Remove punctuations
    vec <- gsub("\\.\\.", "\\.",                     # remove double ..
        gsub(" ", ".",                               # remove spaces
            gsub("[[:punct:]]", "", tolower(vec))    # remove punctuation
        )
    )

    # Convert "funky" alphabets
    vec <- iconv(vec, from = 'UTF-8', to = 'ASCII//TRANSLIT') 

    # Renaming this long name
    is_name_full_name <- grepl("full.name.a.e", vec)

    if (any(is_name_full_name)) {
        vec[which(is_name_full_name)] <- 'full.name' 
    }
    
    vec
}


read_escaped_data_v2 = function(filepath, escape=T) {

    df <- fread(
        filepath, 
        integer64 = 'character', 
        na.strings = c('', 'NA'), 
        encoding = 'UTF-8'
    )
    
    if(escape) {
        df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
    }

    df
}