source('2020-08-31-jsa-type-v2-ch2/01-prep/init.r')
print(paste0(Sys.time(), " --- format data for stan"))


# Read dataframes

# Unique species (year + author) assigned to groups
input_filepath <- paste0(dir_model_folder, "data.csv")
data <- fread(input_filepath, na = c(''), encoding = "UTF-8")
# Summarised number of offset (e.g. publication or author) for each year
input_filepath <- paste0(dir_model_folder, "offset.csv")
off <- fread(input_filepath, na = c(''), encoding = "UTF-8")

# Count matrix

# Count number of species in each group for each year
counts <- data[, list(.N), by = c("group", "year")]

# Reshape from long to wide
count_df <- dcast(counts, year ~ group, value.var="N")

# Merge to template
count_df <- merge(
    data.frame(year = min(data$year):max(data$year)), 
    count_df, 
    by = "year", all.x = T, all.y = F
)

# Append 0 for NAs in data frame
count_df[is.na(count_df)] <- 0
count_mat <- as.matrix(count_df)

# Create matrix and set row names
rownames <- count_mat[, 1]
if(dim(count_mat)[2] <= 2){
    count_mat <- matrix(count_mat[, 2], ncol=1)
    row.names(count_mat) <- rownames
} else {
    row.names(count_mat) <- rownames
    count_mat <- count_mat[, -1]
}


# Offset matrix

# Reshape from long to wide
off_df <- dcast(off, year ~ group, value.var = "N", fun.aggregate = sum)

off_df <- merge(
    data.frame(year = min(data$year):max(data$year)),
    off_df, 
    by = "year", 
    all.x = T, all.y = F
)

off_df[is.na(off_df)] <- 0
off_mat <- as.matrix(off_df)

# Create as matrix and set row names
rownames <- off_mat[, 1]
if(dim(off_mat)[2] <= 2){
    off_mat <- matrix(off_mat[, 2], ncol=1)
    row.names(off_mat) <- rownames
} else {
    row.names(off_mat) <- rownames
    off_mat <- off_mat[, -1]
}

if(model_params$te == 0){
    off_mat[] <- 0 # set offset to zero
}

# note: model_params$te == 0 # no taxonomic effort
#       model_params$te == 1 # taxonomic effort, by publications

########################################
# OUTPUT
########################################

# Other stan variables

nyear <- nrow(count_mat)
jgroup <- ncol(count_mat)

# index where value is not 0 to use as a starting point
starts <- apply(count_mat, 2, function(x) min(which(x != 0))) 

data <- list(
    N = nyear, P = jgroup, str = as.numeric(starts), 
    end = rep(max(dim(count_mat)[1]), jgroup), 
    counts =  t(count_mat), off = t(off_mat)
)

output_filepath <- paste0(dir_model_folder, "count_info_ref.data.R")

with(
    data, 
    {stan_rdump(
        list = c('N', 'P', 'str', 'end', 'counts', 'off'),
        file = output_filepath
    )} 
)


# Modify data for validation
count_mat <- count_mat[1:(dim(count_mat)[1] - model_params$va),]
off_mat <- off_mat[1:dim(count_mat)[1],]

# Other stan variables
nyear <- nrow(count_mat)
jgroup <- ncol(count_mat)

# index where value is not 0 to use as a starting point
starts <- apply(count_mat, 2, function(x) min(which(x != 0))) 

########################################
# OUTPUT
########################################

data <- list(
    N = nyear, P = jgroup, str = as.numeric(starts), 
    end = rep(max(dim(count_mat)[1]), jgroup), 
    counts =  t(count_mat), off = t(off_mat)
)

output_filepath <- paste0(dir_model_folder, "count_info.data.R")

with(
    data, 
    {stan_rdump(
        list = c('N', 'P', 'str', 'end', 'counts', 'off'),
        file = output_filepath
    )} 
)

