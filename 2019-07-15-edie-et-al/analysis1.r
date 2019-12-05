# format data for stan
source('2019-07-15-edie-et-al/init_a.r')
print(paste0(Sys.time(), " --- format data  for stan.r"))

########################################
# PARAMETERS AND READING DATA
########################################

# Read dataframe
input_filepath <- paste0(dir_model_folder, "data.csv")
data <- fread(input_filepath, na=c(''), encoding="UTF-8")

# Publications
df_publications <- get_pub(write=F)
pubs <- data.table(df_publications %>% separate_rows(idxes, sep="; "))
pubs <- unique(pubs[, c("date.n", "idxes", "paper.authors", "journal", 
                        "title", "volume", "issue","page.numbers.publication")])
names(pubs)[which(names(pubs)=="date.n")] <- "year"
pubs$idxes <- as.numeric(pubs$idxes)


########################################
# NEWER CODE, not using loops
########################################

# Create one matrix for each group
counts <- data[, list(.N), by=c("group", "year")]
count.matrix <- dcast(counts, year ~ group, value.var="N")
count.matrix <- merge(data.frame(year=min(data$year):max(data$year)), 
                      count.matrix, 
                      by="year", all.x=T, all.y=F)
count.matrix[is.na(count.matrix)] <- 0; count.matrix <- as.matrix(count.matrix)
rownames <- count.matrix[, 1]
if(dim(count.matrix)[2] <= 2){
    count.matrix <- matrix(count.matrix[, 2], ncol=1)
    row.names(count.matrix) <- rownames
} else {
    row.names(count.matrix) <- rownames
    count.matrix <- count.matrix[, -1]
}

# Get data into stan format
nyear <- nrow(count.matrix); jgroup <- ncol(count.matrix); npred <- 1
# index where value is not 0 to use as a starting point
starts <- apply(count.matrix, 2, function(x) min(which(x != 0))) 
cc <- t(count.matrix) 
N <- ncol(cc)
P <- nrow(cc)

# number of publications per year
dfpub <- merge(data[, c("valid_species_id", "group")], pubs, 
              by.x="valid_species_id", by.y="idxes", all.x=T, all.y=F); dim(dfpub)
dfpub <- unique(dfpub[, c("group", "year", "paper.authors", "journal", "title",
         "volume", "issue", "page.numbers.publication")]); dim(dfpub)
npub <- dfpub[, list(N=.N), by=c("group", "year")]
pub.matrix <- dcast(npub, year ~ group, value.var="N")
pub.matrix <- merge(data.frame(year=min(data$year):max(data$year)),
                    pub.matrix, 
                    by="year", all.x=T, all.y=F)
pub.matrix[is.na(pub.matrix)] <- 0; pub.matrix <- as.matrix(pub.matrix)
rownames <- pub.matrix[, 1]

if(dim(pub.matrix)[2] <= 2){
    pub.matrix <- matrix(pub.matrix[, 2], ncol=1)
    row.names(pub.matrix) <- rownames
} else {
    row.names(pub.matrix) <- rownames
    pub.matrix <- pub.matrix[, -1]
}

# model_params$te == 0 # no taxonomic effort
# model_params$te == 1 # taxonomic effort, by publications
# in future, model_params$te == 2 # taxonomic effort, by taxonomists
if(model_params$te == 0){ # no taxonomic effort
    pub.matrix[] <- 0 # set offset to zero
}

data <- list(N = N, P = P, str = as.numeric(starts), end = rep(max(dim(count.matrix)[1]), P), 
             counts = cc, off = t(pub.matrix))


########################################
# OUTPUT
########################################

output_filepath <- paste0(dir_model_folder, "count_info.data.R")

with(data, {stan_rdump(list = c('N', 'P', 'str', 'end', 'counts', 'off'),
    file = output_filepath)} )



















########################################
# OLDER CODE, using publications wrongly
########################################

original_code <- function() {
    # Original code    
    duration <- seq(from = min(data$year), to = max(data$year))
    group <- split(data, data$group)

    counts <- llply(group, function(x) table(x$year))
    counts <- llply(counts, function(x) {
                    pos <- rep(0, length(duration))
                    names(pos) <- duration
                    pos[duration %in% names(x)] <- x
                    pos})
    count.matrix <- Reduce(cbind, counts)  # named per year in a group
    if (is.null(dim(count.matrix))){
        print("Null matrix")
        rownames <- names(count.matrix)
        count.matrix <- matrix(count.matrix, ncol=1)
        row.names(count.matrix) <- rownames
    }

    # actually fit a model
    # got to get data in stan format
    nyear <- nrow(count.matrix)
    jgroup <- ncol(count.matrix)
    npred <- 1


    starts <- apply(count.matrix, 2, function(x) min(which(x != 0)))
    data <- list()
    for(ii in seq(jgroup)) {
        long <- length(count.matrix[, ii])
        data[[ii]] <- list(counts = count.matrix[seq(from = starts[ii], 
                                                    to = long), ii],
                            N = length(seq(from = starts[ii], to = long)))
    }
    years.named <- llply(data, function(x) names(x$counts))


    cc <- list()
    len <- laply(data, function(x) length(x$counts))
    for(ii in seq(length(data))) {
        cc[[ii]] <- c(rep(0, (max(len) - len[ii])), data[[ii]]$counts)
    }
    if (length(cc)==1){
        print("Null matrix")
        col_names <- names(cc[[1]])
        cc <- matrix(cc[[1]], nrow=1)
        colnames(cc) <- col_names
    } else {
        cc <- Reduce(rbind, cc)
    }

    colnames(cc) <- NULL
    rownames(cc) <- NULL
    N <- ncol(cc)
    P <- nrow(cc)

    # number of publications per year
    npub <- llply(group, function(x) 
                table(unique(x[, c('species_authority', 'year')])$year))
    pub.matrix <- array(0, dim = dim(count.matrix))
    rownames(pub.matrix) <- rownames(count.matrix)
    for(ii in seq(length(npub))) {
        pub.matrix[rownames(pub.matrix) %in% names(npub[[ii]]), ii] <- npub[[ii]]
    }

    # According to Edie (pers. comm.), OFFSET = TRUE is for TE model, and OFFSET = FALSE is for noTE model.
    # if( ! OFFSET){  
    #     pub.matrix[] <- 0 # set offset to zero
    # }

    list(N = N, P = P, str = as.numeric(starts), end = rep(max(len), P), 
                counts = cc, off = t(pub.matrix))

}

# data <-  original_code() 
rm(N, P, starts, pub.matrix, cc, count.matrix)
