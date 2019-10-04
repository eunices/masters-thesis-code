source('2019-07-15-edie-et-al/init.r')

# Libraries for analysis
library(plyr)
library(rstan)

# Parameters
input_filepath <- paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty7-trop-type2.csv")


# change as necessary
dat <- fread(input_filepath)
data <- dat[, c("Latitude_type2", "idx", "full.name.of.describer", "date.n")]
names(data) <- c("group", "valid_species_id", "species_authority", "year")

 

# Original code
run_original <- function() {
    
    duration <- seq(from = min(data$year), to = max(data$year))
    group <- split(data, data$group)

    counts <- llply(group, function(x) table(x$year))
    counts <- llply(counts, function(x) {
                    pos <- rep(0, length(duration))
                    names(pos) <- duration
                    pos[duration %in% names(x)] <- x
                    pos})
    count.matrix <- Reduce(cbind, counts)  # named per year in a group


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
    cc <- Reduce(rbind, cc)
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


    # if( ! OFFSET ){  # set offset to zero
    #     pub.matrix[] <- 0
    # }

    list(N = N, P = P, str = as.numeric(starts), end = rep(max(len), P), 
                counts = cc, off = t(pub.matrix))

}

data <- run_original()






dat <- df[, c("idx", "date.n")]
dat$group <- "g1"
# dat2 <- dat; dat2$group <- "g2"
# dat <- rbind(dat, dat2)
names(dat) <- c("valid_species_id", "year", "group")

# Code

# Create one matrix for each group
counts <- dat[, list(.N), by=c("group", "year")]
count.matrix <- dcast(counts, year ~ group, value.var="N")
count.matrix <- merge(data.frame(year=min(dat$year):max(dat$year)), 
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
pubs <- data.table(df_publications %>% separate_rows(idxes, sep="; "))
pubs <- unique(pubs[, c("date.n", "idxes", "paper.authors", "journal", "title",
                 "volume", "issue", "page.numbers.publication")])
names(pubs)[which(names(pubs)=="date.n")] <- "year"
pubs$idxes <- as.numeric(pubs$idxes)
dfpub <- merge(dat[, c("valid_species_id", "group")], pubs, 
              by.x="valid_species_id", by.y="idxes", all.x=T, all.y=F); dim(dfpub)
dfpub <- unique(dfpub[, c("group", "year", "paper.authors", "journal", "title",
         "volume", "issue", "page.numbers.publication")]); dim(dfpub)
npub <- dfpub[, list(N=.N), by=c("group", "year")]
pub.matrix <- dcast(npub, year ~ group, value.var="N")
pub.matrix <- merge(data.frame(year=min(dat$year):max(dat$year)), 
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


data <- list(N = N, P = P, str = as.numeric(starts), end = rep(max(dim(count.matrix)[1]), P), 
             counts = cc, off = t(pub.matrix))

with(data, {stan_rdump(list = c('N', 'P', 'str', 'end', 'counts', 'off'),
    file = 'tmp/count_info.data.R')} )


