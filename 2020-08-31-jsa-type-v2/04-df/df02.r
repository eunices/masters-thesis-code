# Purpose: create author networks data.frame

source('2020-08-31-jsa-type-v2/00-init/main.r')
print(paste0(Sys.time(), " ----- df02.r"))


# Read data --------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "_7.csv")
df <- read_escaped_data_v2(file)

df <- df[status %in% c("Synonym", "Valid species") &
         duplicated == FALSE &
         date <= cutoff, 
         c("idx", "date", "status", "full.name.of.describer")]


# Create network data ----------------------------------------------------------

# Subset datasets
ps1 <- df[!grepl(";", full.name.of.describer),]  # only 1 describer
ps2 <- df[grepl(";", full.name.of.describer),]   # multiple describers

rm(df)

# Create pairs for multiple describers
ps2 <- strsplit(ps2$full.name.of.describer, split = "; ")
ps2 <- lapply(ps2, function(x) as.data.frame(t(combn(x, m=2))))
ps2 <- rbindlist(ps2)
names(ps2) <- c('p1', 'p2')

# Summarise by pairs
ps2 <- ps2[, .N, by = c("p1", "p2")][order(N)]

# Ensure each pair is unique
ps2$pairs <- apply(ps2[, c("p1", "p2")], 1, 
    function(x) paste0(sort(x), collapse = "; ")
)

ps2 <- ps2[, list(N = sum(N)), by = pairs]
ps2[, c("p1", "p2") := tstrsplit(pairs, "; ", fixed = TRUE)]
ps2$pairs <- NULL


# Append those that did not coauthor 
ps1 <- ps1[, c("full.name.of.describer")]
names(ps1) <- "p1"
ps1 <- ps1[, .N, by=c("p1")]; ps1$p2 <- NA
ps <- rbind(ps1, ps2)


# Write data -------------------------------------------------------------------

file <- paste0(v2_dir_data_raw, v2_basefile, "-describer-network.csv")
fwrite(ps, file)