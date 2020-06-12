# Information about code:
# This code corresponds to cleaning code for my MSc thesis.
# A series of other codes are named as clean1|2|3|4|5|6.r
# Cleans date.of.type, years.lag
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

print("")
print(paste0(Sys.time(), " --- starting clean3.r"))
print("######################################################")

source('2019-06-19-jsa-type/clean/functions.R')


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - cleaning other fields
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- cleaning other fields"))




# Read data
df <- read_escaped_data(paste0(dir_data, basefile, '-idx-2-clean-repo.csv'))




# Clean date.n based on pub_1.0-clean.csv
# note: not cleaning actual date field as data is captured actually in publications table 
pub <- read_escaped_data(paste0(dir_data, basefile, " pub_1.0-clean.csv"))

# Separate by species
pub <- pub %>% separate_rows(idxes, sep="; ")
pub <- unique(pub[, c("idxes", "date.n")])
pub <- pub[!duplicated(idxes)]

# Merge to df
df <- merge(df, pub, all.x=T, all.y=F, by.x="idx", by.y="idxes")



# Clean date.of.type fields

# Create a string data field
df$date.of.type.string <- paste0("'", df$date.of.type)
df$date.of.type.dd <- as.numeric(sub("\\D*(\\d+).*", "\\1", df$date.of.type))
df[df$date.of.type.dd>31,]$date.of.type.dd <- NA

# Create month field
df$date.of.type.mm <- 
    gsub(".*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec).*", "\\1", df$date.of.type)
df[!df$date.of.type.mm %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),]$date.of.type.mm <- ""

# Create year field
df$date.of.type.yyyy <- as.character(as.numeric(sub('.*(\\d{4}).*', '\\1', df$date.of.type)))

# Clean year field
df[as.numeric(df$date.of.type.yyyy) <1200]$date.of.type.yyyy[] <- 
    as.character(lapply(df[as.numeric(df$date.of.type.yyyy) < 1200]$date.of.type.yyyy, 
                        function(x) paste_nine(x)[1]))





# Calculate years.lag
df$years.lag <- as.numeric(df$date.n) - as.numeric(df$date.of.type.yyyy)



# Checks for years.lag <0
# Merge info back

# those which have unresolved discrepancies (-ve date with no reason), will be changed to NA
# field to merge is "date.of.type.corrected" (it is in YYYY format)

date_discrepancy <- read_escaped_data(paste0(dir_data, "clean/date_discrepancy.csv"))
date_discrepancy <- date_discrepancy[, c("idx", "date.of.type.corrected")]

# Merge back
date_discrepancy$idx <- as.character(date_discrepancy$idx)
df <- merge(df, date_discrepancy, by="idx", all.x=T, all.y=F)
df[!is.na(date.of.type.corrected)]$date.of.type.yyyy <- 
    df[!is.na(date.of.type.corrected)]$date.of.type.corrected
df$date.of.type.corrected <- NULL





# Calculate years.lag again
df$years.lag <- as.numeric(df$date.n) - as.numeric(df$date.of.type.yyyy)



# Check for missing authors
# write.csv(df[is.na(full.name.of.describer), c("author", "full.name.of.describer"), 
#           paste0(data_dir, "clean/missing_authors.csv")]

# Add missing authors
missing_auth <- read_escaped_data(paste0(dir_data, "clean/missing_authors_edit.csv"))
df1 <- df[is.na(full.name.of.describer)]
df2 <- df[!is.na(full.name.of.describer)]

# Merge the new authors to data.frame where it is missing
tmp <- merge(df1, missing_auth, by.x="author", by.y="author", all.x=T, all.y=F)
tmp$full.name.of.describer.x <- NULL
names(tmp)[length(tmp)] <- 'full.name.of.describer'

# Merge back to the data.frame
df <- rbind(df2, tmp)
rm(tmp, df1, df2)




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - quick fixes
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- quick fixes"))




# 2019-08-27: discovered when cleaning author dates 
df[idx %in% c(12335, 12337, 12338, 12341, 12346)]$date <- "2008"
df[idx %in% c(13187)]$date <- "2018"
df[idx %in% c(502)]$full.name.of.describer <- "Osamu Tadauchi; Ryôichi Miyanaga; Ahmatjan Dawut"

# flags
df[idx==3335 & idx==9456]$flag = "IGNORE_COUNTRY_DISCREPANCY_ERRONEOUS_GADM_BOUNDARY"

# fixing higher order taxonomy
df[idx==6804]$family <- "Megachilidae"
df[genus=="Nomada" & subfamily=="Apinae"]$subfamily <- "Nomadinae"




# Write data
write.csv(df[order(as.numeric(idx))], paste0(dir_data, basefile, "-idx-3-clean-fields.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")
