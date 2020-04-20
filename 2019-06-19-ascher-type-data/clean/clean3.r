# Information about code:
# This code corresponds to cleaning code for my MSc thesis.
# A series of other codes are named as clean1|2|3|4.r
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - cleaning other fields
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- cleaning other fields"))

filepath <- paste0(dir_data, basefile, '-idx-2-clean-repo.csv')
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] # fread does not escape double quotes

# date: not cleaning actual date field as data is captured actually in publications table 
filepath <- paste0(dir_data, basefile, " pub_1.0-clean.csv")
pub <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
pub <- pub %>% separate_rows(idxes, sep="; ")
pub <- unique(pub[, c("idxes", "date.n")])
pub <- pub[!duplicated(idxes)]
df <- merge(df, pub, all.x=T, all.y=F, by.x="idx", by.y="idxes")

# date.of.type
df$date.of.type.string <- paste0("'", df$date.of.type)
df$date.of.type.dd <- as.numeric(sub("\\D*(\\d+).*", "\\1", df$date.of.type))
df[df$date.of.type.dd>31,]$date.of.type.dd <- NA

df$date.of.type.mm <- 
    gsub(".*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec).*", "\\1", df$date.of.type)
df[!df$date.of.type.mm %in% c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),]$date.of.type.mm <- ""
df$date.of.type.yyyy <- as.character(as.numeric(sub('.*(\\d{4}).*', '\\1', df$date.of.type)))

paste_nine = function(numeric){
    char <- strsplit(as.character(numeric), "")[[1]]
    word <- paste0(char[1], "9", char[3], char[4])
    as.character(word)
}

df[as.numeric(df$date.of.type.yyyy) <1200]$date.of.type.yyyy[] <- 
    as.character(lapply(df[as.numeric(df$date.of.type.yyyy) < 1200]$date.of.type.yyyy, function(x) paste_nine(x)[1]))

# date differences
df$years.lag <- as.numeric(df$date.n) - as.numeric(df$date.of.type.yyyy)

filepath <- paste0(dir_data, "clean/date_discrepancy.csv")
# those which have unresolved discrepancies (-ve date with no reason), will be changed to NA
# field to merge is "date.of.type.corrected" (it is in YYYY format)
date_discrepancy <- fread(filepath, integer64='character', na.strings=c(''), encoding='UTF-8')[, c("idx", "date.of.type.corrected")]
date_discrepancy[, names(date_discrepancy) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
date_discrepancy$idx <- as.character(date_discrepancy$idx)
df <- merge(df, date_discrepancy, by="idx", all.x=T, all.y=F)
df[!is.na(date.of.type.corrected)]$date.of.type.yyyy <- df[!is.na(date.of.type.corrected)]$date.of.type.corrected
df$date.of.type.corrected <- NULL

# calculate again
df$years.lag <- as.numeric(df$date.n) - as.numeric(df$date.of.type.yyyy)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - quick fixes
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- quick fixes"))

# missing full names
filepath <- paste0(dir_data, "clean/missing_authors_edit.csv")
missing_auth <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
missing_auth[, names(missing_auth) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 
df1 <- df[is.na(full.name.of.describer)]
df2 <- df[!is.na(full.name.of.describer)]
tmp <- merge(df1, missing_auth, 
            by.x="author", by.y="author", all.x=T, all.y=F)
tmp$full.name.of.describer.x <- NULL
names(tmp)[length(tmp)] <- 'full.name.of.describer'
df <- rbind(df2, tmp)
rm(tmp, df1, df2)

# 2019-08-27: discovered when cleaning author dates 
df[idx %in% c(12335, 12337, 12338, 12341, 12346)]$date <- "2008"
df[idx %in% c(13187)]$date <- "2018"
df[idx %in% c(502)]$full.name.of.describer <- "Osamu Tadauchi; Ryôichi Miyanaga; Ahmatjan Dawut"

# flags
df[idx==3335 & idx==9456]$flag = "IGNORE_COUNTRY_DISCREPANCY_ERRONEOUS_GADM_BOUNDARY"

# fixing higher order taxonomy
df[idx==6804]$family <- "Megachilidae"
df[genus=="Nomada" & subfamily=="Apinae"]$subfamily <- "Nomadinae"

write.csv(df[order(as.numeric(idx))], 
          paste0(dir_data, basefile, "-idx-3-clean-fields.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")
