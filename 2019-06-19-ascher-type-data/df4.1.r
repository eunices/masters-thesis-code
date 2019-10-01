
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - get distribution
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- get distribution from global mapper"))

df <- fread(paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-coll.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

df_mapper <- df[,c("idx", "global.mapper")]
df_mapper$global.mapper <- gsub("\\[[^][]*]", "", df_mapper$global.mapper) # remove those text in sq brackets
df_mapper2 <- data.table(df_mapper %>% separate_rows(global.mapper, sep=" "))
dim(df_mapper); dim(df_mapper2)
df_mapper2$country <-  gsub("(:|\\[)(.+?)$", "", df_mapper2$global.mapper) # get word before :
df_mapper2$country <-  gsub(":", "", df_mapper2$country); dim(df_mapper2) # erase : 
# note: there are some imperfect matches for 43
df_mapper2$nchar <- nchar(df_mapper2$country)
dim(df_mapper2); df_mapper2 <- df_mapper2[nchar == 2]; dim(df_mapper2)
dim(df_mapper2); df_mapper2 <- df_mapper2[global.mapper !=";;"]; dim(df_mapper2)

df_mapper2 <- merge(df_mapper2, lookup.cty[, c("DL", "Country")],
                    all.x=T, all.y=F, by.x="country", by.y="DL")
df_mapper2 <- merge(df_mapper2, lookup.loc[, c("DL", "NAME_0_owner")],
                    all.x=T, all.y=F, by.x="country", by.y="DL")
df_mapper2$Country.final <- ifelse(is.na(df_mapper2$Country),
                                   df_mapper2$NAME_0_owner, 
                                   df_mapper2$Country)

# write.csv(df_mapper2[is.na(Country.final)], 'tmp/test.csv') # find a way to improve on this in the future

df_mapper2 <- merge(df_mapper2[, c("idx", "Country.final")], 
                    lookup.cty[, c("Country", "A.3")], by.x="Country.final", by.y="Country")

seq(1, 20669)[!(df_mapper2[, list(.N), by="idx"]$idx %in% 1:20669)]

dim(df_mapper2); df_mapper2 <- unique(df_mapper2); dim(df_mapper2)

write.csv(df_mapper2[order(as.numeric(idx)),c("idx", "A.3")], 
        paste0(dir_data, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty1.csv"), na='', row.names=F, fileEncoding="UTF-8")
