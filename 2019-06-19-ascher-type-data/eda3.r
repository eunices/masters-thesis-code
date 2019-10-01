source('2019-06-19-ascher-type-data/subset.r')

# Others

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - data quality
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- data quality"))

spp <- get_df1(write=F)
spp2 <- get_df2(write=F)

flag <- rbind(data.frame(table(spp[date.n <= 2018]$source.of.latlon.n)),
              data.frame(table(spp2[date.n <= 2018]$source.of.latlon.n)))

write.csv(flag,
          paste0(dir_data, "eda3_oth/2019-09-22-type-data-quality2.csv"), na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - lat and lon
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- lat and lon"))

df_ll <- rbind(spp[, c("idx", "lat", "lon")], spp2[, c("idx", "lat", "lon")])
x <- dim(df_ll); df_ll <- df_ll[!(is.na(lat) | is.na(lon))]; y <- dim(df_ll)
round(y/x*100, 2)

summary(df_ll)

write.csv(df_ll[order(as.numeric(idx))], 
          paste0(dir_data, "eda3_oth/2019-09-26-lat-lon.csv"), na='', row.names=F, fileEncoding="UTF-8")


