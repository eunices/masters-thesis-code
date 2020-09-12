library(data.table)


file <- "gadm36_0_Ecoregions2017_realm.csv"
df <- fread(file, encoding = "UTF-8", stringsAsFactors = FALSE)

df2 <- df[, list(
    area = sum(area)
), by=c("GID_0", "REALM")]

df3 <- dcast(df2, GID_0 ~ REALM, value.var = "area", fun.aggregate = sum)
df3 <- df3[trimws(GID_0) != ""]

df4 <- cbind(id = df3[, 1], df3[, -1]/rowSums(df3[, -1]))
largest <- colnames(df4)[-1][apply(df4[, -1], 1, which.max)]
df4$largest_prop <- apply(df4[,-1], 1, max, na.rm=TRUE)
df4$largest <- largest

fwrite(df4, "prop-area.csv")