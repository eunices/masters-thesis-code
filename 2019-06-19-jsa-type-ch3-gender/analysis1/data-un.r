# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Gender analysis: data aquisition
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Libraries
library(httr)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - get UN data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- get UN data"))

# http://ec2-54-174-131-205.compute-1.amazonaws.com/API/Information.php

indicators <- list(`24106` = "schl_f", # GDI: Mean years of schooling (females aged 25 years and above)
                   `24206` = "schl_m", # GDI: Mean years of schooling (males aged 25 years and above)
                   `120606`= "lexp_f", # GDI: Life expectancy at birth, female
                   `121106`= "lexp_m", # GDI: Life expectancy at birth, male
                   `123306`= "esch_f", # GDI: Expected years of schooling, females
                   `123406`= "esch_m", # GDI: Expected years of schooling, males
                   `123506`= "gnip_f", # GDI: Estimated GNI per capita (PPP), female
                   `123606`= "gnip_m", # GDI: Estimated GNI per capita (PPP), male
                   `137906`= "gdip_a"  # GDI
)
query_indicator <- do.call(paste, c(as.list(names(indicators)), sep = ","))
path <- paste0(
    "http://ec2-54-174-131-205.compute-1.amazonaws.com/API/HDRO_API.php/indicator_id=",
    query_indicator, "/year=1990,2017/structure=cyi")

request <- GET(url = path)
request$status_code
response <- content(request, as = "text", encoding = "UTF-8")
parsed_json <- fromJSON(response, flatten = TRUE)

df <-  parsed_json$indicator_value %>% data.frame() %>% t() %>% data.frame()
df$col <- rownames(df)
df <- data.table(df)
df[, c('country', 'year', 'indicator') := tstrsplit(col, ".", fixed=TRUE)]
names(df)[which(names(df)==".")] <- "value"

df_r <- dcast(df[year==2017], country ~ indicator, value.var="value")

names(df_r) <- unlist(sapply(names(df_r), function(name) {
    # print(name)
    if(name %in% names(indicators)) {
        indicators[name]
     } else { name }
}))

cols <- c("A-3", "Country")
lu <- fread('data/lookup/2019-05-29-statoid-country-codes.csv',  encoding="UTF-8")[, ..cols]
df_r <- merge(df_r, lu, by.x="country", by.y="A-3", all.x=T, all.y=F); rm(lu)
df_r <- df_r[!is.na(country)]

un_path <- "data/2019-11-11-un-indicators/"
dir.create(un_path)
filename_write = paste0(un_path, '2019-11-12-indicators.csv')
write.csv(df_r, filename_write, na='', row.names=F, fileEncoding="UTF-8")
