
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - get UN data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- get UN data"))

# http://ec2-54-174-131-205.compute-1.amazonaws.com/API/Information.php

indicators <- c(24106,  # GDI: Mean years of schooling (females aged 25 years and above)
                24206,  # GDI: Mean years of schooling (males aged 25 years and above)
                120606, # GDI: Life expectancy at birth, female
                121106, # GDI: Life expectancy at birth, male
                123306, # GDI: Expected years of schooling, females
                123406, # GDI: Expected years of schooling, males
                123506, # GDI: Estimated GNI per capita (PPP), female
                123606, # GDI: Estimated GNI per capita (PPP), male
                137906  # GDI
)
query_indicator <- do.call(paste, c(as.list(indicators), sep = ","))
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
df_r

un_path <- "data/2019-11-11-un-indicators/"
dir.create(un_path)
write.csv(df_r, paste0(un_path, '2019-11-12-indicators.csv'), na='', row.names=F, fileEncoding="UTF-8")
