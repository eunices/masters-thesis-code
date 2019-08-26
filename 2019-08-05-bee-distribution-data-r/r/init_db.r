source('2019-08-05-bee-distribution-data/init_amnh.r')
source('2019-08-05-bee-distribution-data/init_gbif.r')

# Libraries
library(RSQLite)

# Initialize db
db_name <- "data/sqlite-amnh.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), db_name)
dbWriteTable(mydb, "amnh", df_amnh)
dbDisconnect(mydb)

db_name <- "data/sqlite-gbif.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), db_name)
dbWriteTable(mydb, "gbif", df_gbif)
dbDisconnect(mydb)
