# Reformat griis data
library(data.table)
df <- fread('data/2019-08-10-griis/2019-08-10-export_griis.csv', encoding='UTF-8')
write.csv(df, 'data/2019-08-10-griis/2019-08-10-export_griis_formatted.csv', row.names=F)
