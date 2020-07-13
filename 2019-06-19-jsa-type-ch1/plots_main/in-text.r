
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - In-text figures
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- in-text figures"))

# Hyperdiverse Megachile
meg <- df[tolower(genus)=="megachile", list(N=.N), by=c("type.repository.n_short", "country.of.type.repository.n_long")][order(-N)]
sum(meg$N)
sum(meg[type.repository.n_short != "[unknown]"][1:20]$N)
table(meg[type.repository.n_short != "[unknown]"][1:20]$country.of.type.repository.n_long)