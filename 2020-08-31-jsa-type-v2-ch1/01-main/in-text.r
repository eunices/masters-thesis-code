
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - In-text figures
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- in-text figures"))

# Methods

# % of valid species
dim(df_all)
round(prop.table(table(df_all$status))*100, 1)

# Number of PTEs
dim(df_describers)
dim(df_describers[spp_N_1st_auth_s>=1])

# Date of birth
dim(df_describers[spp_N_1st_auth_s>=1 & !is.na(dob.describer)])
df_describers[spp_N_1st_auth_s>=1 & is.na(dob.describer)]$dob.describer

# Results

# Hyperdiverse Megachile
meg <- df[
    tolower(genus)=="megachile", 
    list(N=.N), 
    by=c("type.repository.n_short", "country.of.type.repository.n_long")
][order(-N)]
sum(meg$N)
sum(meg[type.repository.n_short != "[unknown]"][1:20]$N)
table(meg[type.repository.n_short != "[unknown]"][1:20]$country.of.type.repository.n_long)