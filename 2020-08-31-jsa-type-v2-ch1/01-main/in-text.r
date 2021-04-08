
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - In-text figures
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- in-text figures"))

# Methods

# % of valid species
dim(df_all) # includes subspecies
dim(df_all[status %in% c("Valid species", "Synonym")])
round(prop.table(table(df_all$status))*100, 1)

# Number of PTEs
dim(df_describers)
dim(df_describers[spp_N_1st_auth_s>=1])

# Date of birth
dim(df_describers[spp_N_1st_auth_s>=1 & !is.na(dob.describer)])

