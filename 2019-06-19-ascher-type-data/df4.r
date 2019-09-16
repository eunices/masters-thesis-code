print("######################################################")
print("######################################################")
print("######################################################")
print(paste0(Sys.time(), " --- starting df4.r"))
print("######################################################")
print("######################################################")
print("######################################################")

source('2019-06-19-ascher-type-data/init.r')

# Libraries
#############

# NONE

# Parameters
#############

# loop_4 <- 'Y'
loop_4 <- 'N'

# Scripts
#############
source('2019-06-19-ascher-type-data/df4.1.r', local=T)
source('2019-06-19-ascher-type-data/df4.2.r', local=T)

# *TODO: Find a logical way to clean countries below

# Countries to clean specifically
# AT/PA: MLI, NER, TCD, SDN, YEM, SAU
# AN/PA: OMN, PAK, CHN, 
# AA/IM: IND
# NA/NT: MEX

# One country only: use type lat lon
# >1 Realm, 1 Country in Conflicted Realm: 
length(unique(df_mapper5[BIOGEO_OVERLAP_CTY_row != "FALSE", ]$idx))
length(unique(df_mapper5[no_realms == 1 & countries_n == 1 & BIOGEO_OVERLAP_CTY_row != "FALSE", ]$idx)) +
length(unique(df_mapper5[no_realms > 1 & countries_n == 1 & BIOGEO_OVERLAP_CTY_row != "FALSE", ]$idx)) + 
length(unique(df_mapper5[no_realms > 1 & countries_n > 1 & BIOGEO_OVERLAP_CTY_row != "FALSE", ]$idx))
