
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Other biodata info
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Other biodata info"))

# Other biodata info 

# Who described the most bees
tax_most_spp <- df_describers[order(-ns_spp_N)]

tax_most_spp$full.name.of.describer <- paste0(
    tax_most_spp$full.name.of.describer, " (", 
    tax_most_spp$min, "-", tax_most_spp$max_corrected, ")"
)

tax_most_spp <- tax_most_spp[, 
    c("full.name.of.describer", "ns_spp_N", "n_pubs")
]

summary(df_describers$ns_spp_N)

write.csv(
    tax_most_spp[1:10], 
    paste0(dir_data_ch1, '2019-10-03-tax-1-most-spp.csv'), row.names=F
)

# Who had the most number of publications
tax_most_pub <- df_describers[order(-n_pubs)]

tax_most_pub$full.name.of.describer <- paste0(
    tax_most_pub$full.name.of.describer, " (", 
    tax_most_pub$min, "-", tax_most_pub$max_corrected, ")"
)

tax_most_pub <- tax_most_pub[, c("full.name.of.describer", "n_pubs")]

write.csv(
    tax_most_pub[1:10], 
    paste0(dir_data_ch1, '2019-10-03-tax-2-highest-pub.csv'), row.names=F
)

# Who had the most synonyms

check <- df_describers$spp_N >= 10

cols <- c(
    "full.name.of.describer", "prop_species_syn", 
    "spp_N", "syn_spp_N", "min", "max",
    "dob.describer", "dod.describer"
)

tax_highest_prop_syn <- df_describers[check, ..cols][order(-prop_species_syn)]
table(check)
summary(df_describers[check]$prop_species_syn)

# Francis Walker - many synonyms https://en.wikipedia.org/wiki/Francis_Walker_(entomologist)
df_describers[full.name.of.describer=="Francis Walker"]

write.csv(
    tax_highest_prop_syn[1:10], 
    paste0(dir_data_ch1, '2019-10-03-tax-3-highest-syn.csv'), row.names=F
)

