print(paste0(Sys.time(), " --- Paragraph on taxonomic effort"))

####################
# Summary

# Count number of statuses
status <- table(df_all$status)
sum(status)
round(prop.table(status)*100, 1)


####################
# Data processing

# Subset data for describers which made at least 1 description as 1st/2nd auth
df_tax <- df_describers[spp_N_1st_auth_s > 0]; dim(df_tax)

# Calculate difference between "all species"* max and "valid species" max
# *includes subspecies
df_tax$diff <- df_tax$max - df_tax$ns_max

# Author names for those that described non valid species
tax_only_not_valid <- df_tax[ns_spp_N == 0]$full.name.of.describer

# Author names of those describing valid species AND at least 1 synonym 
tax_valid_w_one_syn <- 
    df_tax[ns_spp_N > 0 & syn_spp_N > 0]$full.name.of.describer

####################
# Tabulation

# Total number of author-years
total_author_years <- dim(df_describers_year)[1]
tax_notvalid_onesyn <- c(tax_only_not_valid, tax_valid_w_one_syn)


# First aspect: -
# Author-years: Only non-valid species
length(tax_only_not_valid) # number of authors 

total_1 <- author_years_not_valid <- 
    length(df_describers_year[full.name.of.describer %in% tax_only_not_valid]$N)
author_years_not_valid / total_author_years * 100 # proportion for non-valid

# Did not shift the median
summary(df_describers_year$N) # their inclusion
summary(df_describers_year[!full.name.of.describer %in% tax_only_not_valid]$N) # did not shift

# Second aspect: -
# Author-years: At least valid species and at least 1 non-valid species
length(tax_valid_w_one_syn)

# Those without difference
no_diff <- df_tax[full.name.of.describer %in% tax_valid_w_one_syn]$diff==0
round(prop.table(table(no_diff))*100, 1)

# Those with difference
length(df_tax[full.name.of.describer %in% tax_valid_w_one_syn & diff > 0]$diff)

# Median and quantiles
summary(df_tax[full.name.of.describer %in% tax_valid_w_one_syn & diff > 0]$diff)

# Cumulative
total_2 <- 
    sum(df_tax[full.name.of.describer %in% tax_valid_w_one_syn & diff > 0]$diff)
total_2

# Total for aspect 1 and 2
(total_1 + total_2) / total_author_years * 100
(total_1 + total_2) 


####################
# Plot

# Plot number of authors-years in a density plot
p1 <- ggplot(df_describers_year) + 
    geom_density(aes(x = N), alpha = 0.2) + 
    xlim(c(0, 20)) + theme

p2 <- ggplot(df_describers_year[!full.name.of.describer %in% tax_only_not_valid]) +
    geom_density(aes(x = N), alpha = 0.2) + xlim(c(0,20)) + theme

grid.arrange(p1, p2) # they look similar
