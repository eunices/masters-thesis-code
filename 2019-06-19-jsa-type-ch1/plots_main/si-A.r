print(paste0(Sys.time(), " --- Paragraph on taxonomic effort"))

# Count number of statuses
df_years <- rbind(df[, c("idx", "status", "date.n")], df2)
status <- table(df_years$status)
sum(status)
round(prop.table(status)*100, 1)

# Get data
df_tax <- df_describers[spp_N_1st_auth_s>0]; dim(df)
df_tax$diff <- df_tax$max - df_tax$ns_max
df_tax$non_valid_spp_N <- df_tax$spp_N - df_tax$ns_spp_N

# Segregate authors with only non-valid and valid w/ non-valid species
tax_only_non_valid <- df_tax[ns_spp_N==0]$full.name.of.describer.n
tax_valid_and_non_valid <-  df_tax[ns_spp_N > 0 & non_valid_spp_N > 0]$full.name.of.describer.n

# Total number of author-years
total_author_years <- dim(df_describers_year)[1]

# Author-years: Only non-valid species
length(tax_only_non_valid)
length(df_describers_year[full.name.of.describer %in% tax_only_non_valid]$N)
length(df_describers_year[full.name.of.describer %in% tax_only_non_valid]$N) / total_author_years * 100

# Did not shift the median
summary(df_describers_year$N)
summary(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid)]$N)
summary(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid, tax_valid_and_non_valid)]$N)

# Author-years: At least valid species and at least 1 non-valid species
length(tax_valid_and_non_valid)
round(prop.table(table(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid]$diff==0))*100, 1)
summary(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid & diff >0]$diff)
sum(df_tax[full.name.of.describer.n %in% tax_valid_and_non_valid & diff >0]$diff)

# Proportion of author-years
(721+524)/ total_author_years * 100

# Plot number of authors-years in a density plot
p1 <- ggplot(df_describers_year) + 
    geom_density(aes(x = N), alpha = 0.2) + 
    xlim(c(0, 20)) + theme
p2 <- ggplot(df_describers_year[!full.name.of.describer %in% c(tax_only_non_valid)]) +
    geom_density(aes(x = N), alpha = 0.2) + xlim(c(0,20)) + theme
grid.arrange(p1, p2) # they look similar
