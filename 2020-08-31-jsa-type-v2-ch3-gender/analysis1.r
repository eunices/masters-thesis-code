# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Gender analysis: utility  functions.
# adapted from https://github.com/lukeholman/genderGapCode/
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2020-08-31-jsa-type-v2/subset.r')
source('2020-08-31-jsa-type-v2-ch3-gender/analysis1/libraries.r') 
source('2020-08-31-jsa-type-v2-ch3-gender/analysis1/params.r') 


# Scripts

# get data from UN's API and save locally
# source('2020-08-31-jsa-type-v2-ch3-gender/analysis1/data-un.r') 

# read local/ bee data
source('2020-08-31-jsa-type-v2-ch3-gender/analysis1/data-model.r') 


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep - in text fig
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# % of M, F, U
names(auth_full)
dim(auth_full)
table(auth_full$describer.gender)
prop.table(table(auth_full$describer.gender))

dim(auth[describer.gender!="U"])
table(auth[describer.gender!="U"]$describer.gender)
prop.table(table(auth[describer.gender!="U"]$describer.gender))

# Earliest date
min_yr_female <- min(dat[describer.gender=="F" ]$date.n)
dat[describer.gender=="F" & date.n== min_yr_female]


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep time series - visualisation 
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# By n species described
ts_prop_f <- dat[, list(
    nF = sum(describer.gender == "F"),
    nM = sum(describer.gender == "M")
), by = "date.n"]

ts_prop_f$prop <- ts_prop_f$nF/ ( ts_prop_f$nF + ts_prop_f$nM )
# ggplot(ts_prop_f, aes(x=date.n, y=prop)) + geom_line() + theme

# By taxonomist
ts_prop_f_tax <- auth_years[, list(
    nF = sum(describer.gender == "F"),
    nM = sum(describer.gender == "M")
), by = "years"]

ts_prop_f_tax$prop <- ts_prop_f_tax$nF/ ( ts_prop_f_tax$nF + ts_prop_f_tax$nM )
# ggplot(ts_prop_f_tax, aes(x=years, y=prop)) + geom_line() + theme


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep - run model for taxonomists
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

unique(auth_years[describer.gender=="U", 'full.name.of.describer'])

# Output data
country <- "All"; position <- "All"; type <- "Tax"
prop  <- generate_prop_t_tax(country=country)
output <- main(country=country, position=position, prop)

r <- output$summary$r; c <- output$summary$c 
parity.year <- output$summary$years.to.parity

baseline_yr <- min(prop$date.n)
max_y <- 70

# determine x-axis max value
max_predict_year <- as.integer((max(prop$date.n) - min(prop$date.n)) * 1.75)
parity.year <- as.numeric(parity.year) + CURRENT_YEAR - baseline_yr 
max_predict_year <- ifelse(
    parity.year > max_predict_year, parity.year, max_predict_year
)

prop_overlay <- cbind(prop, get.CIs(prop$nFemales, prop$nMales))
prop_template <- data.frame(
    yrs_fr_baseline = 0:max_predict_year + baseline_yr,
    predicted = sapply(0:max_predict_year, pfunc, r=r, c=c)
)

prop_overlay <- merge(
    prop_template, prop_overlay, 
    by.x="yrs_fr_baseline", by.y="date.n", 
    all.x=T, all.y=F
)

# NOTE! the unknowns were excluded!
sum(prop_overlay$`U`, na.rm=T)

prop_overlay$U <- NULL
prop_overlay$n <- NULL
prop_overlay$date <- NULL


names(prop_overlay) <- c(
    "year", "prop_F_predicted", "N_female", "N_males",
    "prop_Fl", "prop_F_lwrCI", "prop_F_uprCI"
)

wfile <- paste0(v2_dir_data_webapp, "ch3-fig-05-data.csv")
fwrite(prop_overlay, wfile, na="")



# Run model
result <- run_specific_scenario(
    country="All", position="All", dir_data_subf2, "tax"
)

prop_table <- generate_prop_t(country="All")
result_summary_tax <- result$summary

result_summary_countries_tax <- lapply(countries[1:6], function(country) {

    run_specific_scenario(
        country=country, position="All", dir_data_subf2, "tax"
    )$summary

})

outputs <- rbindlist(c(list(result_summary_tax), result_summary_countries_tax))
write.csv(outputs, paste0(dir_data_subf2, "_outputs.csv"), row.names=F)




# # Testing
#
# # Test generating prop tables
# generate_prop_t_tax("Germany")
# generate_prop_t_tax("United States of America")
#
# # Test plotting
# source('2020-08-31-jsa-type-v2-ch3-gender/analysis1/model.r')
# prop_t <- generate_prop_t_tax(country="Brazil")
# output <- main(country = "Brazil", position = "All", prop_t)
#
# save_graph(
#     dir_data_subf2, country="Brazil", position="All", prop_t, 
#     output$summary$r, output$summary$c, output$summary$years.to.parity, "tax"
# )


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep - in text fig on country
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender rep - in text fig on country"))

prop_tax <- auth[, .N, c("Country", "describer.gender")][order(-N)]
prop_tax <- dcast(prop_tax, Country ~ describer.gender, value.var="N")
prop_tax <- prop_tax[!is.na(Country)]
prop_tax[is.na(prop_tax)] <- 0
prop_tax$N <- prop_tax$F + prop_tax$M
prop_tax$prop_F <- prop_tax$F / prop_tax$N
prop_tax <- prop_tax[order(-prop_F)]

head(prop_tax, 3)

dim(prop_tax)
prop_tax[U >= 1 & (F == 0 & M == 0)]

# Exclude those countries with only unknown gender
prop_tax <- prop_tax[!(U >= 1 & (F == 0 & M == 0))]
prop_tax[grepl("Unknown", Country)]

dim(prop_tax)[1]              # N countries
length(prop_tax[prop_F==0]$F) # N countries w no females
length(prop_tax[prop_F>0]$F)  # N countries w females
median(prop_tax[prop_F>0]$F)  # Median F taxonomists in countries w females
median(prop_tax[prop_F>0]$M)  # Median M taxonomists in countries w females

prop_tax[F+M+U>5][1:10] # 

len = dim(prop_tax[F+M+U>5])[1]
prop_tax[F+M+U>5][(len-10):len]

write.csv(
    prop_tax, paste0(v2_dir_data_ch3_gender, "2019-11-15-prop-taxonomist.csv"), 
    row.names=F, fileEncoding='UTF-8'
)

# # Proportion of papers (! NOT USED)
# prop_t_countries <- rbindlist(lapply(countries, function(country) {
#     prop <- generate_prop_t(country=country, position="All")

#     if (!is.null(prop)) {
#         data.frame(Country=country, M=sum(prop$nMales), F=sum(prop$nFemales))
#     }

# }))

# prop_t_countries <- prop_t_countries[!is.na(Country)]
# prop_t_countries[is.na(prop_t_countries)] <- 0
# prop_t_countries$N <- prop_t_countries$F + prop_t_countries$M
# prop_t_countries$prop_F <- prop_t_countries$F / prop_t_countries$N
# prop_t_countries <- prop_t_countries[order(-prop_F)]

# write.csv(
#     prop_t_countries, 
#     paste0(v2_dir_data_ch3_gender, "2019-11-15-prop-taxonomist-spp.csv"), 
#     row.names=F, fileEncoding='UTF-8'
# )






############### NOT USED IN MAIN TEXT ############### 

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep - run model for species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender rep - run model for species"))

#################
# By Position
#################

result_summary_all_prop <- generate_prop_t(country="All")

spp_sum <- dat[, .N, by=c("describer.gender")]
spp_sum[describer.gender=="U"]$N/sum(spp_sum$N)*100
spp_sum <- spp_sum[describer.gender != "U"]; sum(spp_sum$N)
prop.table(spp_sum[,2])*100

result_summary_all <- lapply(
    c("All", "First", "Last", "First_s", "Last_s"), function(pos) {

        run_specific_scenario(
            country="All", position=pos, dir_data_subf1
        )$summary

})

#################
# By Country
#################

# countries <- c(
#     "United States of America", "Germany", "Brazil",
#     "France", "United Kingdom", "Japan"
# )

result_summary_countries <- lapply(countries[1:6], function(country) {
    run_specific_scenario(
        country=country, position="All", dir_data_subf1
    )$summary
})
# usa, germany, brazil, france, united kingdom, japan [top 6 countries]
# as case studies; they have more than 30 taxonomists across the years
# and potentially have interesting stories to tell

outputs <- rbindlist(c(result_summary_all, result_summary_countries))
write.csv(outputs, paste0(dir_data_subf1, "_outputs.csv"), row.names=F)

# Testing
# print(result_summary[[1]])
# rbindlist(result_summary)

# generate_prop_t("Germany", "All")
# generate_prop_t("France", "All")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - gender rep - if UN factors affect gender propo
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- gender rep - if UN factors affect gender proportion"))

# Modelling taxonomists
prop_tax_mdf <- merge(prop_tax, df_r, by="Country", all.x=T, all.y=F)
prop_tax_mdf <- prop_tax_mdf[N!=0]   

prop_tax_mdf$F_yn <- factor(
    ifelse(prop_tax_mdf$F > 0, "Y", "N"), 
    levels=c("N", "Y")
)

names(prop_tax_mdf)

prop_tax_mdf[is.na(country)]
prop_tax_mdf <- prop_tax_mdf[!is.na(country)]

ggplot(prop_tax_mdf, aes(x=F_yn, y=schl_f)) + geom_boxplot()
ggplot(prop_tax_mdf, aes(x=F_yn, y=gnip_f)) + geom_boxplot()
ggplot(prop_tax_mdf, aes(x=F_yn, y=gdip_a)) + geom_boxplot()

var <- unlist(lapply(prop_tax_mdf, is.numeric))
cor(prop_tax_mdf[, ..var])
heatmap(cor(prop_tax_mdf[, ..var]), Colv = NA, Rowv = NA)

# note: it did not look promising
