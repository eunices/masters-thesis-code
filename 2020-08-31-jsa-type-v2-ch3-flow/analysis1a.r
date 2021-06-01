# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, in-text items for "taxonomic flow" 
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

source('2020-08-31-jsa-type-v2-ch3-flow/analysis1/prep.r')

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Where do describers come from?
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Where do describers come from?"))

des = get_des() # get dataset
dim(des)

des = des[(ns_spp_N + syn_spp_N) >=1]

# number of describers with multiple countries
des_multiple = data.table(separate_rows(
    des[, c("full.name.of.describer", "residence.country.describer")], 
    residence.country.describer, sep="; "
))

dim(des)
dim(des_multiple) - dim(des)

# number of describers with no country
table(is.na(des$residence.country.describer.first))
table(des$residence.country.describer.first == "[unknown]")

# number of countries for describers
des_tabulate <- des[
    !is.na(residence.country.describer.first), .N, 
    by="residence.country.describer.first"
][order(-N)]

dim(des_tabulate[residence.country.describer.first != "[unknown]"]) 

des_tabulate <- merge(
    des_tabulate, lu[, c("DL", "continent")], 
    all.x=T, all.y=F, 
    by.x="residence.country.describer.first", by.y="DL"
)

# top 3 countries with describers
des_tabulate[1:3]

# canada + usa
des_tabulate[residence.country.describer.first %in% c("US", "CA") ]
des_tabulate[continent == "North America"]
sum(des_tabulate$N)



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Where do describers come from - by socioeconomic status
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Where do describers come from - by socioeconomic status"))

des_where <- data.table(separate_rows(
    des[, c("full.name.of.describer", "residence.country.describer")], 
    residence.country.describer, sep="; "
))

des_where <- des_where[!duplicated(full.name.of.describer),]

cols <- c("DL", "Class", "continent")
des_where <- merge(
    des_where, lu[, ..cols], 
    by.x="residence.country.describer", by.y="DL", all.x=T, all.y=T
)

lvls_class <- c(
    "Low income", "Lower middle income", "Upper middle income",
    "High income",  "Unclassed"
)

des_where$Class <- factor(des_where$Class, lvls_class)

lvls_continent <- rev(c(
    "Africa", "Oceania", "Asia", "Australia", "South America", 
    "North America",  "Europe"
))
des_where$continent <- factor(des_where$continent, lvls_continent)

# For countries with describers: Distribution of classes
des_where1 <- des_where[!is.na(full.name.of.describer)]

des_where1[grepl("unknown", tolower(residence.country.describer))] # !CHECK

table(is.na(des_where1$Class)) # with no class due to unknown country of residence

des_where_summary <- des_where1[
    !is.na(des_where1$Class), .N, by=c("Class", "continent")
]

des_where_summary <- dcast(des_where_summary, Class ~ continent, value.var="N")
des_where_summary[is.na(des_where_summary)] <- 0
des_where_summary$N / sum(des_where_summary$N) *100

# By class
des_where_by_class <- cbind(
    des_where_summary[,1], rowSums(des_where_summary[,-1])
)

des_where_by_class$name <- paste0(
    des_where_by_class$Class, "\n(", des_where_by_class$V2, ")"
)

# By continent
des_where_by_continent <- data.frame(col=colSums(des_where_summary[,-1]))

des_where_by_continent$name <- paste0(
    rownames(des_where_by_continent), " (", des_where_by_continent$col, ")"
)

# By continent x class
des_where_summary_plot <- melt(
    des_where_summary, id.vars="Class", 
    measure.vars=names(des_where_summary)[-1]
)

labels_x <- des_where_by_class[
    match(lvls_class, des_where_by_class$Class),]$name

labels_y <- des_where_by_continent[
        match(lvls_continent, rownames(des_where_by_continent)),]$name

des_whereplot <- ggplot(
    des_where_summary_plot, aes(x=Class, y=variable, fill=value)
    ) + 
    geom_tile() + xlab("") + ylab("") + 
    scale_fill_gradient(name="", low="grey40", high="grey90") +
    geom_text(aes(label = ifelse(value >0, round(value, 1), ""))) +
    # scale_fill_continuous(name="") +
    scale_x_discrete(labels=labels_x) +
    scale_y_discrete(labels=labels_y) +
    theme


cfile <- paste0(dir_plot, 'fig-1.png')
ggsave(cfile, des_whereplot, units="cm", width=20, height=10, dpi=300)

des_whereplot_data <- des_where_summary_plot[, c(
    "Class", "variable", "value"
)]

names(des_whereplot_data) <- c("socioecon", "continent", "n_pte")

wfile <- paste0(v2_dir_data_webapp, "ch3-fig-01-data.csv")
fwrite(des_whereplot_data, wfile, na="")


# Chisq test of association

des_where1_test <- des_where1[!is.na(Class) & !is.na(continent)]
chisq.test(des_where1_test$Class, des_where1_test$continent)
# Cannot be run as the marginal values are low 


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Countries with no describers
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Countries with no describers"))

# For all countries, to investigate which countries have no describer
des_where2 <- unique(des_where[is.na(full.name.of.describer)])
des_where2[duplicated(residence.country.describer)] # ! CHECK

# N unclassed
length(unique(des_where2[Class == "Unclassed"]$residence.country.describer)) 

des_where_summary2 <- des_where2[Class != "Unclassed", .N, by=Class] 
tl <- sum(des_where_summary2$N); tl

sum(des_where_summary2$N)
des_where_summary2
des_where_summary2$N / tl *100


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Check if describers are insular
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Check if describers are insular"))

table(t$no_flow) 
# "des" for destination (type locality) and "ori" for origin (describer)

# total number of species
s1 <- t[, list(N=sum(N)), by='des']

# described by taxonomists in the country                   
s2 <- t[no_flow=="TRUE", list(N=sum(N)), by='des']    

ss <- merge(s1, s2, by='des', all.x=T, all.y=F, suffixes=c("_total", "_cty"))
ss[is.na(N_cty)]$N_cty <- 0

ss$prop <- (ss$N_cty/ss$N_total)

ss <- merge(
    ss, lookup.cty[, c("DL", "GEC", "Country", "A-3", "Class")], 
    by.x="des", by.y="DL", all.x=T, all.y=F
)


ss$Class <- factor(ss$Class, levels=lvls_class)
ss[Class=="Unclassed"] # !CHECK
ss[is.na(Country)]     # !CHECK

# Countries with resident describer describing species in the country
dim(ss[N_cty==0 & N_total >=1])
dim(ss[N_cty>=1])

# Median proportion of species
summary(ss[N_total>=10 & N_cty>=1]$prop*100)
dim(ss[N_total>=10 & N_cty>=1])

shapiro.test(ss[N_total>=10 & N_cty>=1]$prop*100) # not normal

# Countries with lowest proportion of species described by non-residents
ss[N_total>=10][order(-prop)][1:3]

ss[des %in% c("CA", "US")]

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Kruskal wallis test for proportion
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Kruskal wallis test for proportion"))

# Countries which are unclassed
ss[Class == "Unclassed"]

# Statistical tests
# source: http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
kruskal.test(prop~Class, data=ss)
pairwise.wilcox.test(ss$prop, ss$Class, p.adjust.method = "BH")

ss_summary <- ss[, list(
    mean=mean(prop), 
    median=median(prop),
    quantile_1st = quantile(prop, 0.25),
    quantile_3rd = quantile(prop, 0.75),
    N=.N
    ), by=c("Class")]

ss_summary

cfile <- paste0(dir_tables, "2019-09-22-summary-country-prop-summary.csv")
write.csv(ss_summary, cfile, na='', row.names=F, fileEncoding="UTF-8")

cfile <- paste0(dir_tables, "2019-09-22-summary-country-prop.csv")
write.csv(ss[order(-prop)], cfile,na='', row.names=F, fileEncoding="UTF-8")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig 2 socioeconomic status on proportion of species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(
    Sys.time(), " --- Fig 2 socioeconomic status on proportion of species"
))

# Plot
xlab <- "\nWorld Bank classification\n of country"
ylab <- "Percentage of species described\n by resident describers (%)\n"

p1 <- ggplot(ss[!is.na(Class)], aes(x=Class, y=round(prop*100,2))) + 
	geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=1, size=1) +
	labs(x=xlab, y=ylab) +
	theme

cfile <- paste0(dir_plot, 'fig-2.png')
ggsave(cfile, p1, units="cm", width=20, height=9, dpi=300)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Kruskal wallis test for number of countries for flow
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Kruskal wallis test for number of countries for flow"))

# Data processing
flow <- unique(spp_s[
    , c("type.country_n", "residence.country.describer.first")
])

flow <- flow[type.country_n != residence.country.describer.first]
flow <- flow[,.N, by=c("residence.country.describer.first")]

des_countries <- unique(des$residence.country.describer.first)
length(des_countries)

des_countries <- des_countries[
    !des_countries %in% flow$residence.country.describer
]

des_countries <- data.frame(
    residence.country.describer.first=des_countries, N=0
)

flow <- rbind(flow, des_countries)        

flow <- merge(
    flow, lookup.cty[, c("DL", "Class")], 
    by.x="residence.country.describer.first", by.y="DL", all.x=T, all.y=F
)

flow <- flow[
    !is.na(residence.country.describer.first) & 
    residence.country.describer.first != "[unknown]"
]

flow$Class <- factor(flow$Class, levels=lvls_class)

kruskal.test(N~Class, data = flow)
pairwise.wilcox.test(flow$N, flow$Class, p.adjust.method = "BH")

flow$N <- as.numeric(flow$N)

flow_summary <- flow[, list(
    mean=mean(N), median=median(N),
    quantile_1st = quantile(N, 0.25),
    quantile_3rd = quantile(N, 0.75),
    N=.N
    ), by=c("Class")]

flow_summary

write.csv(
    flow_summary[order(-N)],
    paste0(dir_tables, "2019-09-22-summary-country-N-summary.csv"), 
    na='', row.names=F, fileEncoding="UTF-8"
)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig 3 socioeconomic status on number of countries contributed to
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Fig 3 socioeconomic status on number of countries contributed to"))

lab_x <- "\nWorld Bank classification\n of donor country"
lab_y <- "Number of recipient countries \n for each donor country"

p2 <- ggplot(flow[!is.na(Class),], aes(x=Class, y=N)) + 
  	geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=1, size=1) +
	labs(x=lab_x, y=lab_y) + theme

cfile <- paste0(dir_plot, 'fig-3.png')
ggsave(cfile, p2, units="cm", width=20, height=10, dpi=300)



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - In-text: correlation between n describers & prop described by non-residents
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- In-text: correlation between n describers & prop described by non-residents"))

countries_prop <- ss[!is.na(Class)]

countries_des <- des_where1[
    !is.na(residence.country.describer),
    .N, by=c("residence.country.describer")
]

countries <- merge(
    countries_prop, countries_des,
    by.x = "des", by.y = "residence.country.describer",
    all.x = F, all.y = F
)

cor(countries$N, countries$prop)

# continue with analysis1b.r for GLM (determinants of flow)






