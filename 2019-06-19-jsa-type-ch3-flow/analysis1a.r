source('2019-06-19-jsa-type-ch3-flow/analysis1/prep.R')

# Parameters
theme = theme_classic()
dir_plot = "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\_figures\\_ch3\\_ch3-flow\\"

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Where do describers come from?
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Where do describers come from?"))

des = get_des(write=F) # get dataset
dim(des)

# number of authors with multiple countries
des_multiple = separate_rows(des[, c("full.name.of.describer.n", "residence.country.describer.n")], 
                             residence.country.describer.n, sep="; ")
des_multiple = data.table(des_multiple)
dim(des)
dim(des) - dim(des_multiple)

# number of describers with no country
length(is.na(des$residence.country.describer.first))

# number of countries for describers
des_tabulate = des[!is.na(residence.country.describer.first), .N, 
                   by="residence.country.describer.first"][order(-N)]
dim(des_tabulate) 

# top 3 countries with describers
des_tabulate[1:3]


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Where do describers come from - by socioeconomic status
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Where do describers come from - by socioeconomic status"))


cols <- c("DL", "Class", "continent")
des_where = separate_rows(des[, c("full.name.of.describer.n", "residence.country.describer.n")], 
                             residence.country.describer.n, sep="; ")
des_where = data.table(des_where[!duplicated(des_where$full.name.of.describer.n),])
des_where = merge(des_where, lu[, ..cols], 
				  by.x="residence.country.describer.n", by.y="DL", all.x=T, all.y=T)
lvls_class = c("Low income", "Lower middle income", "Upper middle income", "High income",  "Unclassed")
des_where$Class = factor(des_where$Class, lvls_class)
lvls_continent = c("Africa", "Oceania", "Asia", "Australia", "South America", "North America",  "Europe")
lvls_continent = rev(lvls_continent)
des_where$continent = factor(des_where$continent, lvls_continent)


# For countries with describers: Distribution of classes
des_where1 = des_where[!is.na(full.name.of.describer.n)]
des_where1[grepl("unknown", tolower(residence.country.describer.n))] # !CHECK
table(is.na(des_where1$Class)) # with no class due to unknown country of residence
des_where_summary = des_where1[!is.na(des_where1$Class), .N, by=c("Class", "continent")] # TODO:
des_where_summary = dcast(des_where_summary, Class ~ continent, value.var="N")
des_where_summary[is.na(des_where_summary)] = 0
des_where_summary$N / sum(des_where_summary$N) *100


# By class
des_where_by_class = cbind(des_where_summary[,1], rowSums(des_where_summary[,-1]))
des_where_by_class$name = paste0(des_where_by_class$Class, "\n(", des_where_by_class$V2, ")")
# By continent
des_where_by_continent = data.frame(col=colSums(des_where_summary[,-1]))
des_where_by_continent$name = paste0(rownames(des_where_by_continent), " (",
                                     des_where_by_continent$col, ")")
# By continent x class
des_where_summary_plot = melt(des_where_summary, id.vars="Class", 
                              measure.vars=names(des_where_summary)[-1])
# des_where_summary_plot$Class = des_where_by_class[match(des_where_summary_plot$Class, 
#                                                         des_where_by_class$Class),]$name
# des_where_summary_plot$variable = 
#     des_where_by_continent[match(des_where_summary_plot$variable, 
#                                  rownames(des_where_by_continent)),]$name
des_whereplot = ggplot(des_where_summary_plot, aes(x=Class, y=variable, fill=value)) + 
    geom_tile() + 
    xlab("") +
    ylab("") + 
    scale_fill_gradient(name="", low="grey40", high="grey90") +
    geom_text(aes(label = ifelse(value >0, round(value, 1), ""))) +
    # scale_fill_continuous(name="") +
    scale_x_discrete(labels=des_where_by_class[match(lvls_class, des_where_by_class$Class),]$name) +
    scale_y_discrete(labels=des_where_by_continent[match(lvls_continent, rownames(des_where_by_continent)),]$name) +
    theme
ggsave(paste0(dir_plot, 'fig-2.png'), des_whereplot, units="cm", width=20, height=10, dpi=300)

# Chisq test of association
des_where1_test = des_where1[!is.na(Class) & !is.na(continent)]
chisq.test(des_where1_test$Class, des_where1_test$continent)
# Cannot be run as the marginal values are low 


# For all countries, to investigate which countries have no describer
des_where2 = unique(des_where[is.na(full.name.of.describer.n)])
des_where2[duplicated(residence.country.describer.n)] # ! CHECK

length(unique(des_where2[Class == "Unclassed"]$residence.country.describer.n)) # N unclassed
des_where_summary2 = des_where2[Class != "Unclassed", .N, by=Class] 
tl = sum(des_where_summary2$N); tl
des_where_summary2
des_where_summary2$N / tl *100




# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Check if describers are insular
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Check if describers are insular"))

table(t$no_flow) # "des" for destination (type locality) and "ori" for origin (describer)
s1 <- t[, list(N=sum(N)), by='des']                   # total number of species
s2 <- t[no_flow=="TRUE", list(N=sum(N)), by='des']    # described by taxonomists in the country
ss <- merge(s1, s2, by='des', all.x=T, all.y=F, suffixes=c("_total", "_cty"))
ss$prop <- ss$N_cty/ss$N_total
ss <- merge(ss, lookup.cty[, c("DL", "GEC", "Country", "A.3", "Class")], 
      		by.x="des", by.y="DL", all.x=T, all.y=F)
ss[is.na(N_cty)]$N_cty <- 0
ss[is.na(prop)]$prop <- 0
ss$Class <- factor(ss$Class, levels=c("High income", 
                                      "Upper middle income",
                                      "Lower middle income",
                                      "Low income", 
                                      "Unclassed"))
ss[Class=="Unclassed"] # !CHECK
ss[is.na(Country)]     # !CHECK

dim(ss[N_cty>=1])
dim(ss[N_total>=30 & N_cty>=1])
summary(ss[N_total>=30 & N_cty>=1]$prop*100)
shapiro.test(ss[N_total>=30 & N_cty>=1]$prop*100) # not normal
ss[N_total>=30][order(-prop)][1:4]



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig 2 socioeconomic status on proportion of species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Fig 2 socioeconomic status on proportion of species"))

# Plot
p1 = ggplot(ss[!is.na(Class)], aes(x=Class, y=round(prop*100,2))) + 
	geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=1, size=1) +
	labs(x="\nWorld Bank classification", 
		 y = "Proportion of species described\n by describers residing in country (%)\n") +
	theme
ggsave(paste0(dir_plot, 'fig-3.png'), p1, units="cm", width=20, height=10, dpi=300)

# Statistical tests
# source: http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
kruskal.test(prop~Class, data=ss)
pairwise.wilcox.test(ss$prop, ss$Class, p.adjust.method = "BH")
ss_summary <- ss[, list(mean=mean(prop),
                        median=median(prop),
                        quantile_1st = quantile(prop, 0.25),
                        quantile_3rd = quantile(prop, 0.75),
                        N=.N),
                  by=c("Class")]

write.csv(ss_summary,
          paste0(dir_data_ch3_flow, "2019-09-22-summary-country-prop-summary.csv"),
		  na='', row.names=F, fileEncoding="UTF-8")

write.csv(ss[order(-prop)],
          paste0(dir_data_ch3_flow, "2019-09-22-summary-country-prop.csv"), 
		  na='', row.names=F, fileEncoding="UTF-8")



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig 3 socioeconomic status on number of countries contributed to
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Fig 3 socioeconomic status on number of countries contributed to"))

# Data processing
flow <- unique(spp_s[, c("idx", "type.country.n", "residence.country.describer.n")])
flow <- flow[!duplicated(idx)]
flow <- unique(separate_rows(flow, residence.country.describer.n, sep="; "))
flow <- unique(flow[, c("type.country.n", "residence.country.describer.n")])
flow <- flow[type.country.n != residence.country.describer.n]
flow <- flow[,.N, by=c("residence.country.describer.n")]

des_countries <- unique(des$residence.country.describer.n); length(des_countries)
des_countries <- des_countries[!des_countries %in% flow$residence.country.describer.n]
des_countries <- unique(unlist(lapply(des_countries, strsplit, "; ")))
des_countries <- data.frame(residence.country.describer.n=des_countries, N=0)
flow <- rbind(flow, des_countries)        

flow <- merge(flow, 
              lookup.cty[, c("DL", "Class")], 
              by.x="residence.country.describer.n", by.y="DL", all.x=T, all.y=F)
flow <- flow[!is.na(residence.country.describer.n) & residence.country.describer.n != "[unknown]"]
flow$Class <- factor(flow$Class, levels=c("High income", 
                                          "Upper middle income",
                                          "Lower middle income",
                                          "Low income", 
                                          "Unclassed"))

kruskal.test(N~Class, data = flow)
pairwise.wilcox.test(flow$N, flow$Class, p.adjust.method = "BH")

p2 = ggplot(flow[!is.na(Class),], aes(x=Class, y=N)) + 
  	geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=1, size=1) +
	labs(x="\nWorld Bank classification", 
		 y = "Number of countries\n to which there is taxonomic flow") +
	theme
ggsave(paste0(dir_plot, 'fig-4.png'), p2, units="cm", width=20, height=10, dpi=300)

flow$N <- as.numeric(flow$N)
flow_summary <- flow[, list(mean=mean(N),
							median=median(N),
							quantile_1st = quantile(N, 0.25),
							quantile_3rd = quantile(N, 0.75),
                        N=.N),
                  by=c("Class")]
flow_summary          
write.csv(flow_summary[order(-N)],
          paste0(dir_data_ch3_flow, "2019-09-22-summary-country-N-summary.csv"), 
          na='', row.names=F, fileEncoding="UTF-8")

# continue with analysis1b.r for GLM (determinants of flow)







########################################### EXTRA EDA

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Location analysis suggested by Ascher
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Location analysis"))

# Where are the type repositories?
loc_type_repo_N <- spp[, list(N_type_repo_N=.N), 
    by=c("country.of.type.repository.n_long")][order(-N_type_repo_N)]
loc_type_repo_N[1:20]

loc_type_repo_unique <- spp[, list(N_type_repo_unique=length(unique(type.repository.n_short))), 
                           by=c("country.of.type.repository.n_long")][order(-N_type_repo_unique)]
loc_type_repo_unique[1:20]

loc_type_repo <- merge(loc_type_repo_N, loc_type_repo_unique, 
                       by="country.of.type.repository.n_long", all.x=T, all.y=T)
table(spp$country.of.type.repository.n_long=="[unknown]")

# Where are the publishers?

# by publication
df_publications <- get_pub(write=F)
loc_pub_N <- merge(df_publications[, list(.N), by=c("country.of.publication")],
                       lookup.cty[, c("Country", "DL")], 
                       by.x="country.of.publication", by.y="DL", all.x=T, all.y=F)
loc_pub_N <- loc_pub_N[order(-N)]
loc_pub_N[order(-N)][1:10]
table(is.na(df_publications$country.of.publication))

# by journals
loc_pub_unique <- merge(df_publications[, list(N=length(unique(journal))), 
                                        by=c("country.of.publication")],
                       lookup.cty[, c("Country", "DL")], 
                       by.x="country.of.publication", by.y="DL", all.x=T, all.y=F)
loc_pub_unique <- loc_pub_unique[order(-N)]
loc_pub_unique[order(-N)][1:10]

# Quick checks
# unique(df_publications[country.of.publication=="JA"]$journal)
# unique(df_publications[country.of.publication=="NZ"]$journal)
# table(df_publications[country.of.publication=="CA"]$journal)
# unique(df_publications[country.of.publication=="RU"]$city.of.publication)
# table(df_publications[country.of.publication=="YA"]$city.of.publication)

# Where are the type localities?
loc_type_loc <- spp[type.country.n.full != "", list(.N), by=c("type.country.n.full")][order(-N)]
loc_type_loc[1:20]

# Where are the describers?

# by N species described
df_describers <- get_des(write=F)
loc_des_N <- df_describers[!is.na(residence.country.describer.first), 
                           list(N=sum(ns_spp_N)), 
                           by=c("residence.country.describer.first")][order(-N)]

# by describer
loc_des_unique <- df_describers[!is.na(residence.country.describer.n), list(.N), 
                              by=c("residence.country.describer.first")][order(-N)]
loc_des_unique[1:20]
table(is.na(df_describers$residence.country.describer.first))


merge1 <- merge(loc_type_repo, loc_pub_N[, c("Country", "N")], 
                by.x="country.of.type.repository.n_long", by.y="Country", all.x=T, all.y=T)
names(merge1)[which(names(merge1) == "N")] <- "N_loc-pub-N"
merge2 <- merge(merge1, loc_pub_unique[, c("Country", "N")], 
                by.x="country.of.type.repository.n_long", by.y="Country", all.x=T, all.y=T)
names(merge2)[which(names(merge2) == "N")] <- "N_loc-pub-unique"
merge3 <- merge(merge2, loc_type_loc, 
                by.x="country.of.type.repository.n_long", by.y="type.country.n.full", 
                all.x=T, all.y=T)
names(merge3)[which(names(merge3) == "N")] <- "N_type-loc"
merge4 <- merge(merge3, loc_des_N, 
                by.x="country.of.type.repository.n_long", 
                by.y="residence.country.describer.first", 
                all.x=T, all.y=T)
names(merge4)[which(names(merge4) == "N")] <- "N_des_N"
merge5 <- merge(merge4, loc_des_unique, 
                by.x="country.of.type.repository.n_long", 
                by.y="residence.country.describer.first", 
                all.x=T, all.y=T)
names(merge5)[which(names(merge5) == "N")] <- "N_des-N"
merge5[is.na(merge5)] <- 0

write.csv(merge5[country.of.type.repository.n_long!=0][order(country.of.type.repository.n_long)], 
          paste0(dir_data_ch3_flow, '2019-10-03-loc.csv'), row.names=F)
