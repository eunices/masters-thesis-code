source('2021-06-02-jsa-type-v2-china/init.r')

# Create map dataset

df_all <- get_df()

df <- df_all[
    duplicated == FALSE & status %in% c("Valid species", "Synonym"), 
    c(
        "idx", "genus", "species", "status", "date", 
        "type.repository.n", "country.of.type.repository.n_long",
        "type.country_n")
]

lp_repo <- unique(df[, c("country.of.type.repository.n_long", "type.repository.n")])

names(df_all)

df[type.country_n == "CH" & type.repository.n == "[unknown]", .N]
repo <- df[
    type.country_n == "CH" , .N, by= c("type.repository.n") 
][order(-N)]
repo[type.repository.n == "[unknown]"]$type.repository.n <- "Unknown"
top <- repo[type.repository.n != "Unknown"][1:5]$type.repository.n
repo$repo_others <- ifelse(
    repo$type.repository.n %in% c(top, "Unknown"), as.character(repo$type.repository.n), "Others"
)
repo <- repo[, list(N=sum(N)), by=repo_others]
repo$pct <- round(100*repo$N/sum(repo$N), 0)
repo <- merge(repo, lp_repo, by.x="repo_others", by.y="type.repository.n", all.x=T, all.y=F)
repo$lab <- ifelse(
    is.na(repo$country.of.type.repository.n_long),
    repo$repo_others, paste0(repo$repo_others, " \n(", repo$country.of.type.repository.n_long,")")
)

repo <- rbind(
    repo[repo_others != "Unknown" & repo_others != "Others"], 
    repo[repo_others == "Others"],
    repo[repo_others == "Unknown"]
)
repo$lab <- factor(repo$lab, levels=repo$lab)

png(paste0(v2_dir_china, '05-type-repo-01.png'), width = 16, height = 16, units="cm", res=300)
pie(repo$N,
    labels = paste0(repo$lab, "\n", repo$N), 
    col = rep("white", length(repo$N)),
    cex = .8)
dev.off()

p <- ggplot(repo, aes(x=repo_others, y=N)) + theme_minimal() +
    geom_col(fill='grey24') +
    xlab("\nCountries") + ylab("Number of species \n") +
    scale_y_continuous(breaks=ybreaks50)
# both valid and synonym


# For IZB
izb <- df[type.repository.n == "IZB"]
izb <- izb[,.N, by= c("type.country_n")][order(-N)]

png(paste0(v2_dir_china, '05-type-repo-02.png'), width = 16, height = 16, units="cm", res=300)
pie(izb$N,
    labels = paste0(izb$type.country_n, "\n", izb$N), 
    col = rep("white", length(izb$N)),
    cex = .8)
dev.off()

# Might be useful to show number of species deposited in various collections. 
# Would likely parallel the describer’s country but would show importance of 
# Chinese type institution IBZ (Beijing). For a paper may be useful to note %
# total fauna for each country.

# When noting describer’s country origin vs. residence should be specified. 
# In general, more precision on such points are needed for me (much less readers)
# to interpret the info).




