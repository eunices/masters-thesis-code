# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Coauthor networks: data preparation
# Note: analysis done in Python, see folder 2019-09-24-coauthor-networkx
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2020-08-31-jsa-type-v2/00-init/var.R')
source('2020-08-31-jsa-type-v2/subset.R')

# Libraries
library(gridExtra)

# Parameters
dir_base <- "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\"
dir_plot <- paste0(dir_base, "_figures\\_ch3\\_ch3-coauth\\")
theme <- theme_minimal()
date_cutoff <- 2019

# Read/wrangle data
df <- get_df()

df <- df[
  status %in% c("Valid species", "Synonym"),
  c("full.name.of.describer", "idx", "date")
]

df$date <- as.integer(df$date)
df <- df[date <= date_cutoff]
df <- df[order(date)]

df$date.decade <- paste0(substr(as.character(df$date), 1, 3), "0s")
df$coauth <- "Did not coauthor"
df[grepl("; ", full.name.of.describer)]$coauth <- "Coauthor"

df_full <- df

# Count number of authors
df <- df %>% separate_rows(full.name.of.describer, sep="; ")
df <- data.table(unique(df))
df <- df[, list(.N), by=c('idx', 'date.decade', 'date')]
df <- df[order(date.decade)]
df$N <- as.character(df$N)

# Create dataset for coauth
df_did_not_coauth <- df[N == 1]  # for coauth analysis
df_coauth <- df[N >= 2] # for coauth analysis


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Number of co-authoring authors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Number of co-authoring authors"))
dim(df)
table(df$N)
prop.table(table(df$N)) * 100
prop.table(table(df$N %in% c(2,3,4)))*100


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - % of author who coauthored
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

nw <- get_describer_network()

auth_coauth <- unique(c(nw[!is.na(p2)]$p1, nw[!is.na(p2)]$p2))

auth_did_not_coauth <- unique(nw[is.na(p2)]$p1)

auth_did_not_coauth <- unique(
    auth_did_not_coauth[!auth_did_not_coauth %in% auth_coauth]
)

any(auth_did_not_coauth %in% auth_coauth) # check

length(auth_did_not_coauth) / 
    (length(auth_coauth) + length(auth_did_not_coauth)) * 100

length(auth_did_not_coauth)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Mean year of publication for co-authors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

df_bp <- rbind(df_coauth, df_did_not_coauth)
df_bp$date <- as.integer(df_bp$date)
df_bp$coauth <- factor(df_bp$coauth, levels=c("Coauthor", "Did not coauthor"))
df_bp[, list(mean=mean(date), se=sd(date)/sqrt(.N), n=.N), by=coauth]

res <- t.test(
    df_coauth$date, df_did_not_coauth$date, 
    alternative = "two.sided", var.equal = FALSE
)

res
res$p.value

# Plot
plot_bp <- ggplot(df_bp) + 
    geom_boxplot(aes(y=date, x=as.character(coauth))) + 
    theme +
    ylab("Year of description\n") + 
    theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
    scale_fill_manual(name="", values=c("grey80", "grey40"))

# plot_bp

# ggsave(
#     paste0(dir_plot, 'fig-X.png'), plot_bp, units="cm",
#     width=15, height=10, dpi=300
# )

# res <- bartlett.test(df_coauth$date, df_did_not_coauth$date)
# res


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Proportion of authors across years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Proportion of authors across years"))

calc_median <- function(x){
  return(c(y = -0.1, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

plot_auth_decade1 <- ggplot(data = df, aes(x=date.decade, fill=N)) +
        geom_bar(position = "fill") +
        xlab("\nDecade") + 
        ylab("Proportion of species with \nN number of authors\n") +
        theme + scale_fill_grey()

plot_auth_decade2 <- ggplot(data=df, aes(x=date.decade, fill=N)) +
        geom_bar(stat = 'count') +
        xlab("\nDecade") +
        ylab("Number of species with \nN number of authors\n") +
        theme + scale_fill_grey()

gr <- grid.arrange(plot_auth_decade1, plot_auth_decade2)

ggsave(
    paste0(dir_plot, 'fig-1.png'), gr, units="cm", width=30, height=15, dpi=300
)

dim(df_full[date.decade=="1820s"])
df_full[date.decade=="1820s"]
df_full[date.decade=="1820s", .N]
df_full[date.decade=="1820s", .N, by="coauth"]


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Write data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@