# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Coauthor networks: data preparation
# Note: analysis done in Python, see folder 2019-09-24-coauthor-networkx
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('2019-06-19-jsa-type/init/var.R')
source('2019-06-19-jsa-type/subset.R')

# Parameters
dir_plot = "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\_figures\\_ch3\\_ch3-coauth\\"
theme = theme_minimal()

# Read/wrangle data
df0a <- get_df1(write=F)
df0b <- get_df2(write=F)
df0b <- df0b[status=="Synonym"]
df <- rbind(df0a[,c("full.name.of.describer", "idx", "date.n")],
            df0b[,c("full.name.of.describer", "idx", "date.n")])
df <- df[date.n <= 2018]

df$date.decade <- paste0(substr(as.character(df$date.n), 1, 3), "0s")
df <- df %>% separate_rows(full.name.of.describer, sep="; ")
df <- unique(df)
df <- df[order(date.n)]
df1 <- df[] # for coauth analysis

df <- df[, list(.N), by=c('idx', 'date.decade')]
df <- df[order(date.decade)]
df$N <- as.character(df$N)



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Number of co-authoring authors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Number of co-authoring authors"))
dim(df)
prop.table(table(df$N %in% c(2,3,4)))*100


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Mean year of publication for co-authors
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

nw = get_describer_network()
auth_coauth = unique(c(nw[!is.na(p2)]$p1, nw[!is.na(p2)]$p2))
auth_did_not_coauth = unique(nw[is.na(p2)]$p1)
auth_did_not_coauth = unique(auth_did_not_coauth[!auth_did_not_coauth %in% auth_coauth])
any(auth_did_not_coauth %in% auth_coauth)

length(auth_did_not_coauth)
length(auth_did_not_coauth) / (length(auth_coauth) + length(auth_did_not_coauth)) * 100

# get publication year
des = get_des(write=F)

idxes_coauth = des[full.name.of.describer.n %in% auth_coauth, c("spp_idxes")]
idxes_coauth = unique(separate_rows(idxes_coauth, spp_idxes, sep=", "))
idxes_coauth = idxes_coauth[order(as.integer(spp_idxes))]
dim(idxes_coauth)
df_coauth = df1[idx %in% as.numeric(idxes_coauth$spp_idxes)]
df_coauth$coauth = "Coauthor"

idxes_did_not_coauth = des[full.name.of.describer.n %in% auth_did_not_coauth, c("spp_idxes")]
idxes_did_not_coauth = unique(separate_rows(idxes_did_not_coauth, spp_idxes, sep=", "))
idxes_did_not_coauth = idxes_did_not_coauth[order(as.integer(spp_idxes))]
dim(idxes_did_not_coauth)
df_did_not_coauth = df1[idx %in% as.numeric(idxes_did_not_coauth$spp_idxes)]
df_did_not_coauth$coauth = "Did not coauthor"

df_bp = rbind(df_coauth, df_did_not_coauth)
df_bp$coauth = factor(df_bp$coauth, levels=c("Coauthor", "Did not coauthor"))

df_bp[, list(mean=mean(date.n),
             se=sd(date.n)/sqrt(.N),
             n=.N), by=coauth]

# Plot
plot_bp = ggplot(df_bp) + 
  geom_boxplot(aes(y=date.n, fill=as.character(coauth))) + 
  theme +
  ylab("Year of description\n") + 
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) + 
  scale_fill_manual(name="", values=c("grey80", "grey40"))

plot_bp
# ggsave(paste0(dir_plot, 'fig-4.png'), plot_bp, units="cm", width=15, height=10, dpi=300)


res = t.test(df_coauth$date.n, df_did_not_coauth$date.n, alternative = "two.sided", var.equal = FALSE)
res$p.value
res

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Proportion of authors across years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- Proportion of authors across years"))

calc_median <- function(x){
  return(c(y = -0.1, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

plot_auth_decade1 <- 
    ggplot(data=df, aes(x=date.decade, fill=N)) +
        geom_bar(position = "fill") +
        xlab("\nDecade") + ylab("Proportion of species with \nN number of authors\n") +
        theme + scale_fill_grey()

plot_auth_decade2 <- 
    ggplot(data=df, aes(x=date.decade, fill=N)) +
        geom_bar(stat='count') +
        xlab("\nDecade") + ylab("Number of species with \nN number of authors\n") +
        theme + scale_fill_grey()

gr = grid.arrange(plot_auth_decade1, plot_auth_decade2)
ggsave(paste0(dir_plot, 'fig-4.png'), gr, units="cm", width=30, height=15, dpi=300)


