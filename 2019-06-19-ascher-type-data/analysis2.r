# Done in Python, see folder 2019-09-24-coauthor-networkx

source('2019-06-19-ascher-type-data/var.R')
source('2019-06-19-ascher-type-data/subset.R')


df0 <- get_df1(write=F)
df0$date.decade <- paste0(substr(as.character(df0$date.n), 1, 3), "0s")

df0[date.decade=='1820s', c("species", "genus", "full.name.of.describer")]

df <- df0[,c("full.name.of.describer", "idx", "date.decade")]; dim(df)
df <- df %>% separate_rows(full.name.of.describer, sep="; ")
df <- unique(df)
df <- df[, list(.N), by=c('idx', 'date.decade')]
df <- df[order(date.n)]
df$N <- as.character(df$N)

calc_median <- function(x){
  return(c(y = -0.1, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

plot_auth_decade1 <- 
    ggplot(data=df, aes(x=date.decade, fill=N)) +
        geom_bar(position = "fill") +
        xlab("\nDecade") + ylab("Proportion species with \nN number of authors\n") +
        theme_minimal()

plot_auth_decade2 <- 
    ggplot(data=df, aes(x=date.decade, fill=N)) +
        geom_bar(stat='count') +
        xlab("\nDecade") + ylab("Number of species with \nN number of authors\n") +
        theme_minimal()

grid.arrange(plot_auth_decade1, plot_auth_decade2)