library(data.table)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("2019-06-19-jsa-type/init/util.r")
source("2019-06-19-jsa-type/clean/functions.r")

data_folder = "data/2020-07-02-zootaxa/"
data_filepath = paste0(data_folder, "Zootaxa Analayses 2000 onwards file.csv")

fig_folder = "C:/Users/ejysoh/Dropbox/msc-thesis/research/correspondences/2020-07-02-zootaxa/"

# Parameters
theme = theme_minimal()


# Read/ wrangle data
df = read_escaped_data(data_filepath)

df[] <- lapply(df, gsub, pattern='[/r/n]', replacement=' ') 

df <- rename_df_names(df)

df <- replace_df_nas(df)

names(df)

# Column specific wrangling
df$date <- as.numeric(df$date)





# Plot: Zootaxa as major journal 2000 - 2020 periods
df$zootaxa <- ifelse(df$journal == "Zootaxa", "Y", "N")
zootaxa_perc <- df[, .N, by=c("date", "zootaxa")]
zootaxa_perc <- dcast(zootaxa_perc, date ~ zootaxa, value.var="N")
zootaxa_perc[is.na(zootaxa_perc)] <- 0

zootaxa_perc <- zootaxa_perc[date != 0]
zootaxa_perc <- zootaxa_perc[date <= 2019]
zootaxa_perc$perc_Y <- zootaxa_perc$Y/ (zootaxa_perc$N + zootaxa_perc$Y) * 100

plot_zootaxa_perc <- 
    ggplot(zootaxa_perc) +
        geom_line(aes(x=date, y=perc_Y), size=2) +
        scale_x_continuous(minor_breaks = seq(2000, 2020, by=1)) + 
        scale_y_continuous(minor_breaks = seq(0, 100, by=5)) + 
        xlab("") + ylab("Percentage of bee species\n described from Zootaxa (%) \n") + 
        theme
plot_zootaxa_year <- 
    ggplot(zootaxa_perc) +
        geom_line(aes(x=date, y=Y), size=2) +
        scale_x_continuous(minor_breaks = seq(2000, 2020, by=1)) + 
        scale_y_continuous(minor_breaks = seq(0, 100, by=5)) + 
        xlab("") + ylab("Number of bee species\n described from Zootaxa\n") + 
        theme

plot_zootaxa <- grid.arrange(grobs=list(plot_zootaxa_perc, plot_zootaxa_year), ncol=1)

ggsave(paste0(fig_folder, "fig-1.png"), plot_zootaxa, width=21, height=16, units="cm")



# Number of authors per year
authors <- separate_rows(df[, c("date", "full.name.of.describer")], "full.name.of.describer", sep = "; ")
authors <- unique(authors)
length(unique(authors$full.name.of.describer))

zootaxa_auth <- df[journal=="Zootaxa", .N, by=c("date")]
zootaxa_auth <- zootaxa_auth[!is.na(date) & date <= 2019]

plot_zootaxa_auth <- 
    ggplot(zootaxa_auth) +
        geom_line(aes(x=date, y=N), size=2) +
        scale_x_continuous(minor_breaks = seq(2000, 2020, by=1)) + 
        # scale_y_continuous(minor_breaks = seq(0, 100, by=5)) + 
        xlab("") + ylab("Number of unique bee describers\n contributing to Zootaxa\n") + 
        theme

ggsave(paste0(fig_folder, "fig-2.png"), plot_zootaxa_auth, width=21, height=8, units="cm")
