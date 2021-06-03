source('2021-06-02-jsa-type-v2-china/init.r')

dir_base = "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\"
dir_maps = paste0(dir_base, "_maps\\_ch3\\_ch3-flow\\2021-02-01-flow-map-v3\\")

#########################################################
# Number species described for China

rfile <- paste0(dir_maps, "2019-09-22-flow-map-type-loc-des-country.csv")
t <- fread(rfile)

type_loc <- t[des=="CH", c("ori", "N")][order(-N)]
type_loc$ori <- factor(type_loc$ori, levels=type_loc$ori)
type_loc <- type_loc[N>10]

p <- ggplot(type_loc, aes(x=ori, y=N, label=N)) + theme_minimal() +
    geom_col(fill='grey24') +
    xlab("\nCountries") + ylab("Number of descriptions \n")
    # geom_text(size = 3, position = position_stack(vjust = 0.5))
# both valid and synonym

ggsave(
    paste0(v2_dir_china, '04-flow-01.png'), p, units="cm", width=15, height=8, dpi=300
)


#########################################################
# Number species described by describers of China

describers_loc <- t[ori=="CH" & des != "CH", c("des", "N")][order(-N)]
describers_loc$des <- factor(describers_loc$des, levels=describers_loc$des)
describers_loc$N <- as.integer(describers_loc$N)

p <- ggplot(describers_loc, aes(x=des, y=N)) + theme_minimal() +
    geom_col(fill='grey24') +
    xlab("\nCountries") + ylab("Number of species \n") +
    scale_y_continuous(breaks=ybreaks1)
# both valid and synonym

ggsave(
    paste0(v2_dir_china, '04-flow-02.png'), p, units="cm", width=15, height=8, dpi=300
)
