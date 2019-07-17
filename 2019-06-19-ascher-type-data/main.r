source('2019-06-19-ascher-type-data/init.r')
source('2019-06-19-ascher-type-data/clean.r')
source('2019-06-19-ascher-type-data/df1.r')

# Plot world object
pdf('plots/2019-06-19-type-data-map.pdf', width=21, height=10)
ggplot() +
  geom_sf(data = shp, fill = NA, ) +
  geom_sf(data = ll, colour = "slategray3") + theme_minimal()
dev.off()

# Exploratory analysis
df[,.(.N), by=.(host.plant)][order(-N)]               # not useful
df[,.(.N), by=.(host.insect.or.prey)][order(-N)]      # not useful
df[,.(.N), by=.(type.status)][order(-N)]              # not useful
df[,.(.N), by=.(status)][order(-N)]                   # not useful
df[,.(.N), by=.(type.repository)][order(-N)]          # not so useful?
df[,.(.N), by=.(verificiation.of.type.repository)][order(-N)]  # not useful
df[,.(.N), by=.(paper.type)][order(-N)]               # not useful
df[,.(.N), by=.(type.sex)][order(-N)]                 # not useful

df$date.decade <- paste0(substr(df$date.n, 1, 3), "0s")
ggplot(df[!is.na(date.n),.(.N), by=.(date.decade)], aes(x=date.decade, y=N)) + geom_bar(stat="identity") + theme_minimal()

# ggplotdf[,.(.N), by=.(date.n)], aes(x=date.n, y=N)) + geom_line() +
#   scale_x_date(format = "%Y") + xlab("") + ylab("")


