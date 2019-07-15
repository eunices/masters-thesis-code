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
df[,.(.N), by=.(host.plant)][order(-N)]
df[,.(.N), by=.(host.insect.or.prey)][order(-N)]
df[,.(.N), by=.(type.status)][order(-N)]
df[,.(.N), by=.(status)][order(-N)]
df[,.(.N), by=.(type.repository)][order(-N)]
df[,.(.N), by=.(verificiation.of.type.repository)][order(-N)]
df[,.(.N), by=.(paper.type)][order(-N)]
df[,.(.N), by=.(type.sex)][order(-N)]

