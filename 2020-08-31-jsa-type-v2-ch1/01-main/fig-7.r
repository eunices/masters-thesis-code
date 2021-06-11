options("scipen"=100, "digits"=4)

yng <- tax[, c("full.name.of.describer", "min", "max_corrected", "spp_N")]
yng$mid <- as.integer((yng$max_corrected - yng$min) / 2) + yng$min

# spp_N includes valid species and synonyms

yng[is.na(age_bin),]$mid

summary(yng$mid)
breaks <- c(seq(1750, 2020, 50), 2020)
yng$age_bin <- cut(yng$min, breaks=breaks, include.lowest=TRUE, right=FALSE)
labels <- c(paste0(breaks[1:5], "-", breaks[2:6]-1), paste0("After ", breaks[6]))
yng$age_bin <- factor(
    yng$age_bin, levels=levels(yng$age_bin), labels=labels
)

summary(yng$spp_N)
breaks <- c(seq(1,10, 2), seq(11, 100, 20), seq(101, 700, 200), 6200)
yng$spp_bin <- cut(yng$spp_N, breaks=breaks, include.lowest=TRUE, right = FALSE)

labels <- unlist(lapply(levels(yng$spp_bin), function(x) {
    first <- gsub("\\[", "", strsplit(as.character(x), ",")[[1]][1])
    second <- as.integer(gsub("\\)", "", strsplit(as.character(x), ",")[[1]][2]))-1
    ifelse(is.na(second), 
        paste0(">", first),
        paste0(first, "-", second)
    )
}))

yng$spp_bin <- factor(
    yng$spp_bin, levels=levels(yng$spp_bin), labels=labels
)

unique(yng$age_bin)
unique(yng$spp_bin)

yng <- yng[order(spp_bin, age_bin)]

summary <- yng[, {
    total = .N
    .SD[,.(frac = .N/total), by=age_bin]
}, by=spp_bin]

p <- ggplot(summary, aes(x = spp_bin, y = frac*100, fill = age_bin)) +
  geom_bar(stat="identity", width = 0.7, color="black", size=.2) +
  labs(x = "\nNumber of species", y = "Percentage of PTEs (%)\n", fill = "First\npublication date") +
  theme_minimal(base_size = 9) + 
  scale_y_continuous(breaks=ybreaks10) +
  scale_x_discrete() +
  theme(panel.grid.major.x = element_blank()) +
  scale_fill_brewer(palette="Greys")

ggsave(
    paste0(dir_plot, 'fig-7a.png'), p, units="cm", 
    width=17.5, height=6, dpi=300
)

summary <- yng[, {
    total = .N
    .SD[,.(frac = .N/total), by=spp_bin]
}, by=age_bin]

library(RColorBrewer)
getPalette <- colorRampPalette(brewer.pal(9, "Greys"))

p <- ggplot(summary, aes(x = age_bin, y = frac*100, fill = spp_bin)) +
  geom_bar(stat="identity", width = 0.7, color="black", size=.2) +
  labs(x = "\nFirst publication date", y = "Percentage of PTEs (%)\n", fill = "Number of species") +
  theme_minimal(base_size = 7) + 
  scale_y_continuous(breaks=ybreaks10) +
  scale_x_discrete() +
  theme(panel.grid.major.x = element_blank()) +
  scale_fill_manual(values=getPalette(length(levels(yng$spp_bin))))

ggsave(
    paste0(dir_plot, 'fig-7b.png'), p, units="cm", 
    width=17.5, height=10, dpi=300
)

summary[age_bin=="After 2000"]
