# Libraries
#############
library(ggplot2)
library(grid); library(gridExtra)

# Exploratory analysis
#############

# # Plot world object
# pdf('plots/2019-06-19-type-data-map.pdf', width=21, height=10)
# ggplot() +
#   geom_sf(data = shp, fill = NA, ) +
#   geom_sf(data = ll, colour = "slategray3") + theme_minimal()
# dev.off()

df[,.(.N), by=.(host.plant)][order(-N)]               # not useful
df[,.(.N), by=.(host.insect.or.prey)][order(-N)]      # not useful
df[,.(.N), by=.(type.status)][order(-N)]              # not useful
df[,.(.N), by=.(status)][order(-N)]                   # not useful
df[,.(.N), by=.(type.repository)][order(-N)]          # not so useful?
df[,.(.N), by=.(verificiation.of.type.repository)][order(-N)]  # not useful
df[,.(.N), by=.(paper.type)][order(-N)]               # not useful
df[,.(.N), by=.(type.sex)][order(-N)]                 # not useful

# Ref
check_df <- function() {
    filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.0-clean.csv")
    ref1 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
    ref1[, names(ref1) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

    filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_2-clean.csv")
    ref2 <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
    ref2[, names(ref2) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

    cols <- c("idx", "genus", "species", "date.n", "author", "paper.type", 
            "title", "journal", "volume", "issue", 
            "page.numbers.publication", "country.of.publication", "city.of.publication")
    ref <- rbind(ref1[, ..cols], ref1[, ..cols])
    ref
}

check <- check_df()

