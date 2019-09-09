source('2019-06-19-ascher-type-data/init.r')

# Scripts
#############
source('2019-06-19-ascher-type-data/clean.r')
source('2019-06-19-ascher-type-data/df1.r')
source('2019-06-19-ascher-type-data/df2.r')

# Libraries
#############
library(ggplot2)
library(grid); library(gridExtra)


# Exploratory analysis
#############

# Plot world object
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


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - useful columns
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- subset useful columns"))

filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.0-clean.csv")
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

# Subset useful columns
main <- c('idx',                                       # identifier
          'family', 'subfamily', 'tribe',              # taxonomic info
          'genus', 'subgenus', 'species', 'full.name',
          'author', 'date.n', 'full.name.of.describer',  # description info
          'collector.of.type', 'date.of.type.string',    # collector info      
          'date.of.type.yyyy', 'date.of.type.mm', 'date.of.type.dd',
          'years.lag',
          'lat', 'lon',                                  # georeference
          'type.repository', 'country.of.type.repository', 'status', # type info
          'flag', 'duplicated.row') 

loc <- c('type.country', 'type.state',     # locality info
         'type.locality.verbatim', 'type.locality.updated', 'elev.m',
         'source.of.latlon')

checks <- c('other.names', 'original.genus',                          # synonyms
            'type.status', 'type.depository.notes', # specimen status
            'title', 'journal', 'volume', 'issue', 'paper.type',      # publication info
            'global.mapper', 'distributional.footnotes', 'notes')     # distribution info

nat_hist <- c('host.plant', 'host.plant.of.type', 'host.insect.or.prey', 'type.sex', # life history info
            'sociality')


cols <- c(main, loc, checks, nat_hist)

write.csv(df[,..cols], 
          paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_3.2-useful-col.csv"), na='', row.names=F, fileEncoding="UTF-8")




filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_1-idx.csv")
df <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
df[, names(df) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 


filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_3-useful-col")
dfs <- fread(filepath, integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')
dfs[, names(dfs) := lapply(.SD, function(x) gsub('\\"\\"', '\\"', x))] 

dfx[idx==28366]$page.numbers.publication
