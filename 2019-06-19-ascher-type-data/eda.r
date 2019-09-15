source('2019-06-19-ascher-type-data/main.r')
source('2019-06-19-ascher-type-data/test.r')

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
# Load clean data frames (useful columns only)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

valid_species_us <- function () {
    print(paste0(Sys.time(), " --- subset useful columns for valid species"))
    filepath <- paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-col.csv")
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

    df[,..cols]
}

not_valid_species_us  <- function() {
    print(paste0(Sys.time(), " --- subset useful col"))

    df_s <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.3-clean-coll.csv"), na.strings=c('', 'NA'), encoding="UTF-8", quote='"')

    describer_cols <- c("idx", "author", "full.name.of.describer", "describer.gender", 
            "dob.describer", "dod.describer",
            "origin.country.describer", "residence.country.describer", "institution.of.describer")
    collector_cols <- c("idx", "collector.of.type", "full.name.of.collector",
            "title.of.collector", "collector.gender", "info.about.collector", "date.of.type.yyyy")
    relevant_cols <- c('idx', 'genus', 'original.genus', 'species', 'status', 'correct_synonym', 'date.n')
    publication_cols <- c("idx", "genus", "species", "date.n", "author", "paper.type", 
            "paper.authors", "title", "journal", "volume", "issue", 
            "page.numbers.publication", "country.of.publication", "city.of.publication")


    cols <- unique(c(relevant_cols, publication_cols, describer_cols, collector_cols))
    cols <- cols[cols %in% names(df_s)]

    df_s[,..cols]
}

