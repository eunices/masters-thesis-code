# Set up
dir_script <- '2020-08-31-jsa-type-v2/'
source(paste0(dir_script, "subset.r"))

source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')


model <- "version01/BGY-E1-C4-I20000-A0.9-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)

df <- fread(paste0(fdir, 'offset.csv'), na.strings='')

ggplot(df, aes(x=year, y=N)) + 
    geom_line() + geom_smooth() + 
    facet_wrap(group~.) + ggtitle("Taxonomic effort by publication") +
    theme_minimal() + xlab("") + ylab("") 


model <- "version01/BGY-E2-C4-I8000-A0.8-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)

df <- fread(paste0(fdir, 'offset.csv'), na.strings='')

ggplot(df, aes(x=year, y=N)) + 
    geom_line() + geom_smooth() + 
    facet_wrap(group~.) + ggtitle("Taxonomic effort by describers") +
    theme_minimal() + xlab("") + ylab("") 