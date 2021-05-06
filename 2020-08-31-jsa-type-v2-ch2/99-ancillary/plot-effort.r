# Set up
dir_script <- '2020-08-31-jsa-type-v2/'
source(paste0(dir_script, "subset.r"))

source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')


model <- "version01/BGY-E1-C4-I20000-A0.9-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)

df <- fread(paste0(fdir, 'offset.csv'), na.strings='')

p <- ggplot(df, aes(x=year, y=N)) + 
    geom_line() + 
    facet_wrap(group~.) + ggtitle("") +
    theme_minimal() + xlab("Year") + ylab("Number of publications\n") 

ggsave(paste0(fdir, "te.png"), p, units="cm", width=20, height=10, dpi=300)




model <- "version01/BGY-E2-C4-I8000-A0.8-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)

df <- fread(paste0(fdir, 'offset.csv'), na.strings='')

p <- ggplot(df, aes(x=year, y=N)) + 
    geom_line() + 
    facet_wrap(group~.) + ggtitle("") +
    theme_minimal() + xlab("Year") + ylab("Number of describers\n") 

ggsave(paste0(fdir, "te.png"), p, units="cm", width=20, height=10, dpi=300)


# By family


model <- "version01/FAM-E1-C4-I100000-A0.9-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)

df <- fread(paste0(fdir, 'offset.csv'), na.strings='')

p <- ggplot(df, aes(x=year, y=N)) + 
    geom_line() + 
    facet_wrap(group~.) + ggtitle("") +
    theme_minimal() + xlab("Year") + ylab("Number of publications\n") 

ggsave(paste0(fdir, "te.png"), p, units="cm", width=20, height=10, dpi=300)


model <- "version01/FAM-E2-C4-I8000-A0.8-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)

df <- fread(paste0(fdir, 'offset.csv'), na.strings='')

p <- ggplot(df, aes(x=year, y=N)) + 
    geom_line() + 
    facet_wrap(group~.) + ggtitle("") +
    theme_minimal() + xlab("Year") + ylab("Number of describers\n") 

ggsave(paste0(fdir, "te.png"), p, units="cm", width=20, height=10, dpi=300)


# By latitudinal grouping


model <- "version01/LTY-E1-C4-I20000-A0.99-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)

df <- fread(paste0(fdir, 'offset.csv'), na.strings='')

p <- ggplot(df, aes(x=year, y=N)) + 
    geom_line() + 
    facet_wrap(group~.) + ggtitle("") +
    theme_minimal() + xlab("\nYear") + ylab("Number of publications\n") 

ggsave(paste0(fdir, "te.png"), p, units="cm", width=24, height=10, dpi=300)



model <- "version01/LTY-E2-C4-I100000-A0.99-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)

df <- fread(paste0(fdir, 'offset.csv'), na.strings='')

p <- ggplot(df, aes(x=year, y=N)) + 
    geom_line() + 
    facet_wrap(group~.) + ggtitle("") +
    theme_minimal() + xlab("\nYear") + ylab("Number of describers\n") 

ggsave(paste0(fdir, "te.png"), p, units="cm", width=24, height=10, dpi=300)