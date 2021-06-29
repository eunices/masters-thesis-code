
# Set up
dir_script <- '2020-08-31-jsa-type-v2/'
source(paste0(dir_script, "subset.r"))

source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')




# plot long-term trend
model <- "version01/BGY-E0-C4-I8000-A0.8-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)
df0 <- fread(paste0(fdir, "output/results.csv"), na.strings='')

model <- "version01/BGY-E1-C4-I20000-A0.9-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)
df1 <- fread(paste0(fdir, "output/results.csv"), na.strings='')

model <- "version01/BGY-E2-C4-I8000-A0.8-T12-F25-V0/"
fdir <- paste0(dir_analysis_edie_model, model)
df2 <- fread(paste0(fdir, "output/results.csv"), na.strings='')

df <- merge(
    df0[, c("groupname", "slowdown", "slowdown_CI_lower", "slowdown_CI_higher")],
    df1[, c("groupname", "slowdown", "slowdown_CI_lower", "slowdown_CI_higher")],
    by = "groupname", suffixes=c("", "1"),
)

df <- merge(
    df,
    df2[, c("groupname", "slowdown", "slowdown_CI_lower", "slowdown_CI_higher")],
    by = "groupname", suffixes=c("", "2"),
)

df <- melt(df,  id.vars = c("groupname"), variable.name = "type")
df$group <- "noTE"
df[grepl("1", type)]$group <- "TEP"
df[grepl("2", type)]$group <- "TED"

df$type <- gsub("1|2", "", df$type)

df <- dcast(df, groupname + group ~ type , value.var="value")

p <- ggplot(df, aes(x=groupname, y=slowdown, fill=group)) + 
    geom_bar(stat='identity', position=position_dodge()) +
    geom_errorbar(aes(ymin=slowdown_CI_lower, ymax=slowdown_CI_higher), width=.2,
        position=position_dodge(.9)) + 
    ggtitle("") + 
    scale_y_continuous(breaks=seq(-0.02, 0.005, 0.004)) +
    scale_fill_manual(values = c(
            "#1D845F", "#1E88E5", "#FFC107"
        )
    ) + labs(fill = "") +
    # scale_fill_manual(values=c(
    #     "#994e95", # AA
    #     "#edad08", # AT 
    #     "#73af48", # IM
    #     "#cc503e", # NA 
    #     "#0f8554", # NT
    #     "#1aa0e6", # OC 
    #     "#38a6a5" # PA 
    # )) +
    theme_minimal() + xlab("\nBiogeographic realm") + ylab("Long-term trend\n") 

dir_ms <- "C:/Users/ejysoh/Dropbox/msc-thesis/research/thesis-manuscripts/global-trajectories-bee-discovery-output/"
ggsave(paste0(dir_ms, "fig-4.png"), p, units="cm", width=15, height=8, dpi=300)

