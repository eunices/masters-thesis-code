print(paste0(Sys.time(), " --- Prop species describing <=N species"))

taxonomic_effort$N_real_describers.1.prop <- taxonomic_effort$N_real_describers.1 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.2.prop <- taxonomic_effort$N_real_describers.2 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.3.prop <- taxonomic_effort$N_real_describers.3 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.4.prop <- taxonomic_effort$N_real_describers.4 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.5.prop <- taxonomic_effort$N_real_describers.5 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.6.prop <- taxonomic_effort$N_real_describers.6 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.7.prop <- taxonomic_effort$N_real_describers.7 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.8.prop <- taxonomic_effort$N_real_describers.8 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.9.prop <- taxonomic_effort$N_real_describers.9 / taxonomic_effort$N_real_describers * 100
taxonomic_effort$N_real_describers.10.prop <- taxonomic_effort$N_real_describers.10 / taxonomic_effort$N_real_describers * 100

taxonomic_effort$N_real_describers.1.prop_roll <- rollmean(taxonomic_effort$N_real_describers.1.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.2.prop_roll <- rollmean(taxonomic_effort$N_real_describers.2.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.3.prop_roll <- rollmean(taxonomic_effort$N_real_describers.3.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.4.prop_roll <- rollmean(taxonomic_effort$N_real_describers.4.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.5.prop_roll <- rollmean(taxonomic_effort$N_real_describers.5.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.6.prop_roll <- rollmean(taxonomic_effort$N_real_describers.6.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.7.prop_roll <- rollmean(taxonomic_effort$N_real_describers.7.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.8.prop_roll <- rollmean(taxonomic_effort$N_real_describers.8.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.9.prop_roll <- rollmean(taxonomic_effort$N_real_describers.9.prop, 10, fill = list(NA, NULL, NA))  
taxonomic_effort$N_real_describers.10.prop_roll <- rollmean(taxonomic_effort$N_real_describers.10.prop, 10, fill = list(NA, NULL, NA)) 

cols <- names(taxonomic_effort)[grepl(".prop", names(taxonomic_effort))]
cols <- c("years", cols)
des_y <- melt(taxonomic_effort[, ..cols], id.vars="years")

cols <- names(taxonomic_effort)[grepl(".prop_roll", names(taxonomic_effort))]
cols <- c("years", cols)
des_y_roll <- melt(taxonomic_effort[, ..cols], id.vars="years")
des_y_roll$variable <- gsub("_roll", "", des_y_roll$variable)

des_y <- merge(
    des_y, des_y_roll, by=c("years", "variable"), suffixes=c("", "_roll")
)

# Trend analysis
cols <- names(taxonomic_effort)[grepl(".prop_roll", names(taxonomic_effort))]
cols <- c("years", cols)
te_trend <- taxonomic_effort[, ..cols]

results_p <- c()
results_tau <- c()
for (i in 1:10) {
    name <- paste0("N_real_describers.", i, ".prop_roll")
    trend <- te_trend[, get(name)]
    # plot(trend)
    res <- MannKendall(trend)
    results_p <- c(results_p, round(as.numeric(res$sl), 2))
    results_tau <- c(results_tau, round(as.numeric(res$tau), 2))
}

print(paste0(results_tau, collapse = ", "))
print(paste0(results_p, collapse = ", "))
print(which(results_p > 0.05))
print(sum(results_p <= 0.05))

formatstr <- function(string) {
    string <- gsub("N_real_describers.", "", string)
    string <- gsub(".prop", "", string)
    string <- ifelse(
        string=="1", 
        paste0(string, " species"), paste0("<=", string, " species")
    )
    string
}

des_y$variable <- factor(
    des_y$variable, levels = paste0("N_real_describers.", 1:10, ".prop")
)

plot_tax <- ggplot(des_y, aes(x=years, y=value, group=variable)) + 
    geom_line(size=.5, colour="grey", linetype='dashed') + 
    geom_point(size=.5, color='grey') + 
    geom_smooth(size=1, colour="black") +
    geom_line(size=.7, aes(y=value_roll), color='grey50') +
    xlab("Year") + ylab("Proportion of PTEs describing <= N species (%)\n") +
    facet_wrap(
        . ~variable, ncol=2, 
        labeller=labeller(variable=formatstr), dir="v"
    ) +
    scale_y_continuous(limits=c(0, 50), breaks=seq(0,50,10)) +
    theme

ggsave(
    paste0(dir_plot, 'fig-5.png'), plot_tax, units="cm", 
    width=15, height=9, dpi=300
)



