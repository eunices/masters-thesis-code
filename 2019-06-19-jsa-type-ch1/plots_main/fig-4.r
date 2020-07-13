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

cols <- names(taxonomic_effort)[grepl(".prop", names(taxonomic_effort))]
cols <- c("years", cols)
des_y <- melt(taxonomic_effort[, ..cols], id.vars="years")

formatstr <- function(string) {
  string <- gsub("N_real_describers.", "", string)
  string <- gsub(".prop", "", string)
  ifelse(string=="1", paste0(string, " species"), paste0("<=", string, " species"))
}

plot_tax <- ggplot(des_y, aes(x=years, y=value, group=variable)) + 
    geom_line(size=.5, colour="grey", linetype='dashed') + 
    geom_point(size=.5, color='grey') + 
    geom_smooth(size=1, colour="black") +
    xlab("Year") + ylab("Proportion of PTEs describing <= N species (%)\n") +
    facet_wrap(. ~variable, ncol=2, labeller=labeller(variable=formatstr), dir="v") +
    scale_y_continuous(limits=c(0, 50), breaks=seq(0,50,10)) +
    theme

ggsave(paste0(dir_plot, 'fig-4.png'), plot_tax, units="cm", width=15, height=9, dpi=300)
