print(paste0(Sys.time(), " --- tax per person"))

ggsave(paste0(dir_plot, '_si/fig-7.png'), p8b, units="cm", width=21, height=5, dpi=300)