print(paste0(Sys.time(), " --- tax per person"))


taxonomic_effort_per_person <- merge(taxonomic_effort, lp_pop, by.x="years", by.y="year",
                                     all.x=T, all.y=F) 
# https://www.worldometers.info/world-population/
taxonomic_effort_per_person[years=="2016"]$pop <- 7464022049	
taxonomic_effort_per_person[years=="2017"]$pop <- 7547858925
taxonomic_effort_per_person[years=="2018"]$pop <- 7631091040

taxonomic_effort_per_person$tax_per_mil_person <- taxonomic_effort_per_person$N_real_describers /
    (taxonomic_effort_per_person$pop /10^9)

yscale = .5
p8b <- ggplot(taxonomic_effort_per_person, aes(x=years, y=tax_per_mil_person)) + 
    xlab("") + ylab("Number of PTEs \nper billion persons (black)") + 
    theme +
    geom_point(size=1, color='grey') + 
    geom_line(size=.5, color='grey', linetype='dashed') +
    geom_line(size=1, y=rollmean(taxonomic_effort_per_person$tax_per_mil_person, 10, 
              fill = list(NA, NULL, NA))) +
    geom_line(aes(y=pop/(10^9)/yscale), size=1, color='red') + 
    scale_x_continuous(breaks=ybreaks50, minor_breaks=ybreaks10) +
    scale_y_continuous(sec.axis = sec_axis(~ .*yscale, name="World population,\nbillions (red)"))

max(taxonomic_effort_per_person$tax_per_mil_person)
taxonomic_effort_per_person[years %in% 2015:2018]$tax_per_mil_person

ggsave(paste0(dir_plot, '_si/fig-1.png'), p8b, units="cm", width=21, height=5, dpi=300)