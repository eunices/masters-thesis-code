dir_script <- '2020-08-31-jsa-type-v2/'
source(paste0(dir_script, "subset.r"))

theme <- theme_minimal()

f <- list.files(v2_dir_data_webapp, pattern="*.csv", full.names=F)

dfs <- list()
counter <- 1
for (i in f) {
    wfile <- paste0(v2_dir_data_webapp, i)
    dfs[[counter]] <- fread(wfile, na.strings=c(""))
    counter <- counter + 1
}

rm(v_continent, v_ecoregions)
rfile <- paste0(v2_dir_data_webapp, "wwf_terr-s.gpkg")
v_ecoregions <- st_read(rfile, quiet = TRUE)

print(paste0("Visualisation for ", f[[1]]))
head(dfs[[1]], 5)

dfs[[1]]$lat <- as.numeric(dfs[[1]]$lat)
dfs[[1]]$lon <- as.numeric(dfs[[1]]$lon)
dfs[[1]]$date <- as.integer(dfs[[1]]$date)

decades <- seq(1800, 2020, by=10)

for (i in decades) {
    xlab <- ""; ylab <- ""
    plt <- ggplot(data=v_ecoregions) +
        xlab(xlab) + ylab(ylab) + theme + 
        geom_point(
            data=dfs[[1]][date<=i], aes(x=lon, y=lat), size = 2, 
            fill='red', colour="red"
        ) +
        ggtitle(i) + 
        geom_sf(fill=NA) 
    
    wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[1]]), i, ".png")
    ggsave(wfile, plt, units="cm", width=30, height=15, dpi=300)
}


# Chapter 1
print(paste0("Visualisation for ", f[[2]]))
head(dfs[[2]], 5)
xlab <- "\nYear"; ylab <- "Number of PTEs\n"
plt <- ggplot(dfs[[2]]) + theme +
    xlab(xlab) + ylab(ylab) +
    geom_point(aes(x=years, y=N_real_describers), size=0.1) +
    geom_line(aes(x=years, y=N_real_describers_roll)) 
wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[2]]), ".png")
ggsave(wfile, plt, units="cm", width=15, height=7, dpi=300)


print(paste0("Visualisation for ", f[[3]]))
head(dfs[[3]], 5)

xlab <- "\nYear"; ylab <- "Number of PTEs (weighted)\n"
plt <- ggplot(dfs[[3]]) + theme +
    xlab(xlab) + ylab(ylab) +
    geom_point(aes(x=years, y=N_weighted_real_describers), size=0.1) +
    geom_line(aes(x=years, y=N_weighted_real_describers_roll)) 
wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[3]]), ".png")
ggsave(wfile, plt, units="cm", width=15, height=7, dpi=300)


print(paste0("Visualisation for ", f[[4]]))
head(dfs[[4]], 5)

dfs[[4]]$N_species_described <- as.factor(dfs[[4]]$N_species_described)

xlab <- "\nYear"; ylab <- "Percentage of PTEs describing \n <=N species\n"
plt <- ggplot(dfs[[4]]) + theme +
    xlab(xlab) + ylab(ylab) +
    geom_point(aes(x=years, y=value), size=0.1) +
    geom_line(aes(x=years, y=value_roll)) + 
    facet_wrap(~N_species_described)
wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[4]]), ".png")
ggsave(wfile, plt, units="cm", width=20, height=9, dpi=300)


print(paste0("Visualisation for ", f[[5]]))
head(dfs[[5]], 5)
# War years


# Chapter 2
print(paste0("Visualisation for ", f[[6]]))
head(dfs[[6]], 5)

xlab <- "\nYear"; ylab <- "Cumulative number of valid species described\n"
plt <- ggplot(dfs[[6]]) + theme +
    xlab(xlab) + ylab(ylab) +
    geom_ribbon(aes(
        x=year,
        ymin=for_cml_value_pred_lwrCI95,
        ymax=for_cml_value_pred_uprCI95
    ), alpha=.2, fill="red", ) + 
    geom_line(aes(x=year, y=for_cml_value_pred_mean), colour="red", size=1.5) + 
    geom_point(aes(x=year, y=cml_value_obs), size=0.1) +
    # geom_line(aes(x=year, y=cml_value_pred_mean)) + 
    facet_wrap(~groupname)
wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[6]]), ".png")
ggsave(wfile, plt, units="cm", width=18, height=10, dpi=300)


print(paste0("Visualisation for ", f[[7]]))
head(dfs[[7]], 5)

xlab <- "\nBiogeographic realm"; ylab <- "Beta coefficient\n"
plt <- ggplot(dfs[[7]]) + theme +
    xlab(xlab) + ylab(ylab) + 
    geom_point(aes(x=groupname, y=slowdown)) + 
    geom_errorbar(aes(
        x=groupname, ymin=slowdown_CI_lower, ymax=slowdown_CI_higher
    )) 
wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[7]]), ".png")
ggsave(wfile, plt, units="cm", width=15, height=10, dpi=300)


# Chapter 3
print(paste0("Visualisation for ", f[[8]]))
head(dfs[[8]], 5)

dfs[[8]]$socioecon <- factor(dfs[[8]]$socioecon,
    c("High income", "Upper middle income", "Lower middle income", "Low income")
)

xlab <- "\nSocioeconomic status of country"; ylab <- "Continent\n"
plt <- ggplot(dfs[[8]]) + theme +
    xlab(xlab) + ylab(ylab) +
    geom_tile(aes(x=socioecon, y=variable, fill=value), color="white") +
    guides(fill=guide_legend(title="Number of describers"))
wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[8]]), ".png")
ggsave(wfile, plt, units="cm", width=24, height=10, dpi=300)


print(paste0("Visualisation for ", f[[9]]))
head(dfs[[9]], 5)
# Flow map

print(paste0("Visualisation for ", f[[10]]))
head(dfs[[10]], 5)

dfs[[10]]$variable <- paste0(dfs[[10]]$variable, dfs[[10]]$significant)

xlab <- "\nVariable"; ylab <- "Coefficient\n"
plt <- ggplot(dfs[[10]]) + theme + coord_flip() +
    xlab(xlab) + ylab(ylab) + 
    geom_hline(yintercept=0, color="grey", linetype=3) +
    geom_point(aes(x=variable, y=coefficient)) + 
    geom_errorbar(aes(
        x=variable, 
        ymin=coefficient-(se*1.96), 
        ymax=coefficient+(se*1.96)
    )) 
wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[10]]), ".png")
ggsave(wfile, plt, units="cm", width=18, height=10, dpi=300)


print(paste0("Visualisation for ", f[[11]]))
head(dfs[[11]], 5)
# Network


print(paste0("Visualisation for ", f[[12]]))
head(dfs[[12]], 5)

xlab <- "\nYear"; ylab <- "Percentage of \nactive female describers\n"
plt <- ggplot(dfs[[12]]) + theme +
    xlab(xlab) + ylab(ylab) +
    geom_point(aes(x=year, y=prop_Fl), size=0.1) +
    geom_line(aes(x=year, y=prop_F_predicted))
wfile <- paste0(v2_dir_data_webapp_img, gsub(".csv", "", f[[12]]), ".png")
ggsave(wfile, plt, units="cm", width=20, height=9, dpi=300)

