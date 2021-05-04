
bdir <- "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\"

odir <- paste0(
    bdir, "research\\thesis-manuscripts\\global-trajectories-bee-discovery-output\\"
)



# Map data

dir_script <- '2020-08-31-jsa-type-v2/'
source(paste0(dir_script, "subset.r"))

df <- get_df()

df$date <-  as.integer(df$date)
df <- df[
    duplicated == FALSE & status %in% c("Valid species"), 
    c("genus", "species", "status", "date", "lat", "lon")
]
df <- df[date <= 2019,]

rfile <- paste0(odir, "fig-2\\data\\lat-lon.csv")
fwrite(df, rfile)


pts <- c(1914, 1919, 1939, 1945)
yr <- df[, .N,by="date"]
yr$date <- as.integer(yr$date)
yr <- yr[order(date)]
yr$cumsum <- cumsum(yr$N)

max <- max(yr$N)
xlab <-  "\nYear"; ylab <- "Number of species\n"
plt <- ggplot(yr) + theme_minimal(base_size=14) +
    theme(plot.title = element_text(hjust = -.12)) +
    annotate("rect", xmin=pts[1], xmax=pts[2], ymin=0, ymax=max, fill="red", alpha=0.2) +
    annotate("rect", xmin=pts[3], xmax=pts[4], ymin=0, ymax=max, fill="red", alpha=0.2) +
    geom_line(aes(x=date, y=N)) + 
    ylab(ylab) + xlab(xlab) +
    scale_x_continuous(breaks=ybreaks20, limits=c(1750, 2020)) +
    ggtitle("(a)")
wfile <- paste0(odir, "fig-1a.png")
ggsave(wfile, plt, units="cm", width=20, height=10, dpi=300)
xlab <-  "\nYear"; ylab <- "Cumulative number of species\n"
plt <- ggplot(yr) + theme_minimal(base_size=14) +
    theme(plot.title = element_text(hjust = -.14)) +
    geom_line(aes(x=date, y=cumsum)) + 
    geom_ribbon(aes(
        x=date,
        ymin=0,
        ymax=cumsum
    ), alpha=.1, fill="#faef1f") + 
    ylab(ylab) + xlab(xlab) +
    scale_x_continuous(breaks=ybreaks20, limits=c(1750, 2020)) +
    theme(plot.title = element_text(hjust = -.12)) +
    ggtitle("(b)") 
wfile <- paste0(odir, "fig-1b.png")
ggsave(wfile, plt, units="cm", width=20, height=10, dpi=300)

# Table data 

source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')


model_analyse <- "BGY-E0-C4-I8000-A0.8-T12-F25-V0"
model_predict <- "BGY-E1-C4-I20000-A0.9-T12-F25-V0"
model_person <- "BGY-E2-C4-I8000-A0.8-T12-F25-V0"

# Get results.csv from model_analyse
dir_model_folder <- paste0(dir_analysis_edie_model, model_analyse, "/")
rfile <- paste0(dir_model_folder, "output/results.csv")
results_analyse <- fread(rfile)

dir_model_folder <- paste0(dir_analysis_edie_model, model_predict, "/")
rfile <- paste0(dir_model_folder, "output/results.csv")
results_predict <- fread(rfile)

results_analyse$group <- NULL
results_analyse$observed_species <- NULL
results_analyse$expected_median <- NULL
results_analyse$expected_CI_lower <- NULL
results_analyse$expected_CI_higher <- NULL
results_analyse$fore_mu <- NULL
results_analyse$fore_lower <- NULL
results_analyse$fore_upper <- NULL

head(results_analyse,3)

results_predict$group <- NULL
results_predict$expected_median <- NULL
results_predict$expected_CI_lower <- NULL
results_predict$expected_CI_higher <- NULL
results_predict$slowdown <- NULL
results_predict$slowdown_CI_lower <- NULL
results_predict$slowdown_CI_higher <- NULL

head(results_predict, 3)

r <- merge(results_analyse, results_predict, by="groupname", all.x=T, all.y=T)

rfile <- paste0(odir, "table-1.csv")
fwrite(r, rfile)


# Fig


# Get graph from model_predict
models <- c(model_analyse, model_predict, model_person)
# models <- c(model_predict)

counter <- 1
for (i in models) {
    print(paste0("MODEL: ", i))
    dir_model_folder <- paste0(dir_analysis_edie_model, i, "/")

    # Load posterior simulation
    load(paste0(dir_model_folder, "post.data"))     # as "allsim"

    # Load forecast predictions
    load(paste0(dir_model_folder, "forecast.data")) # as "forecast"

    # Original data
    data_raw <- read.csv(
        paste0(dir_model_folder, "data.csv"), 
        na.strings=c(""),
        stringsAsFactor = TRUE
    ) 

    # R data
    data <- read_rdump(paste0(dir_model_folder, "count_info.data.R")) 


    # Map model indices to original variables
    mapping <- unique(data.frame(
        groupname = as.character(data_raw$group),
        group = as.numeric(data_raw$group)
    ))

    # Forecast
    forsim <- data.table(convert_forecast_to_df(data, data_raw, forecast))
    names(forsim)[names(forsim) == "index"] <- "year"
    forsim <- forsim[, list(
        for_cml_value_pred_median = as.integer(median(value)),
        for_cml_value_pred_lwrCI80 = as.integer(quantile(value, 0.1)),
        for_cml_value_pred_uprCI80 = as.integer(quantile(value, 0.9)),
        for_cml_value_pred_mean = as.integer(mean(value))
    ), by=c("year", "group")]
    forsim <- merge(forsim, mapping, by="group", all.x=T, all.y=F)
    forsim$group <- NULL

    # Simulated and observed
    li_df <- summarize_simulations_observed(data, allsim)
    Z <- li_df$Z # Counts for each year for sim and actual, df

    # Simulated
    Y <- data.table(Z)
    sim <- Y[sim != 0, c("year", "group", "cml_value", "sim")]
    sim <- merge(sim, mapping, by="group", all.x=T, all.y=F)
    sim$group <- NULL

    # Observed
    obs <- Y[sim == 0, c("year", "group", "cml_value")]
    obs <- merge(obs, mapping, by="group", all.x=T, all.y=F)
    obs$group <- NULL

    # But first, add the final year's count
    final_counts <- obs[year==max(year), list(count = max(cml_value)),
        by= c("year", "groupname")]
    final_counts$year <- NULL
    forsim <- merge(forsim, final_counts, by=c("groupname"), all.x=T, all.y=T)
    forsim$for_cml_value_pred_median <- 
        forsim$for_cml_value_pred_median + forsim$count
    forsim$for_cml_value_pred_lwrCI80 <- 
        forsim$for_cml_value_pred_lwrCI80 + forsim$count
    forsim$for_cml_value_pred_uprCI80 <-
        forsim$for_cml_value_pred_uprCI80 + forsim$count
    forsim$for_cml_value_pred_mean <-
        forsim$for_cml_value_pred_mean + forsim$count
    forsim$count <- NULL
    
    

    if(i == model_predict) {
	    sim <- sim %>% # subset a sample of simmed series
            split( . , .$group) %>%   # group by group
            lapply( . , function(oo){ # for each group

                ids <- sample(unique(oo$sim), 200)
                oo[oo$sim %in% ids, ]

            }) %>% rbind.fill %>% data.table

        xlab <- "\nYear"
        ylab <- "Cumulative number of valid species described\n"
        
        plt <- ggplot() + theme_minimal(base_size=14) + 
            guides(fill=F) + 
            theme(
                panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent", colour = NA)
            )+
            xlab(xlab) + ylab(ylab) +

            # Forecast (for future)
            geom_ribbon(data=forsim, aes(x=year,
                ymin=for_cml_value_pred_lwrCI80,
                ymax=for_cml_value_pred_uprCI80
            ), alpha=.1, fill="red", inherit.aes=F) + 
            geom_line(data=forsim,
                aes(x=year, y=for_cml_value_pred_mean), colour="red", size=1,
                inherit.aes=F
            ) + 

            # Observed (fill)
            geom_ribbon(data=obs, aes(
                x=year, ymin=0, ymax=cml_value, fill=groupname
            ), alpha=1, inherit.aes=F) + 

            # Simulations (within observed timeframe)
            geom_path(data=sim, aes(
                x=year, y=cml_value, group=sim
            ), col = "#5b5b5b", alpha = 0.05, inherit.aes=F) + 

            # Observed (line)
            geom_path(
                data=obs, aes(x=year, y=cml_value), size=1, inherit.aes=F
            ) +

            facet_wrap(~groupname) + 
            scale_y_continuous(limits=c(0,6000), breaks=seq(0, 6000, 2000)) +

            # facet_wrap(~groupname, scales = "free_y") + 

            scale_x_continuous(limits=c(1758, 2025)) +
            scale_fill_manual(values=c(
                "#994e95", # AA
                "#edad08", # AT 
                "#73af48", # IM
                "#cc503e", # NA 
                "#0f8554", # NT
                "#1aa0e6", # OC 
                "#38a6a5" # PA 
            ))

        wfile <- paste0(odir, "fig-2/fig-2-graph.png")
        ggsave(wfile, plt, units="cm", width=26, height=15, dpi=300)
    }

    options <- c("(a)", "(b)", "(c)")
    title <- options[counter] 

    xlab <- "\nYear"; ylab <- "Cumulative number of species\n"
    plt <- ggplot() + theme_minimal(base_size=16) + 
        xlab(xlab) + ylab(ylab) +

        # Forecast (for future)
        geom_ribbon(data=forsim, aes(x=year,
            ymin=for_cml_value_pred_lwrCI80,
            ymax=for_cml_value_pred_uprCI80
        ), alpha=.1, fill="red", inherit.aes=F) + 
        geom_line(data=forsim,
            aes(x=year, y=for_cml_value_pred_mean), colour="red", size=1,
            inherit.aes=F
        ) + 

        # Simulations (within observed timeframe)
        geom_path(data=sim, aes(
            x=year, y=cml_value, group=sim
        ), col = "#5b5b5b", alpha = 0.05, inherit.aes=F) + 

        # Observed (line)
        geom_path(
            data=obs, aes(x=year, y=cml_value), size=1, inherit.aes=F
        ) +

        facet_wrap(~groupname) + 
        scale_y_continuous(limits=c(0,6000), breaks=seq(0, 6000, 2000)) +
        facet_wrap(~groupname, scales="free_y") +
        theme(plot.title = element_text(hjust = -.1)) +
        ggtitle(title)
    
    wfile <- paste0(odir, "supplementary-fig-2-", counter, ".png")
    ggsave(wfile, plt, units="cm", width=26, height=13.5, dpi=300)

    counter <- counter + 1
}





