
bdir <- "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\"

odir <- paste0(
    bdir, "research\\manuscripts\\global-trajectory-bee-discovery-output\\"
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
model_predict <- "BGY-E1-C4-I20000-A0.9-T12-F10-V0"
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

    # Forecast information

    forsim <- data.table(convert_forecast_to_df(data, data_raw, forecast))

    head(forsim)

    forsim <- forsim[, list(
        for_cml_value_pred_median = as.integer(median(value)),
        for_cml_value_pred_lwrCI95 = as.integer(quantile(value, 0.025)),
        for_cml_value_pred_uprCI95 = as.integer(quantile(value, 0.925)),
        for_cml_value_pred_mean = as.integer(mean(value))
    ), by=c("index", "group")]

    names(forsim)[names(forsim) == "index"] <- "year"

    # Up till year 2019

    li_df <- summarize_simulations_observed(data, allsim)
    Z <- li_df$Z                      # Counts for each year for sim and actual, df

    Y <- data.table(Z)
    sim_median <- Y[sim != 0, list(
        cml_value_pred_median = as.integer(median(cml_value)),
        cml_value_pred_lwrCI95 = as.integer(quantile(cml_value, 0.025)),
        cml_value_pred_uprCI95 = as.integer(quantile(cml_value, 0.925)),
        cml_value_pred_mean = as.integer(mean(cml_value))
    ), by=c("year", "group")]
    obs <- Y[sim == 0, c("year", "group", "cml_value")]
    names(obs)[3] <- "cml_value_obs"

    # sanity check
    check <- data.table(data_raw)[, .N, by=c("group", "year")][order(group, year)]
    check[, cml:= cumsum(N), by="group"]
    head(check[group=="NA"], 20)

    all <- merge(obs, sim_median, by=c("year", "group"))


    # Combine with forecasts
    head(all)
    head(forsim)

    final_counts <- all[year==max(year),list(
        count = max(cml_value_obs)
    ), by= c("year", "group")]
    final_counts$year <- NULL

    forsim <- merge(forsim, final_counts, by=c("group"), all.x=T, all.y=T)

    forsim$for_cml_value_pred_median <- 
        forsim$for_cml_value_pred_median + forsim$count
    forsim$for_cml_value_pred_lwrCI95 <- 
        forsim$for_cml_value_pred_lwrCI95 + forsim$count
    forsim$for_cml_value_pred_uprCI95 <-
        forsim$for_cml_value_pred_uprCI95 + forsim$count
    forsim$for_cml_value_pred_mean <-
        forsim$for_cml_value_pred_mean + forsim$count
    forsim$count <- NULL

    head(all)


    all <- merge(all, forsim, by=c("year", "group"), all.x=T, all.y=T)

    all <- merge(all, mapping, by="group", all.x=T, all.y=F)
    all$group <- NULL


    if(i == model_predict) {
        rfile <- paste0(odir, "fig-2/fig-2-data.csv")
        fwrite(all, rfile, na="")

        head(all, 3)

        xlab <- "\nYear"; ylab <- "Cumulative number of valid species described\n"
        plt <- ggplot(all) + theme_minimal(base_size=16) + 
            guides(fill=F) + 
            theme(panel.background = element_rect(fill = "transparent", colour = NA))+
            xlab(xlab) + ylab(ylab) +
            geom_ribbon(aes(
                x=year,
                ymin=for_cml_value_pred_lwrCI95,
                ymax=for_cml_value_pred_uprCI95
            ), alpha=.1, fill="red") + 
            geom_line(aes(x=year, y=for_cml_value_pred_mean), colour="red", size=1.5) + 
            geom_ribbon(aes(
                x=year,
                ymin=0,
                ymax=cml_value_obs, fill=groupname
            ), alpha=.9) + 
            geom_point(aes(x=year, y=cml_value_obs), size=0.1) +
            facet_wrap(~groupname) + 
            scale_y_continuous(limits=c(0,6000), breaks=seq(0, 6000, 2000)) +
            scale_fill_manual(values=c(
                "#e1d8b9", # AA
                "#fdfce0", # AT 
                "#96bd8f", # IM
                "#86d0ff", # NA 
                "#bbdfac", # NT
                "#42c9aa", # OC 
                "#b2f0fe" # PA 
            ))
            # scale_y_continuous(limits=c(0,200), breaks=seq(0, 200, 50))
        wfile <- paste0(odir, "fig-2/fig-2-graph.png")
        ggsave(wfile, plt, units="cm", width=26, height=15, dpi=300)
    }

    options <- c("(a)", "(b)", "(c)")
    title <- options[counter] 

    xlab <- "\nYear"; ylab <- "Cumulative number of species\n"
    plt <- ggplot(all) + theme_minimal(base_size=16) + 
        xlab(xlab) + ylab(ylab) +
        geom_ribbon(aes(
            x=year,
            ymin=for_cml_value_pred_lwrCI95,
            ymax=for_cml_value_pred_uprCI95
        ), alpha=.1, fill="red") + 
        geom_line(aes(x=year, y=for_cml_value_pred_mean), colour="red", size=1.5) + 
        geom_ribbon(aes(
            x=year,
            ymin=cml_value_pred_lwrCI95,
            ymax=cml_value_pred_uprCI95
        ), alpha=.1, fill="blue") + 
        geom_line(aes(x=year, y=cml_value_pred_mean), colour="blue", size=1.5) + 
        geom_point(aes(x=year, y=cml_value_obs), size=0.1) +
        facet_wrap(~groupname, scales="free_y") +
        theme(plot.title = element_text(hjust = -.1)) +
        ggtitle(title)
    
    wfile <- paste0(odir, "supplementary-fig-2-", counter, ".png")
    ggsave(wfile, plt, units="cm", width=26, height=13.5, dpi=300)

    counter <- counter + 1
}





