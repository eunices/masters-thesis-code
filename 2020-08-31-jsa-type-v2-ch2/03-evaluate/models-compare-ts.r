# Compare MAPE for different validation
# of the best model to be used in forecasting

source('2020-08-31-jsa-type-v2-ch2/03-evaluate/init.r')


# Inititalize parameters
model <- c("BGY-E1-C4-I20000-A0.99-T12-F50-V50")

dir_model_folder <- paste0(dir_analysis_edie_model, model, "/")
model_params <- parse_model_identifier(model)

# Original data
file <-paste0(dir_model_folder, "data.csv")
data_raw <- read.csv(file, na.strings=c(""), stringsAsFactors = T) 

# Map model indices to original variables
mapping <- unique(data.frame(
    groupname = as.character(data_raw$group),
    group = as.numeric(data_raw$group)
))

# Read data
files <- dir(dir_model_folder, pattern = 'count_info.data.R', full.names = TRUE)
data <- read_rdump(files)

# Read data
files <- dir(
    dir_model_folder, 
    pattern = 'count_info_ref.data.R',
    full.names = TRUE
)

data_ref <- read_rdump(files)

# Read model
load(paste0(dir_model_folder, "fit.data"))

# Test
posterior_sim(data, fit)
posterior_forecast(data, 5, fit)


# Predict for 5 (and repeat for 10, 15, 20, 25)
n_samples <- 100

dfs <- lapply(seq(5, model_params$va, 5), function(ptime) {

    print(paste0("-------------- Forecast duration: ", ptime, " years"))
    
    first_year <- max(data_raw$year) - model_params$va

    # Using posterior_sim, predict using data itself
    predictions_sim <- mclapply(1:n_samples, mc.cores = 1, function(ii) {
        predictions_actual <- posterior_sim(data_ref, fit)
        predictions_actual <- lapply(predictions_actual, function(x) {
            x[(length(x)-model_params$va+1):(length(x)-model_params$va+ ptime)]
        })
        predictions_actual
    })

    # Using posterior_forecast, predict using naive method
    predictions_forecast <- mclapply(1:n_samples, mc.cores = 1, function(ii) {
        posterior_forecast(data, ptime, fit)
    })

    # Coerce these lists of lists to data.table
    sim_li <- lapply(1:n_samples, function(i) {
        x <- predictions_sim[[i]]
        list_to_df(x, i)
    })

    forecast_li <- lapply(1:n_samples, function(i) {
        x <- predictions_forecast[[i]]
        list_to_df(x, i)
    })

    sim_df <- rbindlist(sim_li)
    sim_df$type <- "forecast"
    forecast_df <- rbindlist(forecast_li)
    forecast_df$type <- "naive forecast"
    df <- rbind(forecast_df, sim_df)

    # Wide to long
    df_long <- melt(df, id.vars = c("group", "sim", "type"))
    names(df_long)[which(names(df_long) == "variable")] <- "year"
    names(df_long)[which(names(df_long) == "value")] <- "model"
    df_long$year <- 
        as.integer(gsub("year_", "", df_long$year)) + first_year - 1
    df_long$group <- mapping[match(df_long$group, mapping$group),]$groupname

    # Merge with actual counts
    df_obs <- data.table(data_raw)[,
        list(obs = .N),
        by = c("year", "group")
    ]

    df_long <- merge(
        df_long, df_obs,
        by = c("year", "group")
    )

    df_long$ptime <- ptime

    df_long

})

# Calculate MAPE
dfs <- rbindlist(dfs)
dfs$diff <- abs(dfs$model - dfs$obs)
dfs$perc <- dfs$diff / dfs$obs

prop <- 1.2 # proportion of MAPE

summary <- dfs[, list(MAPE = mean(perc)), by = c("ptime", "type")]
summary$MAPE1pt5 <- summary$MAPE * prop
summary$MAPEplt <- ifelse(
    summary$type == "naive forecast", 
    summary$MAPE1pt5,
    summary$MAPE
)

# Plot MAPE against ptime

# "naive forecast": not using data
# "forecast": using data
# "best window": largest window where MAPE of "naive forecast" >= "forecast"

p1 <- ggplot(summary, aes(x = ptime, y = summary$MAPEplt, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") + theme +
    ylab("MAPE (%)\n") + xlab("\nPrediction duration") +
    scale_fill_manual(
        values = c('#999999','#E69F00'),
        name = "Type",
        labels = c(
            "Validation forecast (using data)", 
            paste0("Naive forecast (no data) * ", prop)
        )
    )

p1

# Boxplot
p2 <- ggplot(summary, aes(x = as.character(ptime), y = MAPE, fill = type)) +
    geom_boxplot() + theme +
    ylab("MAPE (%)\n") + xlab("\nPrediction duration") +
    scale_fill_manual(
        values = c('#999999','#E69F00'),
        name = "Type"
    )

p2
