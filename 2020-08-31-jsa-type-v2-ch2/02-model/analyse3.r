# Setup
source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')


# Script
print(paste0(Sys.time(), " --- Visualising results"))


####### Load data

# Original data
data_raw <- read.csv(paste0(dir_model_folder, "data.csv"), na.strings=c("")) 

# R data
data <- read_rdump(paste0(dir_model_folder, "count_info.data.R")) 

# Load zero inflated fits
load(paste0(dir_model_folder, "fit.data"))      # as "fit"

# Load posterior simulation
load(paste0(dir_model_folder, "post.data"))     # as "allsim"

# Load forecast predictions
load(paste0(dir_model_folder, "forecast.data")) # as "forecast"

# Map model indices to original variables
mapping <- unique(data.frame(
    groupname = as.character(data_raw$group),
    group = as.numeric(data_raw$group)
))


####### Check for chain convergence
log_chain_sampling(fit, dir_model_folder)


#######  Get simulations and actual data
li_df <- summarize_simulations_observed(data, allsim)

Z <- li_df$Z                      # Counts for each year for sim and actual, df
sum_y <- li_df$sum_y              # Cumulative counts for simulated data, dt
obs_count <- li_df$obs_count      # Cumulative counts for observed data, dt


####### Get parameter (delta)
li_df_delta <- extract_delta(fit)

group_cf1 <- li_df_delta$group_cf1   # Intercept of delta coefficient
group_cf2 <- li_df_delta$group_cf2   # Slope of delta (beta) coefficient
cf2 <- li_df_delta$cf2               # Summarized slope of delta (beta) & 80 CI


####### Create results table - part 1
results <- combine_results(sum_y, obs_count, cf2, mapping)


####### Save plots

# Set up facet labels for **all** plots
labels <- as.character(mapping$groupname)
names(labels) <- mapping$group

# Prepare data for plot 1, 2
li_df_plot1_2 <- prepare_plot1_2_data(Z) # Z as df

obs <- li_df_plot1_2$obs   # Observations with count/ cumulative, df
sims <- li_df_plot1_2$sims # Simulations with count/ cumulative, df

# Plot 1 - cumulative_fit.pdf / cumulative counts
save_plot1(sims, obs, labels, dir_model_folder)

# Plot 2 - count_fit.pdf / counts
save_plot2(sims, obs, labels, dir_model_folder)

# Prepare data for plot 3
li_df_plot3 <- prepare_plot3_data(data, data_raw, group_cf1, group_cf2)

sims <- li_df_plot3$sims         # Simulations for omega, df
om_mean <- li_df_plot3$om_mean   # Mean based on omega, dt

# Plot 3 - regression.pdf / counts with regression line
save_plot3(obs, sims, om_mean, labels, dir_model_folder)
# note: obs is output from prepare_plot1_2_data


####### Create results table - part 2 (add on forecasts)

# Summarize forecast results
forecast_results <- summarize_forecasts(data, data_raw, forecast, obs)

# Output csv table
output_results_csv(results, forecast_results, dir_model_folder)

