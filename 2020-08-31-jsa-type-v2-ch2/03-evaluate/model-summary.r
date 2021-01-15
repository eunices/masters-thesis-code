# This script creates diagnostics for models that have already been run through 
# model.r.

# Resources:

# General diagnostics: 
# https://betanalpha.github.io/assets/case_studies/rstan_workflow.html
# Summary: diagnostic code

# E-BMFI:
# https://betanalpha.github.io/assets/case_studies/pystan_workflow.html

# Strategies for divergences: 
# https://dev.to/martinmodrak/taming-divergences-in-stan-models-5762
# Summary: change priors

# Identify divergent transitions 
# https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# Summary: increase adapt delta or reparametrize (fit hierachical group 
# parameter); example of reparametrizing; 
# false negatives may be involved if divergent iterations are 
# spreaded out (reduce adapt_delta); 

#  Reparametrize to improve models for divergences
# https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html
# Summary: "This reparameterization is helpful when there is not much data, 
# because it separates the hierarchical parameters and lower-level 
# parameters in the prior."
# Reparametrize for Cauchy, Student t-distribution, 
# "Hierarchical Models and the Non-Centered Parameterization", 

# TODO: Posterior predictive checks
# https://mc-stan.org/docs/2_25/stan-users-guide/simulating-from-the-posterior-predictive-distribution.html

source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')

#### Get models

models <- list.dirs(dir_analysis_edie_model, recursive = F, full.names = F)
models <- models[!grepl("_|template|sanity-check", models)]

# OR predefine here
models <- "HAL-E0-C4-I20000-A0.999-T12-F25-V0" # e.g. bad model

##### Diagnostics

# HIGH LEVEL DIAGNOSTICS -------------------------------------------------------

# can either run for ALL models in `models` or just choose 1 model to run
for (i in 1:length(models)) {
    chosen_model <- models[i]

    dir_model_folder <- paste0(dir_analysis_edie_model, chosen_model, "/")
    model_params <- parse_model_identifier(chosen_model)
    
    print(paste0("Checking for ", i , " -- ", chosen_model))

    ##### Load data

    # Original data
    data_raw <- read.csv(paste0(dir_model_folder, "data.csv"), na.strings=c("")) 

    # R data
    data <- read_rdump(paste0(dir_model_folder, "count_info.data.R")) # as "data"

    # Model
    load(paste0(dir_model_folder, "fit.data"))                        # as "fit"


    ##### Diagnostics

    check_all_diagnostics(fit)
    print("-------------------------------------------------------------------")
    print("-------------------------------------------------------------------")
    print("-------------------------------------------------------------------")
}

# SPECIFIC DIAGNOSTICS FOR DIVERGENT ITERATIONS --------------------------------

# Param names
params_n <- names(fit)[!grepl("log_lik|lp", names(fit))]

# Using shinystan
# launch_shinystan(fit)

# Prepare data
params <- as.data.frame(rstan::extract(fit, permuted=FALSE))
params <- data.table(params)
params$div <- get_sampler_params(fit, inc_warmup=FALSE)[[1]][,'divergent__']
params$iter <- 1:dim(params)[1]

# Plot 1 parameter
plot(
    params$iter, params$`chain:1.sigma_phi`, col = c_dark, pch=16, cex=0.8,
    xlab="Iteration", ylab="log(phi)"
) 

# Plot running means
params$running_means <- sapply(
    params$iter, 
    function(n) mean(log(params$`chain:1.sigma_phi`)[1:n])
)

plot(
    params$iter, params$running_means, col=c_dark, pch=16, cex=0.8,
    xlab="Iteration", ylab="MCMC mean of log(phi)"
)

# Plot divergent iterations
plot(
    params[div == 1]$iter, params[div == 1]$`chain:1.sigma_phi`,
    col=c_dark, pch=16, cex=0.8, xlab="Iterations", ylab="sigma_phi",
)

points(
    params[div == 0,]$iter, params[div == 0,]$`chain:1.sigma_phi`,
    col="red", pch=16, cex=0.8
)

# Plot divergent iterations using pairs
l <- length(params_n); n <- 1; increase <- 5
pairs(fit, pars=params_n[n:(n+increase-1)]); n <- ifelse(n+increase>=l, l, n+increase)





##### Forecasting 

# Allows forecasting for custom timeframes, 
# even though data is trained only till 2019 

# Load forecast predictions
load(paste0(dir_model_folder, "forecast.data")) # as "forecast"
# Forecast data is was done up to a certain duration only! 

# Convert data to df
obs <- convert_data_to_df(data)

# Get final count
last_observed_count <- data.table(obs)[
    , list(cml_value = max(cml_value)), by = "group"
] 

# Convert forecast to df 
forsim <- convert_forecast_to_df(data, data_raw, forecast)
forsim <- data.table(forsim)

# subset for the years required
current_year <- 2020
n_years <- 5
years <- (current_year+1):(current_year + n_years) # not including current

results_forecast <- forsim[index %in% years, 
    list(
        value = max(value), # highest value
        index = max(index)  # for the last year
    ), 
    by = c("group", "sim")][,
            list(
                fore_mu = round(mean(value), 0),
                fore_lower = round(quantile(value, 0.1), 0) ,
                fore_upper = round(quantile(value, 0.9), 0)
            ),
            by = "group"
    ]

filename <- paste0(
    "output/output_custom-prediction-", 
    min(years), "-", max(years), 
    "-trained-till-", max(data_raw$year),
    ".csv"
)

output <- paste0(dir_model_folder, filename)

write.csv(results_forecast, file = output, row.names = FALSE)
