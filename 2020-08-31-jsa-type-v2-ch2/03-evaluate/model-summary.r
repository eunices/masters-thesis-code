# This script creates diagnostics for models that have already been run through 
# model.r.

# Resources:

# https://betanalpha.github.io/assets/case_studies/rstan_workflow.html
# Summary: diagnostic code

# E-BMFI:
# https://betanalpha.github.io/assets/case_studies/pystan_workflow.html

# Strategies for divergences: 
# https://dev.to/martinmodrak/taming-divergences-in-stan-models-5762
# Summary: change priors

# TODO: Check divergent transitions 
# https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# Summary: 

# TODO: Reparametrize to improve models for divergences
# https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html
# https://betanalpha.github.io/assets/case_studies/divergences_and_bias.html


source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')

#### Get models

models <- list.dirs(dir_analysis_edie_model, recursive = F, full.names = F)
models <- models[!grepl("_|template|sanity-check", models)]

##### Diagnostics

# HIGH LEVEL DIAGNOSTICS -------------------------------------------------------

# models <- "HAL-E0-C4-I20000-A0.999-T12-F25-V0" # e.g. bad model

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

# Using shinystan
# launch_shinystan(fit)

# Using rstan
params <- names(fit)
params <- params[!grepl("log_lik|lp", params)]

length(params); n <- 1
pairs(fit, pars=params[n:(n+9)]); n <- n+10
pairs(fit, pars=params[n:(n+9)]); n <- n+10
pairs(fit, pars=params[n:(n+9)]); n <- n+10
pairs(fit, pars=params[n:(n+9)]); n <- n+10



##### Forecasting 
obs <- convert_data_to_df(data)
last_observed_count <- data.table(obs)[
    , list(cml_value = max(cml_value)), by = "group"
] 

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
