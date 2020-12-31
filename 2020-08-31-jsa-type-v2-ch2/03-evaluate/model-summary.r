# This script creates diagnostics for models that have already been run through model.r.
# adapted from https://betanalpha.github.io/assets/case_studies/rstan_workflow.html

source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')

##### Parse model info

chosen_model <- "BGY-E0-C4-I8000-A0.8-T12-F25-V0"
dir_model_folder <- paste0(dir_analysis_edie_model, chosen_model, "/")

##### Load data

# Original data
data_raw <- read.csv(paste0(dir_model_folder, "data.csv"), na.strings=c("")) 

# R data
data <- read_rdump(paste0(dir_model_folder, "count_info.data.R")) # as "data"

# Model
load(paste0(dir_model_folder, "fit.data"))                        # as "fit"


##### Diagnostics

check_all_diagnostics(fit)

# launch_shinystan(fit)

# TODO: Check divergent transitions 
# https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# https://betanalpha.github.io/assets/case_studies/rstan_workflow.html

# TODO: Reparametrize to improve models
# https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html
# https://betanalpha.github.io/assets/case_studies/divergences_and_bias.html



##### Summaries
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
