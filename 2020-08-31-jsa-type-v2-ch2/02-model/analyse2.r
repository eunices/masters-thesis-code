# Run posterior forecasts to make predictions


# Set up
source('2020-08-31-jsa-type-v2-ch2/00-init/init-a.r')


# Parameters
set.seed(2020) # for reproducibility
ftime <- model_params$fc


# Script

print(paste0(Sys.time(), " --- making forecasts"))

# Load data
files <- dir(
    dir_model_folder, 
    pattern = 'count_info.data.R',
    full.names = TRUE
)

data <- read_rdump(files)

# Load zero inflated fits
load(paste0(dir_model_folder, "fit.data"))
zips <- fit
rm(fit)

# Simulate the forecast
forecast <- mclapply(1:1000, mc.cores = 1, function(ii) {
	posterior_forecast(data = data, ftime = ftime, model = zips)	 
})

# Save forecast
save(forecast, file = paste0(dir_model_folder, "forecast.data"))
rm(data, zips, forecast)



