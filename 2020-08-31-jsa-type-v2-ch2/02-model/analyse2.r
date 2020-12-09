# Run posterior forecasts to make predictions


# Set up
source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')



# Parameters
set.seed(2020) # for reproducibility
ftime <- model_params$fc



# Script


# Load data
files <- dir(
    dir_model_folder, 
    pattern = 'count_info.data.R',
    full.names = TRUE
)

data <- read_rdump(files)

# Load model
load(paste0(dir_model_folder, "fit.data"))
zips <- fit
rm(fit)




print(paste0(Sys.time(), " --- Making posterior simulation"))

# Posterior simulations
allsim <- mclapply(1:1000, mc.cores = 1, function(ii) {
    posterior_sim(data = data, model = zips)
})

# Save posterior simulations
save(allsim, file = paste0(dir_model_folder, "post.data"))

# Clear memory
rm(allsim)



print(paste0(Sys.time(), " --- Making posterior forecasts"))

# Simulate the forecast
forecast <- mclapply(1:1000, mc.cores = 1, function(ii) {
	posterior_forecast(data = data, ftime = ftime, model = zips)	 
})

# Save forecast
save(forecast, file = paste0(dir_model_folder, "forecast.data"))

# Clean memory
rm(data, zips, forecast)



