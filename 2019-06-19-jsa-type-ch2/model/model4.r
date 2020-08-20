# run posterior forecasts to make predictions
source('2019-06-19-jsa-type-ch2/init/init_a.r')
print(paste0(Sys.time(), " --- making forecasts"))

set.seed(420) # for reproducibility

# parameters
ftime <- as.numeric(25)

# load data
files <- dir(dir_model_folder, pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)

# load zero inflated fits
load(paste0(dir_model_folder, "fit.data"))
zips <- fit; rm(fit)

# simulate the forecast
forecast <- mclapply(1:1000, mc.cores=1, function(ii) {
   posterior_forecast(data=data, ftime=ftime, model=zips) 
})

# save forecast
save(forecast, file=paste0(dir_model_folder, "forecast.data"))
rm(data, zips, forecast)