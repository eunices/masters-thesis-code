# Run posterior simulation to check model fit

# Set up
source('2019-06-19-jsa-type-ch2/init/init_a.r')


# Parameters
set.seed(420) # for reproducibility


# Script

print(paste0(Sys.time(), " --- Posterior simulation for model fit"))

# Load data
files <- dir(
    dir_model_folder, 
    pattern = 'count_info.data.R',
    full.names = TRUE
)

data <- read_rdump(files)

# Load zero inflated fits
load(paste0(dir_model_folder, "fit.data"))

zips <- fit; rm(fit)


# Diagnostic plots

# launch_shinystan(zips)

# Save traceplot TODO: still does not work

png(
    paste0(dir_model_folder, 'traceplot.png'), 
    width = 24, height = 16, 
    units = "cm", res = 150
)

traceplot(zips, pars=names(zips))

dev.off() 


# Save fit
write.csv(
    summary(zips)$summary, 
    paste0(dir_model_folder, 'fit.csv'), 
    fileEncoding = 'UTF-8'
)


# Posterior simulations
allsim <- mclapply(1:1000, function(ii) {
    posterior_sim(data = data, model = zips)
})


# Save posterior simulations
save(allsim, file=paste0(dir_model_folder, "post.data"))


# Clear memory
rm(allsim, zips, data)


