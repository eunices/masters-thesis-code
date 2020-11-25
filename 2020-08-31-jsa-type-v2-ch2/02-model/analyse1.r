# Save parameters

# Set up
source('2020-08-31-jsa-type-v2-ch2/00-init/init-a.r')


# Parameters
set.seed(2020) # for reproducibility


# Script

print(paste0(Sys.time(), " --- Save parameters"))

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

traceplot(zips, pars = names(zips))

dev.off() 


# Save fit
write.csv(
    summary(zips)$summary, 
    paste0(dir_model_folder, 'fit.csv'), 
    fileEncoding = 'UTF-8'
)



