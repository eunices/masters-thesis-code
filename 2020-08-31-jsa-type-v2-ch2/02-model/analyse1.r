# Save parameters

# Set up
source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')

# Parameters
set.seed(2020) # for reproducibility

# Script
print(paste0(Sys.time(), " --- Save parameters"))

# Load data
files <- dir(dir_model_folder, pattern = 'count_info.data.R', full.names = TRUE)
data <- read_rdump(files)

# Load zero inflated fits
load(paste0(dir_model_folder, "fit.data"))
zips <- fit; rm(fit)

# Save traceplot TODO: still does not work
file <- paste0(dir_model_folder, 'traceplot.png')
png(file, width = 24, height = 16, units = "cm", res = 150)
traceplot(zips, pars = names(zips))
dev.off() 

# Save fit
file <- paste0(dir_model_folder, 'fit.csv')
write.csv(summary(zips)$summary, file, fileEncoding = 'UTF-8')

# Diagnostic plots
# launch_shinystan(zips)


