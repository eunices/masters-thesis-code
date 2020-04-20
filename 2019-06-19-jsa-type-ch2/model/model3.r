# run posterior simulation to check model fit
source('2019-06-19-jsa-type-ch2/init/init_a.r')
print(paste0(Sys.time(), " --- posterior simulation for model fit"))

set.seed(420) # for reproducibility

# load data
files <- dir(dir_model_folder, pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)

# load zero inflated fits
load(paste0(dir_model_folder, "fit.data"))
zips <- fit; rm(fit)

# diagnostic plots
# launch_shinystan(zips)

# save traceplot
# TODO: still doesn't work
png(paste0(dir_model_folder, 'traceplot.png'), width=24, height=16, units="in", res=150)
traceplot(zips, pars=names(zips))
dev.off() 

# save fit
write.csv(summary(zips)$summary, paste0(dir_model_folder, 'fit.csv'), fileEncoding='UTF-8')

# posterior simulations
allsim <- mclapply(1:1000, function(ii) {
    posterior.sim(data = data, model = zips)
})

# save posterior simulations
save(allsim, file=paste0(dir_model_folder, "post.data"))
rm(allsim, zips, data)
