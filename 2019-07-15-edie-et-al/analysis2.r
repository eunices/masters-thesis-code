# fit model to data
library(rstan)

start <- proc.time()

# initial data
files <- dir('tmp/', pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)


# fit model

fit <- stan( file="2019-07-15-edie-et-al/zip_count.stan",
                   data=data, 
                   chains=6, 
                   warmup=2500,
                   iter=10000,
                   init=0,
                   thin=5,
                   cores=4,
                   verbose=TRUE, seed=301,
                   control = list(max_treedepth = 15))
save(fit, file="tmp/dump/fit.data")
print(proc.time()-start)
