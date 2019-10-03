# fit model to data
library(rstan)

# initial data
files <- dir('tmp/', pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)


# fit model

fit <- stan( file="2019-07-15-edie-et-al/zip_count.stan",
                   data=data, 
                   chains=10, 
                   warmup=2500,
                   iter=20000,
                   init=0,
                   thin=5,
                   cores=4,
                   verbose=TRUE)
save(fit, file="data/dump/fit.data")
