source('2019-07-15-edie-et-al/init_a.r')

# fit model to data
library(rstan)

output_filepath <- paste0(dir_analysis_edie_tmp, "poutput.txt")
s <- file(output_filepath, open="wt")
sink(s, type=c("output", "message"))

start <- proc.time()

# initial data
files <- dir(dir_analysis_edie_tmp, pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)


# fit model
fit <- stan( file="2019-07-15-edie-et-al/zip_count.stan",
                   data=data, 
                   chains=4, 
                   warmup=2500,
                   iter=5000,
                   init=0,
                   thin=5,
                   cores=4,
                   verbose=TRUE, seed=301, 
                   control = list(max_treedepth = 15, 
                                  adapt_delta=0.9))
save(fit, file=paste0(dir_analysis_edie_tmp, "fit.data"))
print(proc.time()-start)

sink(type="message")
close(s)

# system('shutdown -s')

