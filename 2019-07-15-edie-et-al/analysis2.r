source('2019-07-15-edie-et-al/init_a.r')

# fit model to data
library(rstan)

start <- Sys.time()

# initial data
files <- dir(dir_model_folder, 
             pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)


# fit model
fit <- stan(file="2019-07-15-edie-et-al/zip_count.stan",
            data=data,
            chains=model_params$chains,
            warmup=model_params$iter*0.1, # 10% of iterations
            iter=model_params$iter,
            init=0,
            thin=5,
            cores=4,
            verbose=TRUE,
            seed=301,
            control = list(max_treedepth = model_params$td,
                           adapt_delta=model_params$ad))
save(fit, file=paste0(dir_model_folder, "fit.data"))
stop <- Sys.time()

# Write to log file
filepath_log <- paste0(dir_model_folder, "model.log")
file.create(filepath_log); conn <- file(filepath_log)
writeLines(paste0("Model identifier:", model_identifier), conn)
writeLines(paste0("Model started at:", start), conn)
writeLines(paste0("Model stopped at:", stop), conn)
writeLines(paste0("Model time elapsed:", stop-start), conn)
writeLines(paste0("Model params:\n"), conn)
writeLines(t(data.frame(model_params)), conn)
close(conn)

# system('shutdown -s')
