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
            chains=as.numeric(model_params$chains),
            warmup=as.numeric(model_params$iter)*0.1, # 10% of iterations
            iter=as.numeric(model_params$iter),
            init=0,
            thin=5,
            cores=4,
            verbose=TRUE,
            seed=301,
            control = list(max_treedepth = as.numeric(model_params$td),
                           adapt_delta = as.numeric(model_params$ad)))
save(fit, file=paste0(dir_model_folder, "fit.data"))
stop <- Sys.time()

# Write to log file
model_li_str <- "model_params <- list("
len <- length(names(model_params))
for (i in 1:len) {
    name <- names(model_params)[i]
    model_li_str <- paste0(model_li_str, name, " = ", "'", model_params[name], "'")
    if (i < len) model_li_str <- paste0(model_li_str, ", ")
    else model_li_str <- paste0(model_li_str, ")")
}

conn <- file(filepath_log)
writeLines(paste0("Model identifier:", model_identifier), conn)
writeLines(paste0("Model started at:", start), conn)
writeLines(paste0("Model stopped at:", stop), conn)
writeLines(paste0("Model time elapsed:", stop-start), conn)
writeLines(paste0("Model params:", model_li_str), conn)
close(conn)

# system('shutdown -s')
