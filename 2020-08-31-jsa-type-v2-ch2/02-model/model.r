# Running model
source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')
print(paste0(Sys.time(), " --- running stan model"))

#  Write to logfile
start <- Sys.time()
model_li_str <- "model_params <- list("
len <- length(names(model_params))
for (i in 1:len) {
    name <- names(model_params)[i]
    param_name <- model_params[name]

    if (i %in% c(1, 2)) {
        model_li_str <- paste0(model_li_str, name, " = ", "'", param_name, "'")
    } else {
        model_li_str <- paste0(model_li_str, name, " = ", param_name)
    }

    if (i < len) model_li_str <- paste0(model_li_str, ", ")
    else model_li_str <- paste0(model_li_str, ")")
}

conn <- file(filepath_log, "a")
write(paste0("Model identifier: ", model_identifier), conn, sep="\n", append=T)
write(paste0("Model params: ", model_li_str), conn, sep="\n")
write(paste0("Model started at: ", start), conn, sep="\n", append=T)
close(conn)

# Initial data
files <- dir(dir_model_folder, pattern = 'count_info.data.R', full.names = TRUE)
data <- read_rdump(files)

# Fit model
fit <- stan(

    file = paste0(dir_script_ed, "02-model/zip_count.stan"),
    data = data,

    chains = as.numeric(model_params$chains),
    warmup = round(as.numeric(model_params$iter)*0.3, 0), # 30%
    iter = as.numeric(model_params$iter),

    init = 0, thin = 5, cores = 4, verbose = TRUE, seed = 2020,
    
    control = list(
        max_treedepth = as.numeric(model_params$td),
        adapt_delta = as.numeric(model_params$ad)
    )
)

save(fit, file = paste0(dir_model_folder, "fit.data"))
rm(fit)
stop <- Sys.time()

# Write to log file
conn <- file(filepath_log, "a")
write(paste0("Model stopped at: ", stop), conn, sep="\n")
write(paste0("Model time elapsed: ", stop-start), conn, sep="\n")
close(conn)
