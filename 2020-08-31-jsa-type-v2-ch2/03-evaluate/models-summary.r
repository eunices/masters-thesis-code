

source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')

models <- list.dirs(dir_analysis_edie_model, recursive = FALSE, full.names = FALSE)
models <- models[!grepl("_", models)]

summary <- data.frame(cbind(
    models,
    convergence = unlist(lapply(chosen_models, get_sampling_info))
))

file <- paste0(dir_analysis_edie_model, "model-summaries.csv")
write.csv(summary, file, row.names = FALSE)
