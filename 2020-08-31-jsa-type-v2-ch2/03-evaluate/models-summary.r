
source('2020-08-31-jsa-type-v2-ch2/03-evaluate/init.r')

models <- list.dirs(dir_analysis_edie_model, recursive = F, full.names = F)
models <- models[!grepl("_|template|sanity-check", models)]

# Test
# for (i in length(models)) {
#     tryCatch(
#         {
#             get_sampling_info(models[i])
#             get_duration_info(models[i])
#         },
#         error = function(e) {
#             print(i)
#         }
#     )
# }

# chosen_model <- models[1]
# get_sampling_info(chosen_model)

summary <- data.frame(cbind(
    models, 
    convergence = unlist(lapply(models, get_sampling_info)),
    duration = unlist(lapply(models, get_duration_info))
))


file <- paste0(dir_analysis_edie_model, "model-summaries.csv")
write.csv(summary, file, row.names = FALSE)
