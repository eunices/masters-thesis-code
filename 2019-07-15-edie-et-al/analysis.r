print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO ANALYSES SCRIPTS FOR BEE TYPE DATA (EDIE ET AL)")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# If running script from elsewhere
# setwd("C:/_dev/msc/thesis/")
# source(paste0("2019-07-15-edie-et-al/", "analysis.r"))

# Init
#############
source('2019-07-15-edie-et-al/init_a.r')
source('2019-07-15-edie-et-al/analysis_loops_params.r')

# Model parameters
#############
analysis_edie_loop_type <- "string" # string or params

# For analysis_edie_loop_type == "params"
chosen_speeds <- c('fast') 
chosen_indices <- c(3, 6)   # 6 options

# For analysis_edie_loop_type == "string"
chosen_params <- c("BNN-C4-I8000-A0.9-T12",
                   "BNN-C4-I8000-A0.95-T12",
                   "BNN-C4-I8000-A0.99-T12",
                   "BNN-C4-I12000-A0.99-T12",
                   "BNN-C4-I300000-A0.99-T15")
# chosen_params <- c("BGN-C4-I100000-A0.99-T12",
#                    "BMY-C4-I100000-A0.8-T12",
#                    "BGN-C4-I300000-A0.99-T12",
#                    "BMY-C4-I300000-A0.8-T12")

# model_params        <dataset><ll>-C<chains>-I<iter>-A<ad>-T<td>
#     dataset         # BG = biogeographic realms,  GL = global, BM = biomes, LT = latitude-trop/not
#     ll              # whether using lat lon data (Y) or global.distribution data (N)
#     chains          # stan's number of chains
#     iter            # stan's number of iterations
#     ad              # stan's adapt_delta
#     td              # stan's max tree depth
# note: GL and BM always Y; the rest can be either Y or N

print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

combination_list <- list()
if(analysis_edie_loop_type=="params") {

    speed_len <- length(chosen_speeds); param_len <- length(chosen_indices)
    print(paste0(Sys.time(), " --- Start modelling loop for ", speed_len, " x ", 
            param_len, " = ", speed_len*param_len, " combinations."))
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

    for (j in 1:speed_len) {
        for (i in 1:param_len) {
            chosen_index <- chosen_indices[i]; chosen_speed <- chosen_speeds[j]
            idx <- j*i
            combination_list[[idx]] <- model_params_combinations(chosen_speed)[[chosen_index]]
        }
    }

} else if (analysis_edie_loop_type=="string") {
    len_params <- length(chosen_params)
    print(paste0(Sys.time(), " --- Start modelling loop for ", len_params, " parameter strings."))

    for (i in 1:len_params) {
        combination_list[[i]] <- parse_model_identifier(chosen_params[i])
    }

}

for (i in 1:length(combination_list)) {

        model_params <- combination_list[[i]]

        # Initialize identifier
        model_identifier <- paste0(
            model_params$dataset, model_params$ll, "-",
            "C", as.character(model_params$chains), "-",
            "I", as.character(model_params$iter), "-",
            "A", as.character(model_params$ad), "-",
            "T", as.character(model_params$td))
        dir_model_folder <- paste0(dir_analysis_edie_tmp, "/", model_identifier, "/")
        dir.create(dir_model_folder); dir.create(file.path(dir_model_folder, 'output'))
        filepath_log <- paste0(dir_model_folder, "/model.log"); if (!file.exists(filepath_log)) file.create(filepath_log)

        tryCatch({
            # print("Model params:"); print(model_params)
            # Analysis scripts
            source(paste0(dir_script_ed, 'analysis0.r')) # data prep
            source(paste0(dir_script_ed, 'analysis1.r')) # data prep
            source(paste0(dir_script_ed, 'analysis2.r')) # model fitting
            source(paste0(dir_script_ed, 'analysis3.r')) # post
            source(paste0(dir_script_ed, 'analysis4.r')) # forecast
            source(paste0(dir_script_ed, 'analysis5.r')) # plot
        }, 
        # warning=function(w) {write(toString(w), filepath_log, append=TRUE)},
        error=function(e) {print(paste0("ERROR: ", conditionMessage(e)))})
}

