print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO ANALYSES SCRIPTS FOR BEE TYPE DATA (EDIE ET AL)")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# If running script from elsewhere
# setwd("C:/_dev/msc/thesis/")
# source(paste0("2019-07-15-edie-et-al/", "analysis.r"))

memory.limit(size=12000)
# memory.limit(size=25000)

# Init
#############
source('2019-07-15-edie-et-al/init_a.r')
source('2019-07-15-edie-et-al/analysis_loops_params.r')

# Model parameters
#############
analysis_edie_loop_type <- "string" # string or params

# For analysis_edie_loop_type == "params"
chosen_speeds <- c('fast')  # print(names(speeds))
chosen_indices <- c(3, 6)   # print(combinations)
chosen_efforts <- c(0, 1)   # either 0 (no taxonomic effort), 1 (pub taxonomic effort)

# For analysis_edie_loop_type == "string"
# chosen_params <- c("FAM-E0-C4-I20000-A0.8-T12") # fast run
chosen_params <- c("BMY-E0-C4-I20000-A0.99-T12",
                   "BNN-E0-C4-I20000-A0.99-T12",
                   "FAM-E0-C4-I20000-A0.99-T12",
                   
                   "BMY-E0-C4-I300000-A0.99-T15",
                   "BNN-E0-C4-I300000-A0.99-T15",
                   "FAM-E0-C4-I300000-A0.99-T15",
                   "FAM-E1-C4-I300000-A0.99-T15")
# chosen_params <- c("BGY-E0-C4-I8000-A0.8-T12",
#                    "BMY-E0-C4-I8000-A0.8-T12",
#                    "BNN-E0-C4-I8000-A0.8-T12",

#                    "BGY-E0-C4-I8000-A0.95-T12",
#                    "BMY-E0-C4-I8000-A0.95-T12",
#                    "BNN-E0-C4-I8000-A0.95-T12",

#                    "BMY-E0-C4-I20000-A0.8-T12",

#                    "BGY-E0-C4-I100000-A0.8-T12",
#                    "BMY-E0-C4-I100000-A0.8-T12",
#                    "BNN-E0-C4-I100000-A0.8-T12",

#                    "BGY-E0-C4-I300000-A0.99-T15",
#                    "BMY-E0-C4-I300000-A0.99-T15",
#                    "BNN-E0-C4-I300000-A0.99-T15",
                   
#                    "BGY-E0-C4-I300000-A0.999-T15")
# chosen_params <- c("BMY-E0-C4-I8000-A0.8-T12",
#                    "BMY-E0-C4-I8000-A0.95-T12",
#                    "BMY-E0-C4-I20000-A0.8-T12",
#                    "BMY-E0-C4-I100000-A0.8-T12",

#                    "BGY-E0-C4-I300000-A0.99-T15",
#                    "BMY-E0-C4-I300000-A0.99-T15",
#                    "BNN-E0-C4-I300000-A0.99-T15",

#                    "BGY-E0-C4-I300000-A0.999-T15")


# model_params        <dataset><ll>-E<te>-C<chains>-I<iter>-A<ad>-T<td>
#     dataset         # BG = biogeographic realms,  GL = global, BM = biomes, LT = latitude-trop/not
#     te              # taxonomic effort 0=no taxonomic effort, 1=publication taxonomic effort
#     ll              # whether using lat lon data (Y) or global.distribution data (N)
#     chains          # stan's number of chains
#     iter            # stan's number of iterations
#     ad              # stan's adapt_delta
#     td              # stan's max tree depth
# note: GL and BM always Y; the rest can be either Y or N

print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

# Create list of model params
len_params <- ifelse(analysis_edie_loop_type=="params", 
                     length(chosen_speeds)*length(chosen_indices)*length(chosen_efforts),
                     length(chosen_params))
print(paste0(Sys.time(), " --- Start modelling loop for ", len_params, " parameters."))

if(analysis_edie_loop_type=="params") {
    model_param_list <- create_model_params_combi(chosen_speeds, chosen_indices, chosen_efforts)
} else if (analysis_edie_loop_type=="string") {
    model_param_list <- list()
    for (i in 1:len_params) {
        model_param_list[[i]] <- parse_model_identifier(chosen_params[i])
    }
}


# Loop through list
for (i in 1:length(model_param_list)) {

    model_params <- model_param_list[[i]]

    # Initialize identifier
    model_identifier <- paste0(
        model_params$dataset, model_params$ll, "-",
        "E", as.character(model_params$te), "-",
        "C", as.character(model_params$chains), "-",
        "I", as.character(model_params$iter), "-",
        "A", as.character(model_params$ad), "-",
        "T", as.character(model_params$td))
    dir_model_folder <- paste0(dir_analysis_edie_tmp, "/", model_identifier, "/")
    dir.create(dir_model_folder); dir.create(file.path(dir_model_folder, 'output'))
    filepath_log <- paste0(dir_model_folder, "/model.log"); if (!file.exists(filepath_log)) file.create(filepath_log)
    warnings_log <- paste0(dir_model_folder, "/warnings.log"); if (!file.exists(warnings_log)) file.create(warnings_log)

    # Analysis scripts
    analysis <- function() {
        source(paste0(dir_script_ed, 'analysis0.r')) # data prep
        source(paste0(dir_script_ed, 'analysis1.r')) # data prep
        source(paste0(dir_script_ed, 'analysis2.r')) # model fitting
        source(paste0(dir_script_ed, 'analysis3.r')) # post
        source(paste0(dir_script_ed, 'analysis4.r')) # forecast
        source(paste0(dir_script_ed, 'analysis5.r')) # plot
    }

    tryCatch(
        withCallingHandlers(analysis(), warning = function(w) {write_to_log(w, warnings_log)}),
        error = function(e) {print(paste0("ERROR: ", conditionMessage(e)))}
    ) # Solution from: https://stackoverflow.com/questions/37836392/ 
}

