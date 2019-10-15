print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO ANALYSES SCRIPTS FOR BEE TYPE DATA (EDIE ET AL)")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# If running script from elsewhere
# setwd("C:/Dev/msc-thesis-code/")
# source(paste0(dir_script_ed, "analysis.r"))

# Init
#############
source('2019-07-15-edie-et-al/init_a.r')
source('2019-07-15-edie-et-al/analysis_loops_params.r')

# Model parameters
#############
analysis_edie_loops <- "Y"

# For loops if analysis_edie_loops == "Y"
# chosen_speeds <- c('slow1', 'slow3')
# chosen_indices <- c(2, 3)
# chosen_speeds <- c('fast')
# chosen_indices <- c(3)
chosen_speeds <- c('fast') 
chosen_indices <- c(3)   # 6 options



if (analysis_edie_loops == "N") {
    model_params <- parse_model_identifier("BGY-C4-I8000-A0.8-T12")
    # model_params <- list(
    #     dataset = "BM", # BG = biogeographic realms,  GL = global, BM = biomes, LT = latitude-trop/not
    #     ll = "Y",       # whether using lat lon data (Y) or global.distribution data (N)
    #     chains = 4,     # stan's number of chains
    #     iter = 5000,    # stan's number of iterations
    #     ad = 0.99,      # stan's adapt_delta
    #     td = 15         # stan's max tree depth
    # )   # note: GL and BM always Y; the rest can be either Y or N
    
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
    
    # Analysis scripts
    source(paste0(dir_script_ed, 'analysis0.r')) # data prep
    source(paste0(dir_script_ed, 'analysis1.r')) # data prep
    source(paste0(dir_script_ed, 'analysis2.r')) # model fitting
    source(paste0(dir_script_ed, 'analysis3.r')) # post
    source(paste0(dir_script_ed, 'analysis4.r')) # forecast
    source(paste0(dir_script_ed, 'analysis5.r')) # plot
    

} else if (analysis_edie_loops == "Y") {
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    speed_len <- length(chosen_speeds); param_len <- length(chosen_indices)
    print(paste0(Sys.time(), " --- Start modelling loop for ", speed_len, " x ", 
                 param_len, " = ", speed_len*param_len, " combinations."))
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    
    for (j in 1:speed_len) {
        for (i in 1:param_len) {
            chosen_index <- chosen_indices[i]; chosen_speed <- chosen_speeds[j]
            print("#############################################################")
            print("#############################################################")
            print("#############################################################")
            print(paste0(Sys.time(), " --- Modelling for chosen_index = ", chosen_index, 
                        "; speed = ", chosen_speed))
            model_params <- model_params_combinations(chosen_speed)[[chosen_index]]

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
            warning=function(w) {write(toString(w), filepath_log, append=TRUE)},
            error=function(e) {print(paste0("ERROR: ", conditionMessage(e)))})
        }
    }
} 
