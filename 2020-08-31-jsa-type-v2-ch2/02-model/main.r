print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO ANALYSES SCRIPTS FOR BEE TYPE DATA (EDIE ET AL)")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# Init
#############
source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')
source('2020-08-31-jsa-type-v2-ch2/params-02-model.r')


print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@ SCRIPTS ")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

# Create list of model params
len_params <- length(model_folder_names)
model_param_list <- lapply(model_folder_names, 
    function(x) parse_model_identifier(x)
)

analysis <- function(run = TRUE) {
    if(run) {

        source(paste0(dir_script_ed, '02-model/model.r'))    # model fitting

        source(paste0(dir_script_ed, '02-model/analyse1.r')) # posterior
        source(paste0(dir_script_ed, '02-model/analyse2.r')) # forecast
        source(paste0(dir_script_ed, '02-model/analyse3.r')) # visualise

    }
}

# Loop through list
print(paste0(
    Sys.time(), " --- Start modelling loop for ", len_params, " parameters."
))

for (i in 1:len_params) {

    model_params <- model_param_list[[i]]

    # Initialize identifier
    filepaths <- initialize_model_params(model_params)

    dir_model_folder <- filepaths[1]
    filepath_log <- filepaths[2]
    warnings_log <- filepaths[3]
    model_identifier <- filepaths[4]

    print("#######################################################")
    print(paste0("Starting for >>>>>>>>>> ", model_identifier))
    print("#######################################################")

    # Analysis scripts
    tryCatch(
        withCallingHandlers(analysis(), 
                            warning = function(w) {
                                write_to_log(w, warnings_log)
                            }),
        error = function(e) {print(paste0("ERROR: ", conditionMessage(e)))}
    ) # Solution from: https://stackoverflow.com/questions/37836392/ 
}

