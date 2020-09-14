print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO DATA PREP SCRIPT FOR BEE TYPE DATA (EDIE ET AL)")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
# "prep.r" should be run before "model.r"
# Note: this step may be omitted if data is already prepared in the desired format.

# Init
#############
# Prepare data from original dataset
source('2019-06-19-jsa-type-ch2/init/init_a.r')
source('2019-06-19-jsa-type-ch2/params.r')


print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@ SCRIPTS ")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

print(paste0(Sys.time(), " --- prepare data"))

# Create list of model params
len_params <- length(model_folder_names)
model_param_list <- lapply(model_folder_names, function(x) parse_model_identifier(x))

# Create datasets
for (i in 1:len_params) {

    # Get model params
    model_params <- model_param_list[[i]]

    # Initialize identifier
    filepaths <- initialize_model_params(model_params)

    dir_model_folder <- filepaths[1]
    filepath_log <- filepaths[2]
    warnings_log <- filepaths[3]
    model_identifier <- filepaths[4]

    # Run data prep script
    source(paste0(dir_script_ed, 'prep/prep1.r')) # formatting data
}
