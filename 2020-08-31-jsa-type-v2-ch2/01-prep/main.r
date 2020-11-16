print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO DATA PREP SCRIPT FOR BEE TYPE DATA (EDIE ET AL)")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# Initialize
#############

# Prepare data from original dataset
source('2020-08-31-jsa-type-v2-ch2/00-init/init-a.r')
source('2020-08-31-jsa-type-v2-ch2/params-01-prep.r')

print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
print("@@@@@@@@@ SCRIPTS ")
print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

print(paste0(Sys.time(), " --- prepare data"))

# Create list of model params
len_params <- length(model_folder_names)

model_param_list <- lapply(
    model_folder_names, 
    function(x) parse_model_identifier(x)
)

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
    source(paste0(dir_script_ed, '01-prep/prep1.r')) # prepare raw data
    source(paste0(dir_script_ed, '01-prep/prep2.r')) # prepare offset data
    source(paste0(dir_script_ed, '01-prep/prep3.r')) # prepare data for stan
}
