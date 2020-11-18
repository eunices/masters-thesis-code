# Compare LOOAIC for different groups

source('2020-08-31-jsa-type-v2-ch2/00-init/init-e.r')

chosen_models <- c(
    "BMY-E2-C4-I8000-A0.8-T12-F25-V0",
    "BGY-E2-C4-I8000-A0.8-T12-F25-V0",
    "FAM-E2-C4-I8000-A0.8-T12-F25-V0"
)

model_param_list <- lapply(
    chosen_models, 
    function(x) parse_model_identifier(x)
)

# get predictions for each


# calculate LOOAIC


# plot LOOAIC for each model



