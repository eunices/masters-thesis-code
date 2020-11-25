# This script creates diagnostics for models that have already been run through model.r.
# adapted from https://betanalpha.github.io/assets/case_studies/rstan_workflow.html

source('2020-08-31-jsa-type-v2-ch2/00-init/init-a.r')

chosen_model <- "BGY-E0-C4-I8000-A0.8-T12-F25-V00"

model_params <- parse_model_identifier(chosen_model)
dir_model_folder <- initialize_model_params(model_params)[1]

load(paste0(dir_model_folder, "fit.data")) # as "fit"

check_all_diagnostics(fit)

launch_shinystan(fit)


# TODO: Check divergent transitions 
# https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html
# https://betanalpha.github.io/assets/case_studies/rstan_workflow.html

# TODO: Reparametrize to improve models
# https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html
# https://betanalpha.github.io/assets/case_studies/divergences_and_bias.html
