# compare models
# adapted from https://betanalpha.github.io/assets/case_studies/rstan_workflow.html

source('2019-07-15-edie-et-al/init_a.r')

chosen_model <- "GLY-E0-C4-I8000-A0.8-T12"

model_params <- parse_model_identifier(chosen_model)
dir_model_folder <- initialize_model_params(model_params)[1]

load(paste0(dir_model_folder, "fit.data"))       # as "fit"

check_all_diagnostics(fit)

c_dark <- c("#8F272780")
green <- c("#00FF0080")

partition <- partition_div(fit)
div_params <- partition[[1]]
nondiv_params <- partition[[2]]

# TODO: write function for different params
# to diagnose between pairs
# par(mar = c(4, 4, 0.5, 0.5))
# plot(nondiv_params$`theta[1]`, log(nondiv_params$tau),
#      col=c_dark, pch=16, cex=0.8, xlab="theta[1]", ylab="log(tau)",
#      xlim=c(-20, 50), ylim=c(-1,4))
# points(div_params$`theta[1]`, log(div_params$tau),
#        col=green, pch=16, cex=0.8)

launch_shinystan(fit)

# reparametrize
# https://mc-stan.org/docs/2_18/stan-users-guide/reparameterization-section.html