# Compare LOOAIC between different TE 

source('2020-08-31-jsa-type-v2-ch2/00-init/init-e.r')

chosen_models <- c(
    "BGY-E0-C4-I8000-A0.8-T12-F25-V0",
    "BGY-E1-C4-I8000-A0.8-T12-F25-V0",
    "BGY-E2-C4-I8000-A0.8-T12-F25-V0"
)


get_loo <- function(model) {

    model_params <- parse_model_identifier(model)
    model_dirs <- initialize_model_params(model_params)
    model_dir <- model_dirs[1]

    # load zero inflated fits
    load(paste0(model_dir, "fit.data"))       # as "fit"

    m_log_lik <- extract_log_lik(
        fit, 
        parameter_name = "log_lik", 
        merge_chains = FALSE
    )

    m_r_eff <- relative_eff(exp(m_log_lik), cores = 2)

    m_loo <- loo(m_log_lik, r_eff = m_r_eff, cores = 2)
    m_loo
}


model3 <- chosen_models[3]
model3_loo <- get_loo(model3)

model2 <- chosen_models[2]
model2_loo <- get_loo(model2)

# get predictions for each


# calculate LOOAIC


# plot LOOAIC for each model




# Resources

# TODO: http://mc-stan.org/loo/articles/loo2-with-rstan.html
# Simple example of LOOAIC
# TODO: https://datascienceplus.com/k-fold-cross-validation-in-stan/
# Alternative to LOOAIC
# TODO: https://discourse.mc-stan.org/t/calculating-log-like-for-waic-loo-with-user-defined-functions/2696/3
# How to calculate LOOAIC for custom functions
# TODO: https://fabiandablander.com/r/Law-of-Practice.html
# What's the purpose of LOOAIC




###############
# OLD SCRIPT
###############

# This script compares models that have already been run through model.r.

library(forecast)
library(ggplot2)

source('2020-08-31-jsa-type-v2-ch2/00-init/init-a.r')

chosen_models <- c("BGY-E2-C4-I8000-A0.8-T12-F10",
                   "BGY-E2-C4-I8000-A0.8-T12-F25")

model_param_list <- lapply(
    chosen_models, 
    function(x) parse_model_identifier(x)
)

model_dir_list <- lapply(
    model_param_list[1], 
    function(x) initialize_model_params(x)
)

results <- list()
theme <- theme_minimal()
plots <- list()

for (i in 1:length(chosen_models)) {
    
    dir_model_folder <- model_dir_list[i]

    # initial model data
    data_raw <- read.csv(
        paste0(dir_model_folder, "data.csv"), 
        na.strings = c("")
    ) # original data
    
    data <- read_rdump(paste0(dir_model_folder, "count_info.data.R"))

    # load zero inflated fits
    load(paste0(dir_model_folder, "fit.data"))       # as "fit"
    zips <- fit; rm(fit)

    # load posterior simulation
    load(paste0(dir_model_folder, "post.data"))      # as "allsim"

    # create data.frame of observed
    cumm <- lapply(seq(data$P), function(ii) { # each group
        data.frame(index=1:data$end[ii],                    # index/offset/year
                   value=data$counts[ii, ],                 # count
                   cml_value=cumsum(data$counts[ii, ]),     # cumulative
                   off=data$off[ii, ],                      # tax. effort
                   group=ii)}) %>% rbind.fill               # group "family"
    cumm$sim <- 0 # set sim to 0 to indicate that it is observed data

    # create data.frame of simulated (interpolated)
    cummsim <- lapply(seq(length(allsim)), function(jj) { # each sim
        lapply(seq(data$P), function(ii) { # each group
            data.frame(index=1:data$end[ii],
                    value=allsim[[jj]][[ii]],
                    cml_value=cumsum(allsim[[jj]][[ii]]),
                    off=data$off[ii, ],
                    group=ii,
                    sim=jj)}) %>% rbind.fill
        }) %>% rbind.fill

    # combine the observed and simmed series
    Z <- rbind(cumm, cummsim)

    # add original year back
    Z$year <- Z$index + min(data_raw$year) - 1

    # merge observed values
    Z <- data.table(Z)
    obs_Z <- Z[sim == 0, c("group", "year", "cml_value")]

    Z2 <- merge(
        Z, obs_Z, 
        by = c("group", "year"), 
        all.x = T, all.y = T, 
        suffixes = c("_sim", "_obs")
    )
    
    Z2 <- Z2[sim!=0]

    # get accuracy
    result <- lapply(unique(Z2$group), function(grp) {
        lapply(unique(Z2$sim), function(s) {
            to_calc <- Z2[group==grp & sim==s]
            data.frame(
                accuracy(to_calc$cml_value_sim, to_calc$cml_value_obs),
                group = grp, 
                sim = s
            )
        })
    })
    
    results[[i]] <- result
}

# list of models of groups of simulations
formatted <- list()
for (i in 1:length(results)) {
    formatted[[i]] <- lapply(results[[i]], function(grp) {
        rbindlist(lapply(grp, as.data.frame))
    })
}

formatted2 <- list()

mapping <- unique(data.frame(
    groupname = as.character(data_raw$group),
    group = as.numeric(data_raw$group)
))

for (i in 1:length(results)) {
    formatted2[[i]] <- merge(
        rbindlist(formatted[[i]]), mapping,
        by = "group", all.x = T, all.y = F
    )
}


do.call(cbind, lapply(formatted2, function(model) {
    model[, list(MAPE_mean = mean(MAPE)), by=c('groupname')]
}))

do.call(cbind, lapply(formatted2, function(model) {
    model[, list(MAPE_mean = mean(MAPE))]
}))

# https://otexts.com/fpp2/accuracy.html
# https://pkg.robjhyndman.com/forecast/reference/accuracy.html
