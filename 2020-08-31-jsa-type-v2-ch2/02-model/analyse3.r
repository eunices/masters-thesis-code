# Setup
source('2020-08-31-jsa-type-v2-ch2/00-init/init-a.r')


# Parameters
theme <- theme_minimal()


# Script
print(paste0(Sys.time(), " --- Visualising results"))


####### Load data

# Load data

# Original data
data_raw <- read.csv(paste0(dir_model_folder, "data.csv"), na.strings=c("")) 

# R data
data <- read_rdump(paste0(dir_model_folder, "count_info.data.R")) 

# Load zero inflated fits
load(paste0(dir_model_folder, "fit.data"))       # as "fit"
zips <- fit; rm(fit)

# Load posterior simulation
load(paste0(dir_model_folder, "post.data"))      # as "allsim"

# Load forecast predictions
load(paste0(dir_model_folder, "forecast.data"))  # as "forecast"

# Map model indices to original variables
mapping <- unique(data.frame(
    groupname = as.character(data_raw$group),
    group = as.numeric(data_raw$group)
))



####### Check for chain convergence

# Filter for chains that did not mix
badchain <- summary(zips)$summary %>%
    data.frame(par = row.names( . ), . ) %>%
    filter(Rhat > 1.1 | Rhat < 0.9)

# write warning output
if (nrow(badchain) > 0) {

    interpret <- FALSE
    sink(paste0(dir_model_folder, "output/chain_sampling.txt"))
    cat(
        "Chains have not converged. 
         Increase interations in sampling in stan() function 
         call in code/model.r. 
         Do not interpret model results as parameter estimates are 
         currently highly unstable."
    )
    sink()

} else {

    interpret <- TRUE
    sink(paste0(dir_model_folder, "output/chain_sampling.txt"))
    cat(
        "Chains have converged. Model results are robust to posterior sampling."
    )
    sink()

}


#######  Get predictions

# Cumulative series for observed data
cumm <- lapply(seq(data$P), function(ii) { # each group

    data.frame(
        index = 1:data$end[ii],                 # index (offset "year")
        value = data$counts[ii, ],              # count
        cml_value = cumsum(data$counts[ii, ]),  # cumulative
        off = data$off[ii, ],                   # tax. effort
        group = ii
    )

}) %>% rbind.fill
           
cumm$sim <- 0 # set sim to 0 to indicate that it is observed data

# Cumulative series for simmed data
cummsim <- lapply(seq(length(allsim)), function(jj) { # each sim

    lapply(seq(data$P), function(ii) { # each group

        data.frame(
            index = 1:data$end[ii],
            value = allsim[[jj]][[ii]],
            cml_value = cumsum(allsim[[jj]][[ii]]),
            off = data$off[ii, ],
            group = ii,
            sim = jj
        )
        
    }) %>% rbind.fill
}) %>% rbind.fill

# Combine the observed and simmed series
Z <- rbind(cumm, cummsim)

# Add original year back
Z$year <- Z$index + min(data_raw$year) - 1

# Cumulative count (last year): Simulated/sampled data from posterior
sum_y <- Z %>% 
    filter(sim != 0 & index == max(index)) %>% # last observation
    group_by(group) %>%
    dplyr::summarize(
        med = median(cml_value),
        lower = quantile(cml_value, probs = 0.1),
        upper = quantile(cml_value, probs = 0.9)
    )

# Cumulative count (last year): Observed data
obs_count <- Z %>%
    filter(sim == 0 & index == max(index)) %>% # sim == 0 is observed data
    group_by(group) %>%
    dplyr::summarize(count = cml_value)


####### Get parameters

# outs$delta[, ,2] is the coefficient that estimates the long-term trend in
# description rate, it's in log units, i.e. need to exponentiate it

# where 0 means stable trend, positive means increasing description
# through time and negative is decreasing...

# `coef` is an array where 
# [posterior sample [1:N samples], group number[1:N groups], coefficient[1:2]]
coef <- rstan::extract(zips, par = "delta")[[1]]

group_cf1 <- apply(coef, 1, function(i) i[, 1]) # intercept
group_cf2 <- apply(coef, 1, function(i) i[, 2]) # coef * year

# get mean and 80 CI of coefficient (representing slow down)
cf2_ci80 <- data.frame( t(apply(group_cf2, 1, quantile, probs = c(0.1, 0.9))) )

names(cf2_ci80) <- c("prob10", "prob90")

cf2 <- cbind(data.frame(
    group = 1:nrow(group_cf2),
    slowdown = apply(group_cf2, 1, mean)
), cf2_ci80)

# Compile results into table
rows <- as.numeric(rownames(obs_count))

matched <- match(rows, mapping[, 2])

results <- data.frame(
    group = mapping[matched, 2], 
    groupname = mapping[matched, 1], 
    observed_species = obs_count$count,
    expected_median = round(sum_y$med, 0),
    expected_CI_lower = round(sum_y$lower, 0),
    expected_CI_higher = round(sum_y$upper, 0),
    slowdown = round(signif(cf2$slowdown, 3), 4),
    slowdown_CI_lower = round(signif(cf2$prob10, 3), 4),
    slowdown_CI_higher = round(signif(cf2$prob90, 3), 4),
    row.names = NULL    
) %>% arrange(slowdown)

# Compile results of lambda (poisson parameter)

lambda <- lapply(seq(data$P), function(ii) { # for each group

    cf1 <- group_cf1[ii, ]
    cf2 <- group_cf2[ii, ]

    time <- 1:data$end[ii]
    duration <- max(time) - rev(time)

    cpair <- lapply(1:length(cf1), function(kk) { # for each coefficient pair

        b0 <- cf1[kk]
        b1 <- cf2[kk]
        lam <- exp(b0 + b1 * time)

        cbind(time = duration, lam, group = ii, sim = kk)

    })

    res <- data.frame(do.call(rbind, cpair))
    res

}) %>% 
rbind.fill

# Add original year back
lambda$year <- lambda$time + min(data_raw$year)


####### Plots

# Observed data
obs <- Z %>% filter(sim == 0)   # subset to observed series

# Simulated data (sample 200 time series from each group) for plotting
sims <- Z %>% filter(sim != 0) %>% # subset a sample of simmed series
    split( . , .$group) %>%   # group by group
    lapply( . , function(oo){ # for each group

        ids <- sample(unique(oo$sim), 200)
        oo[oo$sim %in% ids, ]

    }) %>% rbind.fill

# Only plot sims that have 
# less that 4 times the max observed value (of all groups)
# and with the max year (to remove duplicates)
goodsims <- sims %>% filter(
    year == max(year) & cml_value < max(obs$cml_value) * 4
) %>% dplyr::select(group, sim)

sims <- inner_join(sims, goodsims, by = c("group", "sim"))

rm(goodsims)


# Set up facet labels
labels <- as.character(mapping$groupname)

names(labels) <- mapping$group


# Plot cumulative counts
print("Plotting cumulative_fit.pdf")

P <- ggplot() +
    geom_path(
        data = sims, 
        aes(x = year, y = cml_value, group = sim), 
        col = "skyblue2", alpha = 0.1
    ) +
    geom_path(data = obs, aes(x = year, y = cml_value)) +
    facet_wrap(~group, labeller = as_labeller(labels), scales = "free_y") +
    ylab("Number of species") + 
    xlab("Year") + theme 

ggsave(
    P, file = paste0(dir_model_folder, "output/cumulative_fit.pdf"), 
    width = 10, height = 6
)


# Plot per year counts
print("Plotting count_fit.pdf")

P <- ggplot() +
    geom_path(
        data = sims, 
        aes(x = year, y = value, group = sim),
        col = "skyblue2", alpha = 0.1
    ) +
    geom_path(data = obs, aes(x = year, y = value)) +
    facet_wrap(~group, labeller = as_labeller(labels), scales = "free_y") +
    ylab("Number of species") + 
    xlab("Year") + 
    theme

ggsave(
    P, file = paste0(dir_model_folder, "output/count_fit.pdf"),
    width = 10, height = 6
)


# Set up plotting data for long-term trends
sims <- filter(lambda, sim != 0) %>%   # subset only simulated series
    split(., .$group) %>%              # by group
    lapply(., function(oo){            # for each group

        ids <- sample(unique(oo$sim), 200)
        oo[oo$sim %in% ids, ]

    }) %>% rbind.fill


# Mean line per group

mu_sim <- sims %>%
    group_by(group, year) %>%
    dplyr::summarize(mu = mean(lam)) %>%
    data.frame()

# Plot long term trends
print("Plotting regression.pdf")

P <- ggplot() +
    geom_path(data = obs, aes(x = year, y = (value/(off+1)))) +
    geom_line(data = sims, aes(x = year, y = lam, group = sim), 
              col = "skyblue2", alpha = 0.1) +
    geom_line(data = mu_sim, aes(x = year, y = mu), col = "royalblue") +
    facet_wrap(~group, labeller = as_labeller(labels),scales = "free_y") +
    ylab("Number of species") +
    xlab("Year") +
    theme

ggsave(
    P, file = paste0(dir_model_folder, "output/regression.pdf"),
    width = 10, height = 6
)

rm(P)


####### Create csv table

print("Generating output.csv table")

# Cumulative series for sampled data
n_md_years <- ncol(data$counts)           # number of years used in modelling
n_fc_years <- length(forecast[[1]][[1]])   # number of forecast years
min_year <- min(data_raw$year)         # earliest year used in modelling 0th yr

# forecast years
index <- ((n_md_years + 1):(n_md_years + n_fc_years)) + min_year - 1


# forecast
forsim <- lapply(seq(length(forecast)), function(jj) { # each simulation x1000

    lapply(seq(data$P), function(ii) { # each group

        data.frame(
            index = index,
            value = cumsum(forecast[[jj]][[ii]]), # sum the poisson counts
            group = ii,
            sim = jj
        )

    }) %>% rbind.fill
}) %>% rbind.fill

# for each group and each sim, find the mean and CI of cumsums
forecast_table <- split(forsim, forsim$group) %>% # by group
    lapply(., function(gg) { # for each group

        group <- unique(gg$group)

        # get the highest cumulative count for each simulation (for each group)
        vv <- lapply(split(gg, gg$sim), function(oo) { # for each sim

            cs <- max(oo$value)             # max cumulative count for sim
            oo[nrow(oo), ]$value <- cs      # set it as last row
            oo[nrow(oo), ]                  

        }) %>% rbind.fill

        final_cumulative_count <- max(obs[obs$group == group, "cml_value"]) 

        fore_mu <- round(mean(vv$value), 0) + final_cumulative_count
        fore_lower <- round(quantile(vv$value, 0.1), 0) + final_cumulative_count   
        fore_upper <- round(quantile(vv$value, 0.9), 0) + final_cumulative_count

        data.frame(
            group = group, 
            fore_mu, fore_lower, fore_upper
        )    

    }) %>% rbind.fill

# Merge to Results table

final_results <- merge(
    results, forecast_table, 
    by = "group"
    ) %>% arrange(desc(observed_species))

write.csv(
    final_results, file = paste0(dir_model_folder,"output/results.csv"), 
    row.names = FALSE
)


# Remove variables to free up memory
rm(final_results)
rm(obs, sims, mu_sim, forsim)
rm(zips, mapping)
