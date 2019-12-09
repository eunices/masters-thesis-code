# visualize results from model fit and forecasting
source('2019-07-15-edie-et-al/init_a.r')
print(paste0(Sys.time(), " --- visualising results"))


####### Load data

# load data
data_raw <- read.csv(paste0(dir_model_folder, "data.csv"), na.strings=c("")) # original data
data <- read_rdump(paste0(dir_model_folder, "count_info.data.R")) # initial model data

# load zero inflated fits
load(paste0(dir_model_folder, "fit.data")) # as "fit"
zips <- fit; rm(fit)

# load posterior simulation
load(paste0(dir_model_folder, "post.data")) # as "allsim"

# load forecast predictions
load(paste0(dir_model_folder, "forecast.data")) # as "forecast"

# map model indeces to original variables
mapping <- unique(data.frame(groupname=as.character(data_raw$group),
                             group=as.numeric(data_raw$group)))

####### END Load data

####### Check for chain convergence

# filter for chians that did not mix
badchain <- summary(zips)$summary %>%
    data.frame(par=row.names( . ), . ) %>%
    filter(Rhat > 1.1 | Rhat < 0.9)

# write warning output
if (nrow(badchain) > 0) {
    interpret <- FALSE
    sink(paste0(dir_model_folder, "output/chain_sampling.txt"))
    cat("Chains have not converged. Increase interations in sampling in stan() function 
        call in code/model.r. Do not interpret model results as parameter estimates are 
        currently highly unstable.")
    sink()
} else {
    interpret <- TRUE
    sink(paste0(dir_model_folder, "output/chain_sampling.txt"))
    cat("Chains have converged. Model results are robust to posterior sampling.")
    sink()
}
####### END Check for chain convergence


####### Prepare cumulative series

# cumulative series for observed data
cumm <- lapply(seq(data$P), function(ii) { # each group
    data.frame(index=1:data$end[ii],                    # index (offset "year")
               value=data$counts[ii, ],                 # count
               cml_value=cumsum(data$counts[ii, ]),     # cumulative
               off=data$off[ii, ],                      # tax. effort
               group=ii)}) %>% rbind.fill               # group (e.g. "family")
cumm$sim <- 0 # set sim to 0 to indicate that it is observed data

# cumulative series for simmed data
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

####### END Prepare cumulative series


####### Summarise model results
sum_y <- Z %>% 
  filter(sim !=0 & index==max(index)) %>% # last observation
  group_by(group) %>%
  dplyr::summarize(med=median(cml_value),
                   lower=quantile(cml_value, probs=0.1),
                   upper=quantile(cml_value, probs=0.9))

obs_count <- Z %>%
  filter(sim==0 & index==max(index)) %>% # sim==0 is observed data
  group_by(group) %>%
  dplyr::summarize(count=cml_value)

# outs$coef[2] is the coefficient that estimates the long-term trend in
# description rate, it's in log units, i.e. need to exponentiate it

# where 0 means stable trend, positive means increasing description
# through time and negative is decreasing...

# `coef` is an array where 
# [posterior sample [1:N samples], group number[1:N groups], coefficient[1:2]]
coef <- rstan::extract(zips, par="coef")[[1]]

group.cf1 <- apply(coef, 1, function(i) i[, 1])
group.cf2 <- apply(coef, 1, function(i) i[, 2])

# get mean and 80 CI
mean_cf2 <- data.frame(provid=1:nrow(group.cf2),
                       slowdown=apply(group.cf2, 1, mean))
CI80_cf2 <- data.frame(provid=1:nrow(group.cf2), 
                       slowdown=t(apply(group.cf2, 1, quantile, probs=c(0.1, 0.9))))

# compile results into table
results <- data.frame(
    group=mapping[match(as.numeric(rownames(obs_count)), mapping[, 2]), 2], 
    groupname=mapping[match(as.numeric(rownames(obs_count)), mapping[, 2]), 1], 
    observed_species=obs_count$count,
    expected_median=round(sum_y$med, 0),
    expected_CI_lower=round(sum_y$lower, 0),
    expected_CI_higher=round(sum_y$upper, 0),
    slowdown=round(signif(mean_cf2$slowdown, 3), 4),
    slowdown_CI_lower=round(signif(CI80_cf2[, 2], 3), 4),
    slowdown_CI_higher=round(signif(CI80_cf2[, 3], 3), 4),
    row.names=NULL) %>% 
        arrange(slowdown)

# compile results of lambda (poisson parameter)
lambda <- lapply(seq(data$P), function(ii) { # for each group
    cf1 <- group.cf1[ii, ]
    cf2 <- group.cf2[ii, ]
    time <- 1:data$end[ii]
    duration <- max(time) - rev(time)

    cPair <- lapply(1:length(cf1), function(kk) { # for each coefficient pair
        b0 <- cf1[kk]
        b1 <- cf2[kk]
        lam <- exp(b0 + b1 * time)
        cbind(time=duration, lam, group=ii, sim=kk)
    })

    res <- data.frame(do.call(rbind, cPair))
    res
}) %>% rbind.fill

# add original year back
lambda$year <- lambda$time + min(data_raw$year)

####### END Summarise model results


####### Plot model fit

obs <- Z %>% filter(sim==0)   # subset to observed series

# set up plotting data for group panels
sims <- filter(Z, sim!=0) %>% # subset a sample of simmed series
    split( . , .$group) %>%   # group by group
    lapply( . , function(oo){ # for each group
        ids <- sample(unique(oo$sim), 200)
        oo[ oo$sim %in% ids, ]
    }) %>% rbind.fill

# only keep sims less that 4 times the max observed value and with the max year
goodsims <- filter(sims, year==max(year) & cml_value < max(obs$cml_value)*4) %>%
    dplyr::select(group, sim)
sims <- inner_join(sims, goodsims, by=c("group", "sim"))
rm(goodsims)

# set up facet labels
labels <- as.character(mapping$groupname)
names(labels) <- mapping$group

# plot cumulative counts
P <- ggplot() +
    geom_path(data=sims, aes(x=year, y=cml_value, group=sim), 
              col="skyblue2", alpha=0.1) +
    geom_path(data=obs, aes(x=year, y=cml_value)) +
    facet_wrap(~group, scales="free_y", labeller=as_labeller(labels)) +
    ylab("Number of species") + 
    xlab("Year")
ggsave(P, file=paste0(dir_model_folder, "output/cumulative_fit.pdf"), width=10, height=6)

# plot per year counts
P <- ggplot() +
    geom_path(data=sims, aes(x=year, y=value, group=sim),
              col="skyblue2", alpha=0.1) +
    geom_path(data=obs, aes(x=year, y=value)) +
    facet_wrap(~group, scales="free_y", labeller=as_labeller(labels)) +
    ylab("Number of species") + 
    xlab("Year")
ggsave(P, file=paste0(dir_model_folder, "output/count_fit.pdf"), width=10, height=6)

# set up plotting data for long-term trends
sims <- filter(lambda, sim!=0) %>%   # subset only simulated series
    split(., .$group) %>%          # by group
    lapply(., function(oo){        # for each group
        ids <- sample(unique(oo$sim), 200)
        oo[oo$sim %in% ids, ]
    }) %>% rbind.fill

# mean line per group
mu_sim <- sims %>%
    group_by(group, year) %>%
    dplyr::summarize(mu=mean(lam)) %>%
    data.frame()

# plot long term trends
P <- ggplot() +
    geom_path(data=obs, aes(x=year, y=(value/(off+1)))) +
    geom_line(data=sims, aes(x=year, y=lam, group=sim), 
              col="skyblue2", alpha=0.1) +
    geom_line(data=mu_sim, aes(x=year, y=mu), col="royalblue") +
    facet_wrap(~group, scales="free_y", labeller=as_labeller(labels)) +
    ylab("Number of species") +
    xlab("Year")
ggsave(P, file=paste0(dir_model_folder, "output/regression.pdf"), width=10, height=6)

rm(P)
####### END Plot model fit


####### Summarise forecast

# cumulative series for sampled data
nc <- ncol(data$counts)
len_fc <- length(forecast[[1]][[1]])
min_year <- min(data_raw$year)
index <- ((nc + 1):(nc + len_fc)) + (min_year - 1)

forsim <- lapply(seq(length(forecast)), function(jj) { # each sim
    lapply(seq(data$P), function(ii) { # each group
        data.frame(index=index, value=cumsum(forecast[[jj]][[ii]]), group=ii, sim=jj)
    }) %>% rbind.fill
}) %>% rbind.fill

# for each group and each sim, find the mean and CI of cumsums
fore.table <- split(forsim, forsim$group) %>% # by group
    lapply(., function(gg) { # for each group
        group <- unique(gg$group)
        vv <- lapply(split(gg, gg$sim), function(oo) { # for each sim
            cs <- max(oo$value)
            oo[nrow(oo), ]$value <- cs
            oo[nrow(oo), ]
        }) %>% rbind.fill
        fore.mu <- round(mean(vv$value), 0) +               # summarize mean expected
            max(obs[obs$group == group, "cml_value"])       # add to currents counts
        fore.lower <- round( quantile(vv$value, 0.1), 0) +  # summarize lower expected
            max(obs[obs$group == group, "cml_value"])       # add to currents counts    
        fore.upper <- round( quantile(vv$value, 0.9), 0) +  # summarize upper expected
            max(obs[obs$group == group, "cml_value"])       # add to currents counts
        data.frame(group=group, fore.mu, fore.lower, fore.upper)    
    }) %>% rbind.fill

# merge to Results table
final_results <- merge(results, fore.table, by="group") %>%
    arrange(desc(observed_species))
write.csv(final_results, file=paste0(dir_model_folder,"output/results.csv"), row.names=FALSE)
rm(final_results)

####### END Summarise forecast


# remove variables to free up memory
rm(obs, sims, mu_sim, forsim)
rm(zips, mapping)
