# analysis2.r

sample_model_posterior_parameters <- function(model) {

    #' A single sample of model parameters from model's posterior
    #'
    #' Returns a list of sampled model parameters
	#' 
    #' @param model Model dumped from rstan

    outs <- rstan::extract(model, permuted = TRUE) # posterior

    # ACP

	# t=1
    phi <- apply(outs$phi, 2, function(x) sample(x, 1))
	
	# t>1

	# Autoregressive portion
    alp <- apply(outs$alpha, 2, function(x) sample(x, 1))
    bet <- apply(outs$beta, 2, function(x) sample(x, 1))

    # Regression portion for intercept
	if("delta" %in% names(outs)) {
		coef0 <- apply(outs$delta[, , 1], 2, function(x) sample(x, 1))
    	coef1 <- apply(outs$delta[, ,2], 2, function(x) sample(x, 1))
	} else { # alternate name used in original Sedie code
		coef0 <- apply(outs$coef[, , 1], 2, function(x) sample(x, 1))
    	coef1 <- apply(outs$coef[, ,2], 2, function(x) sample(x, 1))
	}



    # Markov
    gam <- apply(outs$gamma, 2, function(x) sample(x, 1))
    eta <- apply(outs$eta, 2, function(x) sample(x, 1))
      
	list(
		phi=phi, 

		gam=gam,
		eta=eta,

		coef0=coef0, 
		coef1=coef1,
		alp=alp, 
		bet=bet
	)

}


lam <- function(coef0, coef1, alpha, beta, lambda, y, grp, t) {

	# Calculate lambda	
	exp(coef0[grp] + coef1[grp] * t) +
		alpha[grp] * y[t - 1] + 
		beta[grp] * lambda[t - 1]
}


tht <- function(gam, eta, y, grp, t) {

	# Calculate theta
	z <- ifelse(y[t - 1] == 0, 1, 0)
	(z * gam[grp]) + ((1 - z) * eta[grp])

}


posterior_sim <- function(data, model) {

	#' A sample from posterior parameters for checking model fit
	#' 
	#' Return sampled counts for each year by group
	#' 
	#' @param over If over = T, use negative binomial, otherwise use 
	#' zero-inflated Poisson

	# Sample from model posterior
	mp <- sample_model_posterior_parameters(model)
	list2env(mp, .GlobalEnv); rm(mp)

	# Number of groups
	p <- data$P

	sims <- list()
	for(g in seq(p)) { # by group

		# Offset segment starts at first year
		start <- data$str[g]
		end <- data$end[g]		
		toff <- data$off[g, ][start:end]

		# Initialize parameters 
		# & set first time point
		lambda <- c(phi[g])
		theta <- c(0)
		oo <- c(data$counts[g, start])  

		for(t in seq(2, end - start + 1)) { # by time

			lambda[t] <- lam(
				coef0, coef1,
				alp, bet, lambda, 
				y = oo,
				grp = g, t = t
			)

			theta[t] <- tht(
				gam, eta, 
				y = oo, 
				grp = g, t = t
			)

			oo[t] <- gamlss.dist::rZIP(
				n = 1, 
				mu = (toff[t] + 1) * lambda[t], 
				sigma = theta[t]
			)

		}

		oo <- c(rep(0, start - 1), oo) # pad with 0s
		sims[[g]] <- oo

	}

	sims # return counts
}


posterior_forecast <- function(data, ftime, model, offset = "random") {

	#' A sample from posterior parameters for
	#' predictions of time series
	#' 
	#' Given a model, make predictions based on a predefined timestep
	#' 
	#' @param data Data as output by initial data processing step 
	#' i.e. count_info.data.R
	#' @param ftime Number of years to predict 
	#' @param model rStan model i.e. fit.data
	#' @param offset Offset type, either "random", "decrease" or "constant"
	
	#' "random" represents the last previous years
	#' "decrease" represents a 3% decrease per year
	#' "constant" represents the same as previous years

	# Sample from model posterior
	mp <- sample_model_posterior_parameters(model)
	list2env(mp, .GlobalEnv); rm(mp)

	# Number of groups
	p <- data$P 

	forecast <- list()
	for(g in seq(p)) { # by group
		
		# Offset segment starts at first year
		start <- data$str[g]
		end <- data$end[g]
		all_toff <- data$off[g, ][start:end]

		# Generate offset segment 
		len <- length(all_toff)
		if (offset == "random") {
			# by sampling past ftime of offsets
			toff <- sample(all_toff[(len-ftime):len], ftime, replace = TRUE)
		} else if (offset == "constant") {
			toff <- rep(all_toff[len], ftime)
		} else if (offset == "decreasing") {
			toff <- c(all_toff[len])
			for(i in 1:ftime) {
				toff <- c(toff, round(toff[i]*0.03, 0))
			}
		}

		# Initialize parameters
		# & set first time point
		oo <- data$counts[g, end] # final year's count
		
		lambda <- c(oo)
		theta <- c(0)
		co <- c(oo) # forecast

		for(t in 2:ftime) { # by time

			lambda[t] <- lam(
				coef0, coef1,
				alp, bet, lambda, 
				y = oo,
				grp = g, t = t
			)

			theta[t] <- tht(
				gam, eta, 
				y = oo, 
				grp = g, t = t
			)

			co[t] <- gamlss.dist::rZIP(
				n = 1, 
				mu = (toff[t] + 1) * lambda[t],
				sigma = theta[t]
			)

			oo[t] <- co[t] # current count becomes next expected count

		}

		forecast[[g]] <- co
	}

	forecast
}


# analysis3.r

log_chain_sampling <- function(fit, dir_model_folder) {
	
	# Filter for chains that did not mix
	badchain <- data.table(summary(fit)$summary)[Rhat > 1.1 | Rhat < 0.9]
	output <- paste0(dir_model_folder, "output/chain_sampling.txt")

	# Write warning output
	if (nrow(badchain) > 0) {

		sink(output)
		cat(
			"Chains not converged. 
			Increase iterations in sampling in stan() function 
			call in code/model.r. 
			Do not interpret model results as parameter estimates are 
			currently highly unstable."
		)
		sink()

	} else {

		sink(output)
		cat(
			"Chains have converged. Model results robust to posterior sampling."
		)
		sink()

	}

}


convert_data_to_df <- function(data) {

	print("----------- Convert data to df (cumulative sum from counts)")	
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
	cumm
}


convert_allsim_to_df <- function(allsim, data) {

	print("----------- Convert allsim to df (cumulative sum from counts)")

	# Cumulative series for simmed data
	cumsim <- lapply(seq(length(allsim)), function(jj) { # each sim

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

	cumsim
}


create_Z <- function(cum, cumsim, data_raw) {

	Z <- rbind(cum, cumsim) # combine obs/ sim
	Z$year <- Z$index + min(data_raw$year) - 1 # add original year

	Z
}


summarize_simulations_observed <- function(data, allsim) {

	cum <- convert_data_to_df(data)
	cumsim <- convert_allsim_to_df(allsim, data)
	Z <- create_Z(cum, cumsim, data_raw)

	# Cumulative count (last year): Simulated/sampled data from posterior
	print("----------- Summarize 80 CI for simulations")
	sum_y <- data.table(Z)[
		sim != 0 & index == max(index), 
		list(
			med = median(cml_value),
			lower = quantile(cml_value, probs = 0.1),
			upper = quantile(cml_value, probs = 0.9)
		),
		by = "group"
	]

	# Cumulative count (last year): Observed data
	print("----------- Get maximum count for observed data")
	obs_count <- data.table(Z)[
		sim == 0 & index == max(index),  # sim == 0 is observed data
		list(count = cml_value),
		by = "group"
	]

	list(Z = Z, sum_y = sum_y, obs_count = obs_count)

}


extract_delta <- function(fit) {

	print("----------- Extract & summarize coefficients from delta")

	# outs$delta[, ,2] is the coefficient that estimates the long-term trend in
	# description rate, it's in log units, i.e. need to exponentiate it

	# where 0 means stable trend, positive means increasing description
	# through time and negative is decreasing...

	# `coef` is an array where 
	# [posterior sample [1:N samples], 
	# group number[1:N groups], coefficient[1:2]]

	coef <- rstan::extract(fit, par = "delta")[[1]]

	group_cf1 <- apply(coef, 1, function(i) i[, 1]) # intercept
	group_cf2 <- apply(coef, 1, function(i) i[, 2]) # coef * year

	# get mean and 80 CI of coefficient (representing slow down)
	cf2_ci80 <- data.frame( t(apply(
		group_cf2, 
		1, 
		quantile, 
		probs = c(0.1, 0.9)
	)) )

	names(cf2_ci80) <- c("prob10", "prob90")

	cf2 <- cbind(data.frame(
		group = 1:nrow(group_cf2),
		slowdown = apply(group_cf2, 1, mean)
	), cf2_ci80)

	list(cf2 = cf2, group_cf1 = group_cf1, group_cf2 = group_cf2)

}


combine_results <- function(sum_y, obs_count, cf2, mapping) {

	# Compile results into table
	rows <- as.numeric(rownames(obs_count))
	matched <- match(rows, mapping[, 2])

	results <- data.table(
		group = mapping[matched, 2], 
		groupname = mapping[matched, 1], 

		observed_species = obs_count$count,

		expected_median = round(sum_y$med, 0),
		expected_CI_lower = round(sum_y$lower, 0),
		expected_CI_higher = round(sum_y$upper, 0),

		# long-term trend coef
		slowdown = round(signif(cf2$slowdown, 3), 4), 
		slowdown_CI_lower = round(signif(cf2$prob10, 3), 4),
		slowdown_CI_higher = round(signif(cf2$prob90, 3), 4),

		row.names = NULL    
	) 

	results <- results[order(-slowdown)]

	results
}


prepare_plot1_2_data <- function(Z) {

	print("----------- Prepare plot 1/2 data - observed data")
	
	# Observed data
	obs <- data.table(Z)[sim == 0] # subset to observed series

	print("----------- Prepare plot 1/2 data - simulations")
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
	goodsims <- data.table(sims)[
		year == max(year) & cml_value < max(obs$cml_value) * 4,
		c("group", "sim")
	]

	sims <- inner_join(sims, goodsims, by = c("group", "sim"))

	list(sims = sims, obs = obs)
}


save_plot1 <- function(sims, obs, labels, dir_model_folder) {

	#-------------------
	# Plot #1 
	#-------------------
	output1 = paste0(dir_model_folder, "output/cumulative_fit.pdf")
	output2 = paste0(dir_model_folder, "output/cumulative_fit.png")
	print(paste0("Plotting cumulative_fit.pdf"))


	# Plot cumulative counts
	P <- ggplot() +
		geom_path(
			data = sims, 
			aes(x = year, y = cml_value, group = sim), 
			col = "skyblue2", alpha = 0.1
		) +
		geom_path(data = obs, aes(x = year, y = cml_value)) +
		facet_wrap(~group, labeller = as_labeller(labels), scales = "fixed") +
		ylab("Number of species\n") + 
		xlab("Year") + theme +
		scale_y_continuous(limits = c(0, 6000))

	ggsave(P, file = output1, width = 10, height = 6)
	ggsave(P, file = output2, width = 10, height = 6)

}


save_plot2 <- function(sims, obs, labels, dir_model_folder) {
	
	#-------------------
	# Plot #2
	#-------------------
	output = paste0(dir_model_folder, "output/count_fit.pdf")
	print(paste0("Plotting count_fit.pdf"))

	# Using the same data as Plot #1
	# Plot per year counts
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

	ggsave(P, file = output, width = 10, height = 6)

}


prepare_plot3_data <- function(data, data_raw, group_cf1, group_cf2) {

	print("----------- Prepare plot 3 data - omega")
	
	# Prepare data

	# Calculate omega ("long term trend fit") for PLOTTING
	# note: initial code labelled "lambda", 
	# now changed to "omega" (to clarify with authors)
	omega <- lapply(seq(data$P), function(ii) { # for each group

		cf1 <- group_cf1[ii, ]
		cf2 <- group_cf2[ii, ] # "long-term trend coefficient"

		time <- 1:data$end[ii]
		duration <- max(time) - rev(time)

		cpair <- lapply(1:length(cf1), function(kk) { # each coefficient pair

			d0 <- cf1[kk]
			d1 <- cf2[kk]
			om <- exp(d0 + d1 * time)

			cbind(time = duration, om, group = ii, sim = kk)

		})

		res <- data.frame(do.call(rbind, cpair))
		res

	}) %>% rbind.fill

	# Add original year back
	omega$year <- omega$time + min(data_raw$year)

	# Set up plotting data for long-term trends
	sims <- omega %>%
		split(., .$group) %>%              
		lapply(., function(oo){            # for each group

			ids <- sample(unique(oo$sim), 200)
			oo[oo$sim %in% ids, ]

	}) %>% rbind.fill

	# Mean line per group
	om_mean <- data.table(sims)[,
		list(mean = mean(om)),
		by = c("group", "year")
	] 
	
	list(sims = sims, om_mean = om_mean)

}


save_plot3 <- function(obs, sims, om_mean, labels, dir_model_folder) {

	#-------------------
	# Plot #3
	#-------------------

	output = paste0(dir_model_folder, "output/regression.pdf")
	print(paste0("Plotting regression.pdf "))

	# Plot long-term trends
	P <- ggplot() +
		geom_path(data = obs, aes(x = year, y = (value/(off+1)))) +
		geom_line(
			data = sims, aes(x = year, y = om, group = sim), 
			col = "skyblue2", alpha = 0.1
		) +
		geom_line(data = om_mean, aes(x = year, y = mean), col = "royalblue") +
		facet_wrap(~group, labeller = as_labeller(labels), scales = "free_y") +
		ylab("Number of species") +
		xlab("Year") +
		theme

	ggsave(P, file = output, width = 10, height = 6)
}


convert_forecast_to_df <- function(data, data_raw, forecast) {

	print("----------- Convert forecast to df (cumulative sum from count)")
	
	myears <- ncol(data$counts)             # number of years used in modelling
	fyears <- length(forecast[[1]][[1]])    # number of forecast years
	min_year <- min(data_raw$year) # earliest year used in modelling
	index <- ((myears + 1):(myears + fyears)) + min_year - 1 # forecast years

	# Forecast coerce to data.frame from list of list and sum counts
	forsim <- lapply(seq(length(forecast)), function(jj) { # each simulation
	# note: there should be 1000 simulations

		lapply(seq(data$P), function(ii) { # each group

			data.frame(
				index = index,
				value = cumsum(forecast[[jj]][[ii]]), # sum the poisson counts
				group = ii,
				sim = jj
			)

		}) %>% rbind.fill
	}) %>% rbind.fill

	forsim

}


summarize_forecasts <- function(data, data_raw, forecast, obs) {

	# forecast coerced to data.frame from list of list and sum counts
	forsim <- convert_forecast_to_df(data, data_raw, forecast)

	print("----------- Summarize forecast results")
	# For each group and each sim, find the mean and CI of cumsums

	last_observed_count <- data.table(obs)[
		, list(cml_value = max(cml_value)), by = "group"
	] 

	forsim <- data.table(forsim)

	fore_results <- forsim[,
		list(
			value = max(value), # highest value
			index = max(index)  # for the last year
			), 
		by = c("group", "sim")
	][,
		list(
			fore_mu = round(mean(value), 0),
			fore_lower = round(quantile(value, 0.1), 0) ,
			fore_upper = round(quantile(value, 0.9), 0)
		),
		by = "group"
	]

	# Note: in original code but commented out
	# Add to observed count from last year
	# fore_results <- merge(fore_results, last_observed_count, by = "group")
	# fore_results$fore_mu <- fore_results$fore_mu + fore_results$cml_value
	# fore_results$fore_lower <- fore_results$fore_lower + fore_results$cml_value
	# fore_results$fore_upper <- fore_results$fore_upper + fore_results$cml_value
	# fore_results$cml_value <- NULL

	fore_results

}


output_results_csv <- function(results, forecast_results, dir_model_folder) {

	print("Generating output.csv table")

	# Merge to Results table

	final_results <- merge(results, forecast_results, by = "group")

	final_results <- final_results[order(-observed_species)]

	output <- paste0(dir_model_folder, "output/results.csv")
	write.csv(final_results, file = output, row.names = FALSE)

}
