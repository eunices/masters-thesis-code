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
    coef0 <- apply(outs$delta[, , 1], 2, function(x) sample(x, 1))
    coef1 <- apply(outs$delta[, ,2], 2, function(x) sample(x, 1))

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


posterior_forecast <- function(data, ftime, model) {

	#' A sample from posterior parameters for
	#' predictions of time series
	#' 
	#' Given a model, make predictions based on a predefined timestep
	#' 
	#' @param data Data as output by initial data processing step 
	#' i.e. count_info.data.R
	#' @param ftime Number of years to predict 
	#' @param model rStan model i.e. fit.data

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

		# Generate offset segment by sampling past ftime of offsets
		len <- length(all_toff)
		toff <- sample(all_toff[(len-ftime):len], ftime, replace = TRUE)

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

