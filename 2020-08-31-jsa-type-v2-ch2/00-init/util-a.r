
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

	# Autoregressive terms
    alp <- apply(outs$alpha, 2, function(x) sample(x, 1))
    bet <- apply(outs$beta, 2, function(x) sample(x, 1))

    # Regression portion
    coef0 <- apply(outs$delta[, , 1], 2, function(x) sample(x, 1))
    coef1 <- apply(outs$delta[, ,2], 2, function(x) sample(x, 1))

    # markov
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

	# Starting counts of the different groups
	initial <- sapply(seq(p), function(pp) data$counts[pp, data$str[pp]])

	# By group
	sims <- list()
	for(ii in seq(p)) {

		# Offset segment starts at first year
		start <- data$str[ii]
		end <- data$end[ii]
		toff <- data$off[ii, ][start:end]
		
		# By time point

		# Initialize parameters
		lambda <- c()
		theta <- c()
		oo <- c() 

		# Set first time point (t = 1)
		lambda[1] <- phi[ii]
		theta[1] <- 0
		oo[1] <- initial[ii]

		# Set subsequent time points
		for(jj in seq(2, end - start + 1)) {
			
			lambda[jj] <- exp(coef0[ii] + coef1[ii] * jj) +
						  alp[ii] * oo[jj - 1] + 
						  bet[ii] * lambda[jj - 1]

			z <- ifelse(oo[jj - 1] == 0, 1, 0)

			theta[jj] <- (z * gam[ii]) + 
						 ((1 - z) * eta[ii])

			oo[jj] <- gamlss.dist::rZIP(
				1, 
				mu = (toff[jj] + 1) * lambda[jj], 
				sigma = theta[jj]
			)

		}

		oo <- c(rep(0, start - 1), oo) # pad with 0s
		sims[[ii]] <- oo

	}

	sims
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
	list2env(mp); rm(mp)

	# Number of groups
	p <- data$P 

	# Initial number of species for each group
	initial <- sapply(seq(p), function(pp) data$counts[pp, data$str[pp]])

	# By group
	out <- list()
	for(ii in seq(p)) {
		
		# Offset segment starts at first year
		start <- data$str[ii]
		end <- data$end[ii]
		all_toff <- data$off[ii, ][start:end]


		# Generate offset segment by sampling past ftime of offsets
		len <- length(all_toff)

		# # original code
		toff <- sample(all_toff[(len-ftime):len], ftime, replace = TRUE)

		# # naive method suggested by roman
		# # https://otexts.com/fpp2/simple-methods.html
		# toff <- rep(all_toff[len], ftime)


		# By time point

		# Initialize parameters
		lambda <- c()
		theta <- c()
		oo <- data$counts[ii, end] # final year's count
		co <- c()                  # empty expected count vec
		
		# Set first time point (t = 1)
		lambda[1] <- oo
		theta[1] <- 0
		co[1] <- oo[1]

		# Set subsequent time points
		for(jj in 2:ftime) {

			lambda[jj] <- exp(coef0[ii] + coef1[ii] * jj) +
						  alp[ii] * oo[jj - 1] + 
						  bet[ii] * lambda[jj - 1]

			z <- ifelse(oo[jj - 1] == 0, 1, 0)

			theta[jj] <- (z * gam[ii]) + 
					     ((1 - z) * eta[ii])

			co[jj] <- gamlss.dist::rZIP(
				1, 
				mu = (toff[jj] + 1) * lambda[jj],
				sigma = theta[jj]
			)

			oo[jj] <- co[jj] # current count becomes next expected count

		}

		out[[ii]] <- co
	}

	out
}

