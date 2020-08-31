parse_model_identifier <- function(string) {

    #' Parses model identifier into a list of parameters

    #' Parses a specific model identifier into a list of parameter.
    #'
    #' @param string The string that is to be decomposed into a list of
    #' parameters.

    model_params <- list()
    string <- strsplit(string, "-")[[1]]

    for (i in 1:length(string)){

        if (i == 1) {

            model_params$dataset <- substr(string[i], 1, 2)     # 
            model_params$ll <- substr(string[i], 3, 3)          
            # "Y" for using lat lon / "N" for using country distribution

        } else {                                

            first_letter <- substr(string[i], 1, 1)

            if (first_letter == "E" ) { # taxonomic effort
                model_params$te <- 
                  as.numeric(substr(string[i], 2, nchar(string[i])))
            }

            if (first_letter == "C") { # chains
                model_params$chains <- 
                  as.numeric(substr(string[i], 2, nchar(string[i])))
            }

            if (first_letter == "I") { # iterations
                model_params$iter <- 
                  as.numeric(substr(string[i], 2, nchar(string[i])))
            } 

            if (first_letter == "A") { # adapt delta
                model_params$ad <- 
                  as.numeric(substr(string[i], 2, nchar(string[i])))
            } 

            if (first_letter == "T") { # tree depth
                model_params$td <- 
                  as.numeric(substr(string[i], 2, nchar(string[i])))
            } 

        }
    }

    model_params
}

# Test
# string <- "BMY-C4-I5000-A0.99-T15"
# parse_model_identifier(string)


initialize_model_params <- function(model_params, custom=NA) {

    #' Parses list of model params and initializes necessary folders
    #'
    #' Parses list of model params into a model identifier and
    #' initializes necessary folders. Returns the model folder directory.
    #' @model_params list The list that contains a list of model parameters.

	model_identifier <- paste0(
		model_params$dataset, model_params$ll, "-",
		"E", as.character(model_params$te), "-",
		"C", as.character(model_params$chains), "-",
		"I", as.character(model_params$iter), "-",
		"A", as.character(model_params$ad), "-",
		"T", as.character(model_params$td)
	)
  
	# Define model folder
	dir_model_folder <- paste0(dir_analysis_edie_model, "/", 
							   model_identifier, "/")
	if(!is.na(custom)) dir_model_folder <- custom

	# Create folders
	dir.create(dir_model_folder)
	dir.create(file.path(dir_model_folder, 'output'))

	# Create log files
	filepath_log <- paste0(dir_model_folder, "/model.log")
	if (!file.exists(filepath_log)) file.create(filepath_log)

	warnings_log <- paste0(dir_model_folder, "/warnings.log")
	if (!file.exists(warnings_log)) file.create(warnings_log)


	c(dir_model_folder, 
	  filepath_log,
	  warnings_log,
	  model_identifier)

} 


write_to_log <- function(w, warn_log_fp) {

    #' Writes warning to warning logfile

    #' Writes warning to logfile in specified path.
	#' 
    #' @param w Warning output from a try-catch block.
    #' @param warn_log_fp Warning log filepath. Should be a .log file. 

    write(conditionMessage(w), file = warn_log_fp, append = TRUE)
}


sample_model_posterior_parameters <- function(model) {

    #' Samples from model's posterior
    #'
    #' Returns a list of sampled model parameters
	#' 
    #' @param model Model dumped from rstan

    outs <- extract(model, permuted = TRUE) # posterior

    # ACP

	# t=1
    phi <- apply(outs$phi, 2, function(x) sample(x, 1))
	
	# t>1

	# Autoregressive terms
    alp <- apply(outs$alpha, 2, function(x) sample(x, 1))
    bet <- apply(outs$beta, 2, function(x) sample(x, 1))

    # Regression portion
    coef0 <- apply(outs$coef[, , 1], 2, function(x) sample(x, 1))
    coef1 <- apply(outs$coef[, , 2], 2, function(x) sample(x, 1))

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

	#' Sample from posterior parameters for checking model fit
	#' 
	#' Return sampled counts for each year by group
	#' 
	#' @param over If over = T, use negative binomial, otherwise use 
	#' zero-inflated Poisson

	# Sample from model posterior
	mp <- sample_model_posterior_parameters(model)
	list2env(mp); rm(mp)

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

		# Set first time point
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

	#' Make predictions for time series
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
	initial <- sapply(seq(p), function(pp) data$counts[ pp, data$str[pp]])

	# By group
	out <- list()
	for(ii in seq(p)) {
		
		start <- data$str[ii]
		end <- data$end[ii]

		# Offset starts at first naming year
		all_toff <- data$off[ii, ][start:end]
		len <- length(all_toff)

		# Generate offset segment by sampling past ftime of offsets
		# TODO: use naive method from
		# https://otexts.com/fpp2/simple-methods.html
		toff <- sample(all_toff[(len-ftime):len], ftime, replace = TRUE)

		# By time point

		# Initialize parameters
		lambda <- c()
		theta <- c()
		oo <- data$counts[ii, end] # final year's count
		co <- c() # empty expected count vec
		
		# Set first time point
		lambda[1] <- oo
		theta[1] <- 0
		co[1] <- oo[1]

		# set subsequent time points
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

