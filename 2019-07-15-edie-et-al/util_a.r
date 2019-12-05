parse_model_identifier <- function(string) {

    #' Parses model identifier into a list of parameters

    #' Parses a specific model identifier into a list of parameter.
    #' @param string The string that is to be decomposed into a list of parameters.

    model_params <- list()
    string <- strsplit(string, "-")[[1]]

    for (i in 1:length(string)){
        if (i == 1) {
            model_params$dataset <- substr(string[i], 1, 2)
            model_params$ll <- substr(string[i], 3, 3)
        } else {
            first_letter <- substr(string[i], 1, 1)
            if (first_letter == "E" ) { # taxonomic effort
                model_params$te <- as.numeric(substr(string[i], 2, nchar(string[i])))
            }
            if(first_letter == "C") { # chains
                model_params$chains <- as.numeric(substr(string[i], 2, nchar(string[i])))
            }
            if (first_letter == "I") { # iterations
                model_params$iter <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } 
            if (first_letter == "A") { # adapt delta
                model_params$ad <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } 
            if (first_letter == "T") { # tree depth
                model_params$td <- as.numeric(substr(string[i], 2, nchar(string[i])))
            } 
        }
    }
    model_params
}

# Test
# string <- "BMY-C4-I5000-A0.99-T15"
# parse_model_identifier(string)


write_to_log <- function(w, warn_log_fp) {

    #' Writes warning to warning logfile

    #' Writes warning to logfile in specified path. 
    #' @param w Warning output from a try-catch block.
    #' @param warn_log_fp Warning log filepath. Should be a .log file. 

    write(conditionMessage(w), file=warn_log_fp, append=T)
}


sample_model_posterior_parameters <- function(model) {

    #' Samples from model's posterior
    #'
    #' Returns a list of sampled model parameters
    #' @param model Model dumped from rstan

    outs <- extract(model, permuted = TRUE) # posterior
  
    # regression
    coef0 <- apply(outs$coef[, , 1], 2, function(x) sample(x, 1))
    coef1 <- apply(outs$coef[, , 2], 2, function(x) sample(x, 1))

    # acp
    alp <- apply(outs$alpha, 2, function(x) sample(x, 1))  # for each group
    bet <- apply(outs$beta, 2, function(x) sample(x, 1))  # for each group

    # markov
    gam <- apply(outs$gamma, 2, function(x) sample(x, 1))  # for each group
    eta <- apply(outs$eta, 2, function(x) sample(x, 1))  # for each group
      
    # t=1
    phi <- apply(outs$phi, 2, function(x) sample(x, 1))  # for each group

    return(list(coef0=coef0, coef1=coef1,
                alp=alp, bet=bet,
                gam=gam,eta=eta,
                phi=phi))
}


posterior.sim <- function(data, model) {

  #' Sample from posterior parameters for checking model fit
  #' 
  #' Return sampled counts for each year by group
  #' @param over If over =T, use negative binomial, otherwise use zero-inflated Poisson

  # sample from model posterior
  mp <- sample_model_posterior_parameters(model)
  coef0=mp$coef0; coef1=mp$coef1; alp=mp$alp; bet=mp$bet;
  gam=mp$gam; eta=mp$eta; phi=phi; rm(mp)

  # number of groups
  p <- data$P

  # initial count
  initial <- sapply(seq(p), function(pp) data$counts[pp, data$str[pp]])

  # by group
  sims <- list() # store simulations
  for(ii in seq(p)) {
    start <- data$str[ii]; end <- data$end[ii]

    # offset segment starts at first naming year
    toff <- data$off[ii, ][start:end]

    # initialize parameters
    lambda <- c()
    theta <- c()
    oo <- c()  # counts over time
    
    # by time point

    # set first time point
    lambda[1] <- phi[ii]
    theta[1] <- 0
    oo[1] <- initial[ii]

    # set subsequent time points
    for(jj in seq(2, end - start + 1) {
      
      lambda[jj] <- exp(coef0[ii] + coef1[ii] * jj) +
        alp[ii] * oo[jj - 1] + bet[ii] * lambda[jj - 1]

      z <- ifelse(oo[jj - 1] == 0, 1, 0)
      theta[jj] <- (z * gam[ii]) + ((1 - z) * eta[ii])

      oo[jj] <- rZIP(1, lambda = (toff[jj] + 1) * lambda[jj], 
                        sigma = theta[jj])
    }

    oo <- c(rep(0, start - 1), oo) # pad with 0s
    sims[[ii]] <- oo
  
  }

  sims
}


post.forecast <- function(data, ftime, model) {

  #' Make predictions for time series
  #' 
  #' Given a model, make predictions based on a predefined timestep
  #' @param data Data as output by initial data processing step i.e. count_info.data.R
  #' @param ftime Number of years to predict 
  #' @param model rStan model i.e. fit.data

  # sample from model posterior
  mp <- sample_model_posterior_parameters(model)
  coef0=mp$coef0; coef1=mp$coef1; alp=mp$alp; bet=mp$bet;
  gam=mp$gam; eta=mp$eta; phi=phi; rm(mp)

  # number of groups
  p <- data$P 

  # initial count
  initial <- sapply(seq(p), function(pp) data$counts[ pp, data$str[pp]])

  # by group
  out <- list() # store predictions
  for(ii in seq(p)) {
      start <- data$str[ii]; end <- data$end[ii]

      # offset starts at first naming year
      all_toff <- data$off[ii, ][start:end]; len <- length(all_toff)

      # generate offset segment by sampling past decade of offsets
      toff <- sample(all_toff[(len-ftime):len], ftime, replace=TRUE)

      # initialize parameters
      lambda <- c()
      theta <- c()
      oo <- data$counts[ii, end] # final year's count
      co <- c() # empty expected count vec

      # by time point
      
      # set first time point
      lambda[1] <- oo
      theta[1] <- 0
      co[1] <- oo[jj]

      # set subsequent time points
      for(jj in 2:ftime) {
        lambda[jj] <- exp(coef0[ii] + coef1[ii] * jj) +
          alp[ii] * oo[jj - 1] + bet[ii] * lambda[jj - 1]

        z <- ifelse(oo[jj - 1] == 0, 1, 0)
        theta[jj] <- (z * gam[ii]) + ((1 - z) * eta[ii])

        co[jj] <- rZIP(1, lambda = (toff[jj] + 1) * lambda[jj], 
                       sigma = theta[jj])

        oo[jj] <- co[jj] # current count becomes next expected count
      }
      out[[ii]] <- co
  }
  out
}
