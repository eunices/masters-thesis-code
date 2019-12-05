source('2019-07-15-edie-et-al/init_a.r')
print(paste0(Sys.time(), " --- analysis4.r"))

ftime <- as.numeric(25)
set.seed(420)

# initial data
files <- dir(dir_model_folder, pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)

# zero inflated fits
load(paste0(dir_model_folder, "fit.data")) # loads as fit
zips <- fit # reassign to zips
rm(fit) # remove from memory

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

# simulate the forecast
forecast <- mclapply(1:1000, mc.cores=1, function(ii) {
   post.forecast(data=data, ftime=ftime, model=zips) 
})

save(forecast, file=paste0(dir_model_folder, "forecast.data"))
rm(data, zips, forecast)