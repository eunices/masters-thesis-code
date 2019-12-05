source('2019-07-15-edie-et-al/init_a.r')
print(paste0(Sys.time(), " --- analysis3.r"))

# run posterior simulation from model fit

set.seed(420)

# initial data
files <- dir(dir_model_folder, pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)

# zero inflated fits
load(paste0(dir_model_folder, "fit.data")) # loads as fit
zips <- fit # reassign to zips
rm(fit) # remove from memory


# N <- length(slotNames(fit))
# for(i in 1:N) {
#   print(i)
#   name <- slotNames(fit)[i]; print(eval(paste0("fit@", name)))
# }


# Diagnostic plots
# launch_shinystan(zips)
# n <- 30; n_plots <- ceiling(length(names(zips))/n)
# for (i in 1:1) {
#     par(ask=T)
#     start = 1 + (i-1)*n; end = ifelse(i==n_plots, length(names(zips)), start + 29)
#     tp <- traceplot(zips, pars=names(zips)[start:end])
#     tp
# }; par(ask=F)

png(paste0(dir_model_folder, 'traceplot.png'), width=24, height=16, units="in", res=150)
traceplot(zips, pars=names(zips))
dev.off() 
# TODO: still doesn't work

write.csv(summary(zips)$summary, paste0(dir_model_folder, 'fit.csv'), fileEncoding='UTF-8')

# posterior predictive simulations for checking model fit
posterior.sim <- function(data, model, over = FALSE) {

  #' Sample from posterior parameters
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

    toff <- data$off[ii, ][start:end] # offset segment starts at first naming year

    lambda <- c()
    theta <- c()
    oo <- c()  # counts over time
    
    # by time point
    for(jj in seq(end - start + 1) {
      
      if(jj == 1) {
        lambda[jj] <- phi[ii]
        theta[jj] <- 0
        oo[jj] <- initial[ii]
      } else {
        
        lambda[jj] <- exp(coef0[ii] + coef1[ii] * jj) +
          alp[ii] * oo[jj - 1] + bet[ii] * lambda[jj - 1]

        val <- ifelse(oo[jj - 1] == 0, 1, 0)
        theta[jj] <- (val * gam[ii]) + ((1 - val) * eta[ii])

        if(!over) {
          oo[jj] <- rZIP(1, lambda = (toff[jj] + 1) * lambda[jj], 
                         sigma = theta[jj])
        } else {
          # if sampled variable <= theta (p(y=0)), it is 0, 
          # otherwise, sample from neg binomial
          oo[jj] <- ifelse(runif(1, min = 0, max = 1)  <= theta[jj], 0, 
                           rnbinom(1, lambda = (toff[jj] + 1) * lambda[jj], 
                                   size = phi[ii]))
        }
      } 
    }
    oo <- c(rep(0, start - 1), oo) # pad with 0s
    sims[[ii]] <- oo
  }
  sims
}

# posterior simulations
allsim <- mclapply(1:1000, function(ii) {
    posterior.sim(data = data, model = zips, over = FALSE)
} )
save(allsim, file=paste0(dir_model_folder, "post.data"))

rm(allsim, zips, data)
