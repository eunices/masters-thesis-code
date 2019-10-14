source('2019-07-15-edie-et-al/init_a.r')
print(paste0(Sys.time(), " --- analysis3.r"))

# run posterior simulation from model fit
library(rstan)
library(gamlss.dist)
library(parallel)
library(shinystan)

set.seed(420)

# initial data
files <- dir(dir_model_folder, pattern = 'count_info.data.R',
             full.names = TRUE)
data <- read_rdump(files)

# zero inflated fits
load(paste0(dir_model_folder, "fit.data")) # loads as fit
zips <- fit # reassign to zips
rm(fit) # remove from memory

# Diagnostic plots
# launch_shinystan(zips)
# n <- 30; n_plots <- ceiling(length(names(zips))/n)
# for (i in 1:1) {
#     par(ask=T)
#     start = 1 + (i-1)*n; end = ifelse(i==n_plots, length(names(zips)), start + 29)
#     tp <- traceplot(zips, pars=names(zips)[start:end])
#     tp
# }; par(ask=F)
# traceplot(zips, pars=names(zips))

# posterior predictive simulations for checking model fit
posterior.sim <- function(data, model, over = FALSE) {
  outs <- extract(model, permuted = TRUE) # posterior
  p <- data$P # number of groups
  
  # regression
  coef0 <- apply(outs$coef[, , 1], 2, function(x) sample(x, 1))
  coef1 <- apply(outs$coef[, , 2], 2, function(x) sample(x, 1))

  # acp
  alp <- apply(outs$alpha, 2, function(x) sample(x, 1))  # for each prov
  bet <- apply(outs$beta, 2, function(x) sample(x, 1))  # for each prov

  # markov
  gam <- apply(outs$gamma, 2, function(x) sample(x, 1))  # for each prov
  eta <- apply(outs$eta, 2, function(x) sample(x, 1))  # for each prov
  
  # t=1
  phi <- apply(outs$phi, 2, function(x) sample(x, 1))  # for each prov

  # initial count
  initial <- sapply(seq(p), function(pp) data$counts[ pp, data$str[pp]])

  if(over) phi <- apply(outs$phi, 2, function(x) sample(x, 1))

  sims <- list() # store simulations
  # by province
  for(ii in seq(p)) {
    toff <- data$off[ii, ][data$str[ii]:data$end[ii]] # offset segment starts at first naming year
    mu <- c()
    theta <- c()
    oo <- c()  # counts over time
    # by time point
    for(jj in seq(data$end[ii] - (data$str[ii] - 1))) {
      if(jj == 1) {
        mu[jj] <- phi[ii]
        theta[jj] <- 0
        oo[jj] <- initial[ii]
      } else {
        mu[jj] <- exp(coef0[ii] + coef1[ii] * jj) +
        alp[ii] * oo[jj - 1] + bet[ii] * mu[jj - 1]

        val <- (oo[jj - 1] == 0) * 1
        theta[jj] <- (val * gam[ii]) + ((1 - val) * eta[ii])

        if(!over) {
          oo[jj] <- rZIP(1, mu = (toff[jj] + 1) * mu[jj], 
                         sigma = theta[jj])
        } else if(over) {
          if(runif(1, min = 0, max = 1) > theta[jj]) {
            oo[jj] <- rnbinom(1, mu = (toff[jj] + 1) * mu[jj], 
                              size = phi[ii])
          } else {
            oo[jj] <- 0
          }

        }
      } 
    }
    oo <- c(rep(0, data$str[ii] - 1), oo)
    sims[[ii]] <- oo
  }
  sims
}

# post sim
allsim <- mclapply(1:1000, function(ii) {
    posterior.sim(data = data, model = zips, over = FALSE)
} )
# allsim <- mclapply(1:1000, mc.cores=4, function(ii) {
#     posterior.sim(data = data, model = zips, over = FALSE)
# } )
save(allsim, file=paste0(dir_model_folder, "post.data"))





