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

    #' 
    #' 
    #' 
    #'  

    # sample from model posterior
    mp <- sample_model_posterior_parameters(model)
    coef0=mp$coef0; coef1=mp$coef1; alp=mp$alp; bet=mp$bet;
    gam=mp$gam; eta=mp$eta; phi=phi; rm(mp)

    # initial count
    p <- data$P # number of groups
    initial <- sapply(seq(p), function(pp) data$counts[ pp, data$str[pp]])

    # by group
    out <- list()
    for(ii in seq(p)) {
        start <- data$str[ii]
        end <- data$end[ii]

        # offset starts at first naming year
        all_toff <- data$off[ii, ][start:end] 
        # generate offset segment by sampling past decade of offsets
        toff <- sample(all_toff[ (length(all_toff) - ftime ) : length(all_toff) ], 
            ftime, replace=TRUE)

        mu <- c()
        theta <- c()
        oo <- data$counts[ii, end]
        co <- c() # empty expected count vec
        # by time point
        for(jj in 1:ftime) {  # time seq extends the series by ftime years
          if(jj == 1) { # the starting point is the number of species described at end of time series.
            mu[jj] <- oo
            theta[jj] <- 0
            co[jj] <- oo[jj]
          } else { # if not the starting point
            mu[jj] <- exp(coef0[ii] + coef1[ii] * jj) +
            alp[ii] * oo[jj - 1] + bet[ii] * mu[jj - 1]

            val <- (oo[jj - 1] == 0) * 1
            theta[jj] <- (val * gam[ii]) + ((1 - val) * eta[ii])

            co[jj] <- rZIP(1, mu = (toff[jj] + 1) * mu[jj], 
                           sigma = theta[jj])
            oo[jj] <- co[jj] # current count becomes next expected count
          }
        }
        out[[ii]] <- co
    }
    out
}

# simulate the forecast
forecast <- mclapply(1:1000, mc.cores=1, function(ii) {
   post.forecast(data=data, ftime=ftime, model=zips) 
})
save(forecast, file=paste0(dir_model_folder, "forecast.data")); rm(data, zips, forecast)

