pfunc <- function(t, r, c) {
    exp(0.5*r*t) / (2*exp(0.5*r*t) + c)
}

# li <- sapply(1:100, function(x) pfunc(c(x, 1, 1)))
# plot(1:100, li)
# li <- sapply(1:100, function(x) pfunc(c(x, 0.1, 1)))
# plot(1:100, li)

pfunc.deriv <- function(p, r) r*p*(0.5-p) 

# log likelihood of data given the parameters, assuming binomial distribution
# returns the negative log-likelihood to be minimised with optim()
find.ll<-function(data, par) {
  suppressWarnings(with(data, 
    -1 * sum(dbinom(x = nFemales, size = n, prob = pfunc(t = date, r = par[1], c = par[2]), log = TRUE)))) }

# Internal function to perform 1D optimisation and find the parameters r and c that optimises the log likelihood
# for the focal set of data. Arbitrarily chosen starting values
run.optimiser <- function(data) optim(par = c(0.1, 1), find.ll, data = data)



##############################

find.response.variables <- function(data) {
  # This function runs the optimiser on a focal bit of resampled data, and gets the current gender ratio, 
  # rate of change, and the year at which the ratio got within 5% of gender parity using the pfunc model
  # with optimised parameters
  optim.results <- run.optimiser(data)
  r <- optim.results$par[1]
  c <- optim.results$par[2]
  gender.ratio.at.present <- pfunc(123, r, c)
  # Compute the predicted gender ratio at the "present", i.e. the day I collected the data, 20/8/16
  current.rate.of.change <- pfunc.deriv(p = gender.ratio.at.present, r = r) 
  # Compute the rate of change in the gender ratio at the "present"
  year.range <- 0:9999
  predicted.gender.ratio <- pfunc(year.range, r, c)   
  # Compute the predicted gender ratio from year 2000 way into the future
  ifelse(predicted.gender.ratio[2] > predicted.gender.ratio[1], rising <- TRUE, rising <- FALSE)  
  # Check if it's rising or falling towards 50:50
  if(rising) {
    if(gender.ratio.at.present < 0.5) parity.year <- year.range[which(predicted.gender.ratio > 0.45)][1]                 # record first year it got within 5% of parity
    else(parity.year <- "Female-biased and becoming more so")
  }
  else {
    if(gender.ratio.at.present > 0.5) parity.year <- year.range[which(predicted.gender.ratio < 0.55)][1]
    else(parity.year <- "Male-biased and becoming more so")
  }
  return(data.frame(gender.ratio.at.present=gender.ratio.at.present, 
                    current.rate.of.change=current.rate.of.change,
                    parity.year=parity.year, r=r, c=c,
                    stringsAsFactors = F))
}