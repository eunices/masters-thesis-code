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


# Used to find the 95% CIs on a proportion (uses default method of the binom.test function). 
# Works for a pair of columns nFemales and nMales
get.CIs <- function(nFemales.column, nMales.column){
  CIs <- data.frame(lower = numeric(length(nFemales.column)), upper = NA)
  for(i in 1:length(nFemales.column)){
    CIs.foc <- as.numeric(binom.test(nFemales.column[i], nFemales.column[i] + nMales.column[i])$conf.int)
    CIs$lowerCI[i] <- CIs.foc[1]
    CIs$upperCI[i] <- CIs.foc[2]
  }
  return(100 * CIs)
}


##############################

find.response.variables <- function(data) {
  # This function runs the optimiser on a focal bit of resampled data, and gets the current gender ratio, 
  # rate of change, and the year at which the ratio got within 5% of gender parity using the pfunc model
  # with optimised parameters
  optim.results <- run.optimiser(data)
  r <- optim.results$par[1]
  c <- optim.results$par[2]

  # Compute the predicted gender ratio at the "present", i.e. the first year with data
  gender.ratio.at.present <- pfunc(max(data$date), r, c)
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


make.resampled.data <- function(real.data, n.authors, chunk.size)  {
  # Function to make "chunk.size" resampled datasets containing n.authors each 
  # this will be done 'n.chunks' times (too big for memory if done in one go)

  # sample idx for each chunk
  resampled.data <- sample(1:nrow(real.data), size = chunk.size * n.authors, replace = T, prob = probabilities)

  # get frequencies of idx and boot replicate
  resampled.data <- table(factor(resampled.data, levels = 1:nrow(real.data)), # row idx, var1
                          rep(1:chunk.size, each = n.authors))                # boot replicate, var2

  # convert frequencies table into dataframe
  resampled.data <- melt(resampled.data) %>% 
      rename(row = Var1, boot.replicate = Var2, count = value) %>% 
      mutate(row = as.numeric(as.character(row)))
  
  # create resampled data using row idx to reference real.data
  resampled.data <- data.frame(date = real.data$date[resampled.data$row], 
                               gender = real.data$gender[resampled.data$row],
                               boot.replicate = resampled.data$boot.replicate,
                               count = resampled.data$count)
  
  # get sum of counts for M and F for each boot.replicate/date combination
  resampled.data <- resampled.data %>% 
    group_by(boot.replicate, date) %>% 
    summarise(nFemales = sum(count[gender == "F"]),
              nMales = sum(count[gender == "M"]),
              n = nFemales+nMales) %>% 
    as.data.frame %>% filter(n > 0)
  
  resampled.data
}

