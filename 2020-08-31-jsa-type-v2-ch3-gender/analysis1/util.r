# Information about code:
# This code corresponds to a chapter in my MSc thesis for
# Chapter 3, the section on Gender analysis: utility  functions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# adapted from https://github.com/lukeholman/genderGapCode/

pfunc <- function(t, r, c) exp(0.5*r*t) / (2*exp(0.5*r*t) + c)
pfunc.deriv <- function(p, r) r*p*(0.5-p) 

# log likelihood of data given the parameters, assuming binomial distribution
# returns the negative log-likelihood to be minimised with optim()
find.ll <-function(data, par) {

    sample <- dbinom(
        x=data$nFemales, size=data$n, 
        prob=pfunc(t=data$date, r=par[1], c=par[2]), log=TRUE
    )

    -1 * sum(sample)
}

# Internal function to perform 1D optimisation and find 
# the parameters r and c that optimises the log likelihood
# for the focal set of data. Arbitrarily chosen starting values
run.optimiser <- function(data) optim(par = c(0.1, 1), find.ll, data = data)

# Used to find the 95% CIs on a proportion 
# (uses default method of the binom.test function). 
# Works for a pair of columns nFemales and nMales
get.CIs <- function(nFemales.column, nMales.column){

    CIs <- data.frame(lowerCI = numeric(length(nFemales.column)), upperCI = NA)
    
    for(i in 1:length(nFemales.column)){

        CIs.foc <- as.numeric(binom.test(
            nFemales.column[i],
            nFemales.column[i] + nMales.column[i]
        )$conf.int)

    CIs$lowerCI[i] <- CIs.foc[1]
    CIs$upperCI[i] <- CIs.foc[2]

    }

    return(100 * CIs)

}

Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
} 
##############################

find.response.variables <- function(data) {

    # This function runs the optimiser on a focal bit of resampled data,
    # and gets the current gender ratio, rate of change, and 
    # the year at which the ratio got within 
    # 5% of gender parity using the pfunc model with optimised parameters

    optim.results <- run.optimiser(data)
    r <- optim.results$par[1]
    c <- optim.results$par[2]

    # Compute the predicted gender ratio at the "present", 
    # i.e. the first year with data
    gender.ratio.at.present <- pfunc(max(data$date), r, c)

    # Compute the rate of change in the gender ratio at the "present"
    current.rate.of.change <- pfunc.deriv(p = gender.ratio.at.present, r = r) 

    # Compute parity year
    year.range <- 0:9999
    predicted.gender.ratio <- pfunc(year.range, r, c)

    # check if female ratio increasing
    is_female_ratio_increasing <- ifelse(
        predicted.gender.ratio[2] > predicted.gender.ratio[1], TRUE, FALSE
    )

    if(is_female_ratio_increasing) { # becoming more female biased
    
    parity.year <- ifelse(
        gender.ratio.at.present < 0.5,
        # record first year it got within 5% of parity
        year.range[which(predicted.gender.ratio > 0.45)][1], 
        "Female-biased and becoming more so"
    )

    } else { # becoming more male biased
    
    parity.year <- ifelse(
        gender.ratio.at.present > 0.5, 
        # record first year it got within 5% of parity
        year.range[which(predicted.gender.ratio < 0.55)][1],
        "Male-biased and becoming more so"
    )

    }

    return(data.frame(
        gender.ratio.at.present = gender.ratio.at.present,
        current.rate.of.change = current.rate.of.change,
        parity.year = parity.year, r = r, c = c,
        stringsAsFactors = F)
    )
}

