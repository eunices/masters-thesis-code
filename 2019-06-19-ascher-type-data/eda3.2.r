
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - load data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- load data"))

source('2019-06-19-ascher-type-data/eda3_util.r') # util functions


# Read/initialize all data/variables
CURRENT_YEAR <- 2018
theme <- theme_minimal()

# Plausible UN data
un_path <- "data/2019-11-11-un-indicators/"
df_r <- fread(paste0(un_path, '2019-11-12-indicators.csv'), encoding="UTF-8", stringsAsFactors=F, na=c(""))
names_df_r <- df_r[1,]; df_r <- df_r[-1,]
names(df_r) <- unlist(names_df_r, use.names=FALSE)

# Lookup table for countries
lu <- fread('data/lookup/2019-05-29-statoid-country-codes.csv',  encoding="UTF-8")


# For species

# Denormalised authors
filepath <- '2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv'
dat <- fread(paste0(dir_data, filepath), encoding="UTF-8", stringsAsFactors=F, na=c(""))
cols <- c('idxes', 'full.name.of.describer.n', 'idxes_author.order', 'date.n'); dat <- dat[,..cols]

# Author info
auth <- get_des(write=F)
auth <- auth[, c('full.name.of.describer.n', 'residence.country.describer.n', 'describer.gender.n',
                 "min", "max_corrected")]
auth$residence.country.describer.n <- sapply(auth$residence.country.describer.n, function(x) strsplit(x, "; ")[[1]][1])
auth <- merge(auth, lu[, c("DL", "Country")], all.x=T, all.y=F, 
              by.x="residence.country.describer.n", by.y="DL")
auth$residence.country.describer.n <- NULL

print(table(dat$idxes_author.order))

# Merge dataframes
dat <- merge(dat, auth[, c("full.name.of.describer.n", "describer.gender.n", "Country")],
             all.x=T, all.y=F, by="full.name.of.describer.n")

# For taxonomist
seq <- mapply(function(a, b) {
    seq(a, b)
}, a=auth$min, b=auth$max_corrected)
auth$years <- seq
auth_years <- data.table(auth %>% unnest(years)); auth$years <- NULL


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - functions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- functions"))

generate_prop_t <- function(country="All", position="All") { 
    
    # Filtering for each country
    if (country != "All") {
        dat <- dat[Country == country]
    } 

    # Filtering for position: "first"
    if (position == "First") {
        dat <- dat[idxes_author.order %in% c("1")]
    } else if (position == "Last") {
        dat <- dat[idxes_author.order %in% c("L")]
    } else if (position == "First_s") {
        dat <- dat[idxes_author.order %in% c("1", "S")]
    } else if (position == "Last_s") {
        dat <- dat[idxes_author.order %in% c("S", "L")]
    }

    # count N by gender
    prop <- dat[, .N, by=c("date.n", "describer.gender.n")]
    prop <- dcast(prop, date.n ~ describer.gender.n, value.var="N")

    # ensure no blank years
    prop <- merge(prop, data.frame(date.n=min(prop$date.n):max(prop$date.n)),
                  all.x=T, all.y=T, by="date.n")
    prop[is.na(prop$F)]$F <- 0
    prop[is.na(prop$M)]$M <- 0
    prop[is.na(prop$U)]$U <- 0

    # tabulate proportion
    prop$prop_F <- ifelse(prop$M + prop$F == 0, 0, prop$F / (prop$M + prop$F))
    prop$N <- prop$F + prop$M

    if (any(names(prop) %in% "F")) {
        # filter data to year with at least 1 female
        first_year_female <- min(prop[F>0]$date, na.rm=T)
        prop <- prop[date.n>=first_year_female]
        prop$date <- prop$date.n - first_year_female
        # prop$date  <- prop$date.n - min(prop$date.n) # no need for at least 1 female

        # rename data
        names(prop)[which(names(prop) == 'F')] <- "nFemales"
        names(prop)[which(names(prop) == 'M')] <- "nMales"
        names(prop)[which(names(prop) == 'N')] <- "n"
        
        # include more than 0 to fit graph
        return(prop[n>0])

        # output looks like this
        #     date.n nFemales nMales U prop_F  n date
        # 1:   1996        1      1 0  0.500  2    0
        # 2:   1997        0      5 0  0.000  5    1
        # 3:   1998        1      0 0  1.000  1    2
        # 4:   1999        0      8 0  0.000  8    3
        # 5:   2000        0      8 0  0.000  8    4

    } else {
        print("No female taxonomist in dataset.")
        return(NULL)
    }

}

generate_prop_t_tax <- function(country="All") {

    # Filter data by country
    if (country != "All") {
        auth_years <- auth_years[Country == country]
    }
    
    # Get male female taxonomist ratio
    prop <- auth_years[, list(N = length(full.name.of.describer.n)), by=c("years", "describer.gender.n")]
    prop <- dcast(prop, years ~ describer.gender.n, value.var="N")
    prop <- data.table(merge(data.frame(years=min(prop$years):max(prop$years)), prop, by="years"))
    prop[is.na(prop)] <-  0
    prop <- prop[years <= 2018,]
    prop$N <- prop$F + prop$M
    prop$prop_F <- prop$F / prop$N

    if (any(names(prop) %in% "F")) {
        first_year_female <- min(prop[F > 0,]$year)
        prop <- prop[years >= first_year_female,]
        prop$date <- prop$years - first_year_female

        prop$F <- as.integer(prop$F)
        prop$M <- as.integer(prop$M)
        prop$N <- as.integer(prop$N)

        names(prop)[which(names(prop) == 'F')] <- "nFemales"
        names(prop)[which(names(prop) == 'M')] <- "nMales"
        names(prop)[which(names(prop) == 'N')] <- "n"
        names(prop)[which(names(prop) == 'years')] <- "date.n"

        # include more than 0 to fit graph
        return(prop[n>0])

        # output looks like this
        #     date.n nFemales nMales U  n    prop_F date
        # 1:   1996        1      5 0  6 0.1666667    0
        # 2:   1997        1      7 0  8 0.1250000    1
        # 3:   1998        2      7 0  9 0.2222222    2
        # 4:   1999        2      7 0  9 0.2222222    3

    } else {
        print("No female taxonomist in dataset.")
        return(NULL)
    }

}

# country = "All"
# position = "All"
main <- function(country="All", position="All", prop) {

    print(paste0(Sys.time(), ": Optimizing for graphing purposes"))

    # Optimize c and t based on data
    res <- find.response.variables(prop)

    print(paste0(Sys.time(), ": Resampling data"))
    # For generating confidence intervals
    # reshape data for sampling
    prop_p <- melt(prop, id.vars=c("date"), measure.vars=c("nFemales", "nMales"))
    names(prop_p) <- c("date", "gender", "count")

    # get row index for resampling
    n.authors <- sum(prop_p$count); probabilities <- prop_p$count / n.authors; replicates <- 1000
    resampled.data <- sample(1:nrow(prop_p), size = n.authors * replicates,
                            replace = T, prob = probabilities)

    # get frequency (=count)
    resampled.data <- table(factor(resampled.data, levels = 1:nrow(prop_p)),
                            rep(1:replicates, each = n.authors)) %>% data.table
    names(resampled.data)[which(names(resampled.data)=="V1")] = "row"
    names(resampled.data)[which(names(resampled.data)=="V2")] = "replicate_id"
    names(resampled.data)[which(names(resampled.data)=="N")] = "count"
    resampled.data$row <- as.numeric(resampled.data$row)

    # get resampled rows with associated "information" (date and gender)
    resampled.data <- data.frame(date = as.numeric(prop_p$date[resampled.data$row]),
                                gender = prop_p$gender[resampled.data$row],
                                replicate_id = as.numeric(resampled.data$replicate_id),
                                count = as.numeric(resampled.data$count)) %>% data.table

    # summarize resampled data
    resampled.data <- dcast(resampled.data, date + replicate_id ~ gender, value.var="count")
    resampled.data <- resampled.data[, list(nFemales = sum(nFemales),
                                            nMales = sum(nMales)),
                                    by=c("replicate_id", "date")]
    resampled.data$n <- resampled.data$nFemales + resampled.data$nMales

    print(paste0(Sys.time(), ": Generating confidence intervals"))
    # get response variables from each replicate
    bootstrap_estimates <- rbindlist(lapply(1:replicates, function(i) {
        find.response.variables(resampled.data[resampled.data$replicate_id == i,])
    }))

    check_na <- function(x) {
        table(is.na(x))        
    }

    # calculate confidence intervals
    confi_95.1 <- 0.025; confi_95.2 <- 0.975
    CIs_ratio <- as.numeric(quantile(bootstrap_estimates$gender.ratio.at.present, probs = c(confi_95.1 , confi_95.2)))
    CIs_dt <- as.numeric(quantile(bootstrap_estimates$current.rate.of.change, probs = c(confi_95.1, confi_95.2)))

    # tabulate parity year
    baseline_yr <- min(prop$date.n)
    if(is.numeric(bootstrap_estimates$parity.year)) {
        parity.years <- as.numeric(bootstrap_estimates$parity.year) # may generate some NAs
        parity.year <- median(parity.years, na.rm=T) + baseline_yr - CURRENT_YEAR # ignore text with na.rm=T
        CIs_yr <- as.numeric(quantile(parity.years, probs = c(confi_95.1, confi_95.2)))
        CIs_yr <- median(parity.years) + baseline_yr - CURRENT_YEAR
        parity.year[parity.year < 0] <- 0; CIs_yr[CIs_yr < 0] <- 0
    } else {
        # if not going to hit parity sometimes, pick the mode non-parity outcome
        parity.year <- Mode(bootstrap_estimates$parity.year)
        CIs_yr <- c(NA, NA)
    }


    summary <- data.frame(country = country, position = position, n.authors = n.authors,

                           gender.ratio.at.present = median(bootstrap_estimates$gender.ratio.at.present), 
                           low.CI.1 = CIs_ratio[1], up.CI.1 = CIs_ratio[2],
       
                           current.rate.of.change = median(bootstrap_estimates$current.rate.of.change), 
                           low.CI.2 = CIs_dt[1], up.CI.2 = CIs_dt[2],
       
                           years.to.parity = parity.year, 
                           low.CI.3 = CIs_yr[1], up.CI.3 = CIs_yr[2], 
       
                           r=res$r, c=res$c, stringsAsFactors = F)
    return(list(summary=summary, bootstrap_estimates=bootstrap_estimates))
}

save_graph <- function(dir_output, country, position, prop, r, c, parity.year) {

    print(paste0(Sys.time(), ": Plotting data"))
    
    baseline_yr <- min(prop$date.n)

    # determine y-axis max value
    max_predict_year <- as.integer(round((max(prop$date.n) - min(prop$date.n)) * 1.75, 0))
    if (is.numeric(parity.year)) {
        parity.year <- parity.year + CURRENT_YEAR - baseline_yr # normalise to baseline
        max_predict_year <- ifelse(parity.year > max_predict_year,
                                   parity.year, max_predict_year)
    }

    # get confidence intervals
    cols <- c('date.n', 'prop_F', 'n', 'nFemales', 'nMales')
    prop_overlay <- cbind(prop, get.CIs(prop$nFemales, prop$nMales)) # TODO: how CI works [0,0 not allowed]

    # merge to template (there will be NAs)
    prop_template <- data.frame(x = 0:max_predict_year + baseline_yr,
                                y = sapply(0:max_predict_year, pfunc, r=r, c=c))
    prop_overlay <- merge(prop_template, prop_overlay, by.x="x", by.y="date.n", all.x=T, all.y=F)
    prop_overlay$y <- round(prop_overlay$y*100, 5); prop_overlay$prop_F <- round(prop_overlay$prop_F*100, 5) 

    y_axis_title <- ifelse(country=="All",
        paste0("Proportion of female-authored species (%),\n", tolower(position), " authors \n"), 
        paste0("Proportion of female-authored species (%),\n", tolower(position), " authors for ", country, "\n"))
    p1 <- ggplot() + 
        # geom_errorbar(data = prop_overlay, colour = "darkgrey", width = 0,
        #               aes(x = date.n, ymin = lowerCI, ymax = upperCI)) +
        geom_point(data = prop_overlay, aes(x = x, y = prop_F)) +                 # real data
        geom_line(data = prop_overlay, aes(x = x, y = y)) +                       # model predicted data
        geom_hline(yintercept= 45, linetype="dotted", size=0.8, color="red")  +   # thresholds
        geom_hline(yintercept= 55, linetype="dotted", size=0.8, color="red")  +
        xlab("\nYear") + ylab(y_axis_title) + ylim(c(0,100)) + theme
    
    if (is.numeric(parity.year)) {
        p1 <- p1 + geom_vline(xintercept= parity.year + baseline_yr, linetype="dashed", size=1.5, color="black")
    }

    plot_filepath <- paste0(dir_output, country, "-", position, ".png")
    ggsave(plot_filepath, plot=p1, width = 10, height = 7, dpi = 150, units = "in", device='png')
    
}

run_specific_scenario <- function(country="All", position="All", dir_output, type="pub") {

    print("#################################")
    print(paste0("Running for '", country, "' at position '", position, "' (", type, ")."))

    tryCatch ({
        if (type=="pub") {        # for publication
            prop_t  <- generate_prop_t(country=country, position=position)
        } else if (type=="tax") { # for taxonomists
            prop_t <- generate_prop_t_tax(country=country)
        }
        if (!is.null(prop_t)) {
            output <- main(country = country, position = position, prop_t)
            save_graph(dir_output, country=country, position=position, prop_t, 
                       output$summary$r, output$summary$c, output$summary$years.to.parity)
            output$summary
        }
    }, error = function(e) {print(e)})
}

