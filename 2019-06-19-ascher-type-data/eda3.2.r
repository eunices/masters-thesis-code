
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - load data
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- load data"))

source('2019-06-19-ascher-type-data/eda3_util.r') # util functions

theme <- theme_minimal()

# Read/initialize all data

# Plausible UN data
un_path <- "data/2019-11-11-un-indicators/"
df_r <- fread(paste0(un_path, '2019-11-12-indicators.csv'), encoding="UTF-8", stringsAsFactors=F, na=c(""))
names_df_r <- df_r[1,]; df_r <- df_r[-1,]
names(df_r) <- unlist(names_df_r, use.names=FALSE)

# Lookup table for countries
lu <- fread('data/lookup/2019-05-29-statoid-country-codes.csv',  encoding="UTF-8")

# Denormalised authors
filepath <- '2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_4.0-denormalised2.csv'
dat <- fread(paste0(dir_data, filepath), encoding="UTF-8", stringsAsFactors=F, na=c(""))
cols <- c('idxes', 'full.name.of.describer.n', 'idxes_author.order', 'date.n'); dat <- dat[,..cols]

# Author info
auth <- get_des(write=F)
auth <- auth[, c('full.name.of.describer.n', 'residence.country.describer.n', 'describer.gender.n')]
auth$residence.country.describer.n <- sapply(auth$residence.country.describer.n, function(x) strsplit(x, "; ")[[1]][1])
auth <- merge(auth, lu[, c("DL", "Country")], all.x=T, all.y=F, 
              by.x="residence.country.describer.n", by.y="DL")
auth$residence.country.describer.n <- NULL

print(table(dat$idxes_author.order))

# Merge dataframes
dat <- merge(dat, auth[, c("full.name.of.describer.n", "describer.gender.n", "Country")],
             all.x=T, all.y=F, by="full.name.of.describer.n")

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

        return(prop)
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
    # TODO: figure out why parity.year may be NA
    baseline_yr <- min(prop$date.n); current_yr <- 2019
    if(is.numeric(bootstrap_estimates$parity.year)) {
        parity.years <- bootstrap_estimates$parity.year
        parity.year <- median(parity.years) + baseline_yr - current_yr
        CIs_yr <- as.numeric(quantile(parity.years, probs = c(confi_95.1, confi_95.2)))
        CIs_yr <- median(parity.years) + baseline_yr - current_yr
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

save_graph <- function(dir_output, country, position, prop, r, c) {
        # ggplot(df_p) +
    #     geom_bar(stat="identity",  aes(x=x, y=y), fill="grey") +
    #     geom_bar(stat="identity", aes(x=x, y=prop_F), fill = "#FF6666") +
    #     geom_vline(xintercept= (res$parity.year + min(prop$date.n)), linetype="dashed", size=1.5, color="black") +
    #     theme + xlab("Year") + ylab("Proportion of female-authored species, overall (%)\n") + ylim(c(0,50))


    print(paste0(Sys.time(), ": Plotting data"))
    max_predict_year <- as.integer(round((max(prop$date.n) - min(prop$date.n)) * 3/2, 0))
    baseline_yr <- min(prop$date.n)

    df_p <- data.frame(x = 0:max_predict_year + baseline_yr,
                       y = sapply(0:max_predict_year, pfunc, r=r, c=c))

    df_p <- merge(df_p, prop[, c('date.n', 'prop_F')], by.x="x", by.y="date.n", all.x=T, all.y=F)
    df_p$y <- round(df_p$y*100, 5)
    df_p$prop_F <- round(df_p$prop_F*100, 5)

    prop_overlay <- prop[, c("date.n", "nFemales", "nMales", "n", "prop_F")][n>0]

    # TODO: check how CI works here 
    prop_overlay <- cbind(prop_overlay, get.CIs(prop_overlay$nFemales, prop_overlay$nMales))

    y_axis_title <- ifelse(country=="All",
        paste0("Proportion of female-authored species (%),\n", tolower(position), " authors \n"), 
        paste0("Proportion of female-authored species (%),\n", tolower(position), " authors for ", country, "\n"))
    p1 <- ggplot() + 
        geom_errorbar(data = prop_overlay, aes(x = date.n, ymin = lowerCI, ymax = upperCI), 
                    colour = "darkgrey", width = 0) + 
        geom_point(data = df_p, aes(x = x, y = prop_F)) + 
        geom_line(data = df_p, aes(x = x, y = y)) +
        xlab("\nYear") + ylab(y_axis_title) + ylim(c(0,50)) + theme

    plot_filepath <- paste0(dir_output, country, "-", position, ".png")
    ggsave(plot_filepath, plot=p1)
    
}


run_specific_scenario <- function(country="All", position="All", dir_output) {
    tryCatch ({
        prop_t  <- generate_prop_t(country=country, position=position)
        if (!is.null(prop_t)) {
            output <- main(country = country, position = position, prop_t)
            save_graph(dir_output, country=country, position=position, prop_t, output$summary$r, output$summary$c)
            output$summary
        }
    }, error = function(e) {print(e)})
}