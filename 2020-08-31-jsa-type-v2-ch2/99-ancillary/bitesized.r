source('2020-08-31-jsa-type-v2-ch2/02-model/init.r')

model_analyse <- "BGY-E0-C4-I8000-A0.8-T12-F25-V0"
model_predict <- "BGY-E1-C4-I20000-A0.9-T12-F10-V0"


# Get graph from model_predict
dir_model_folder <- paste0(dir_analysis_edie_model, model_predict, "/")

# Load posterior simulation
load(paste0(dir_model_folder, "post.data"))     # as "allsim"

# Load forecast predictions
load(paste0(dir_model_folder, "forecast.data")) # as "forecast"

# Original data
data_raw <- read.csv(
    paste0(dir_model_folder, "data.csv"), 
    na.strings=c(""),
    stringsAsFactor = TRUE
) 

# R data
data <- read_rdump(paste0(dir_model_folder, "count_info.data.R")) 


# Map model indices to original variables
mapping <- unique(data.frame(
    groupname = as.character(data_raw$group),
    group = as.numeric(data_raw$group)
))

li_df <- summarize_simulations_observed(data, allsim)
Z <- li_df$Z                      # Counts for each year for sim and actual, df

Y <- data.table(Z)
sim_median <- Y[sim != 0, list(
    cml_value_pred_median = as.integer(median(cml_value)),
    cml_value_pred_lwrCI95 = as.integer(quantile(cml_value, 0.025)),
    cml_value_pred_uprCI95 = as.integer(quantile(cml_value, 0.925)),
    cml_value_pred_mean = as.integer(mean(cml_value))
), by=c("year", "group")]
obs <- Y[sim == 0, c("year", "group", "cml_value")]
names(obs)[3] <- "cml_value_obs"


# sanity check
check <- data.table(data_raw)[, .N, by=c("group", "year")][order(group, year)]
check[, cml:= cumsum(N), by="group"]
head(check[group=="NA"], 20)

all <- merge(obs, sim_median, by=c("year", "group"))
all <- merge(all, mapping, by="group", all.x=T, all.y=F)
all$group <- NULL
all$value <- NULL

wfile <- paste0(v2_dir_data_webapp, "ch2-fig-01-data.csv")
fwrite(all, wfile, na="")


# show the mean!






# Get results.csv from model_analyse
dir_model_folder <- paste0(dir_analysis_edie_model, model_analyse, "/")
rfile <- paste0(dir_model_folder, "output/results.csv")
results <- fread(rfile)

results$expected_median <- NULL
results$expected_CI_lower <- NULL
results$expected_CI_upper <- NULL
results$fore_mu <- NULL
results$fore_lower <- NULL
results$fore_upper <- NULL
results$group <- NULL

dir_model_folder <- paste0(dir_analysis_edie_model, model_predict, "/")
rfile <- paste0(dir_model_folder, "output/results.csv")
results_pred <- fread(rfile)

results_pred$expected_median <- NULL
results_pred$expected_CI_lower <- NULL
results_pred$expected_CI_higher<- NULL
results_pred$slowdown<- NULL
results_pred$slowdown_CI_lower<- NULL
results_pred$slowdown_CI_higher<- NULL
results_pred$group <- NULL

results <- merge(results, results_pred, on="groupname")

names(results)[grep("fore_", names(results))] <- 
    paste0(names(results)[grep("fore_", names(results))], "_10y")

wfile <- paste0(v2_dir_data_webapp, "ch2-fig-02-data.csv")
fwrite(results, wfile, na="")


