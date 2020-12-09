# Compare MAPE for different validation
# of the best model to be used in forecasting

source('2020-08-31-jsa-type-v2-ch2/03-evaluate/init.r')



# Inititalize parameters
model <- c("BGY-E2-C4-I8000-A0.8-T12-F25-V30")
model_params <- parse_model_identifier(model)
filepaths <- initialize_model_params(model_params)

dir_model_folder <- filepaths[1]
filepath_log <- filepaths[2]
warnings_log <- filepaths[3]
model_identifier <- filepaths[4]



# Original data
data_raw <- read.csv(paste0(dir_model_folder, "data.csv"), na.strings=c("")) 

# Map model indices to original variables
mapping <- unique(data.frame(
    groupname = as.character(data_raw$group),
    group = as.numeric(data_raw$group)
))

# Read data
files <- dir(
    dir_model_folder, 
    pattern = 'count_info.data.R',
    full.names = TRUE
)

data <- read_rdump(files)

# Read data
files <- dir(
    dir_model_folder, 
    pattern = 'count_info_ref.data.R',
    full.names = TRUE
)

data_ref <- read_rdump(files)

# Read model
load(paste0(dir_model_folder, "fit.data"))

# Test
posterior_sim(data, fit)
posterior_forecast(data, 5, fit)




# Predict for 5 (and repeat for 10, 15, 20, 25)
posterior_sim(data_ref, fit)





# Using posterior_forecast, predict using naive method
forecast <- mclapply(1:1000, mc.cores = 1, function(ii) {
    posterior_forecast(data, vtime, fit)
})

# Modify from list to data.frame
df_li <- lapply(1:length(forecast), function(i) {

    x <- forecast[[i]]
    x <- setDT(tstrsplit(as.character(x), ", ", fixed=TRUE))[]
    names(x) <- paste0("year_", 1:dim(x)[2])
    
    x$year_1 <- gsub("c\\(", "", x$year_1)
    x$year_5 <- gsub("\\)", "", x$year_5)
    x[] <- lapply(x, as.numeric)

    x$group <- 1:7
    x$sim <- i
    
    x
})

# Wide to long
first_year <- max(data_raw$year) - 25 

df <- rbindlist(df_li)
df <- melt(df, id.vars = c("group", "sim"))
names(df)[which(names(df) == "variable")] <- "year"

df$year <- as.integer(gsub("year_", "", df$year)) + first_year - 1
df$group <- mapping[match(df$group, mapping$group),]$groupname

head(df)

# Summarize by 80 CI
df <- df[,
    list(
        mean = quantile(value, .5),
        qt10 = quantile(value, .1),
        qt90 = quantile(value, .9)
    ),
    by = c("group", "year")
][order(group, year)]


# Merge with actual counts

model_params$fc

df_obs <- data.table(data_raw)[,
    list(obs = .N),
    by = c("year", "group")
]

df <- merge(
    df, df_obs,
    by = c("year", "group")
)


# Calculate MAPE








# Plot chart of MAPE against ftime