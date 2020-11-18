# Compare MAPE for different validation
# of the best model to be used in forecasting

source('2020-08-31-jsa-type-v2-ch2/00-init/init-e.r')
source('2020-08-31-jsa-type-v2-ch2/00-init/util-model.r')

chosen_model <- c("BGY-E2-C4-I8000-A0.8-T12-F25-V25")
model_params <- parse_model_identifier(chosen_model)

# use 1 model

# predict for 5, 10, 15, 20, 25, 30, 35, 40

# predict using naive method

# plot all of these on a chart
