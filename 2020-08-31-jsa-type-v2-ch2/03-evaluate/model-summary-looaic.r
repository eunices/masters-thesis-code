# Compare LOOAIC for different groups

source('2020-08-31-jsa-type-v2-ch2/00-init/init-e.r')

chosen_models <- c(
    "BMY-E0-C4-I8000-A0.8-T12-F25-V0",
    "BGY-E0-C4-I8000-A0.8-T12-F25-V0",
    "FAM-E0-C4-I8000-A0.8-T12-F25-V0"
)

model1 <- chosen_models[1]
model1_loo <- get_loo(model1)

print(model1_loo)

model2 <- chosen_models[2]
model2_loo <- get_loo(model2)

print(model2_loo)

model3 <- chosen_models[3]
model3_loo <- get_loo(model3)

print(model3_loo)

