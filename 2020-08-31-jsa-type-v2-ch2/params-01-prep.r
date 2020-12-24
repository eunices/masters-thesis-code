# These are parameters to be fed into prep.r and model.r

# Parameters
#############

# Model parameters
model_folder_names <- c("BGY-E0-C4-I8000-A0.8-T12-F25-V0") # fast run

# model_folder_names <- c(
#     "BGY-E0-C4-I8000-A0.8-T12-F50-V50",
#     "BGY-E1-C4-I8000-A0.8-T12-F50-V50",
#     "BGY-E2-C4-I8000-A0.8-T12-F50-V50",

#     "BGY-E0-C4-I8000-A0.8-T12-F25-V0",
#     "BGY-E1-C4-I8000-A0.8-T12-F25-V0",
#     "BGY-E2-C4-I8000-A0.8-T12-F25-V0",

#     "BMY-E0-C4-I8000-A0.8-T12-F25-V0",
#     "FAM-E0-C4-I8000-A0.8-T12-F25-V0",
#     "HAL-E0-C4-I8000-A0.8-T12-F25-V0",
#     "LTY-E0-C4-I8000-A0.8-T12-F25-V0",

#     "BMY-E1-C4-I8000-A0.8-T12-F25-V0",
#     "FAM-E1-C4-I8000-A0.8-T12-F25-V0",
#     "HAL-E1-C4-I8000-A0.8-T12-F25-V0",
#     "LTY-E1-C4-I8000-A0.8-T12-F25-V0",

#     "BMY-E2-C4-I8000-A0.8-T12-F25-V0",
#     "FAM-E2-C4-I8000-A0.8-T12-F25-V0",
#     "HAL-E2-C4-I8000-A0.8-T12-F25-V0",
#     "LTY-E2-C4-I8000-A0.8-T12-F25-V0"
# )


# # Hyperparameters of models where chains could mix
# model_folder_names <- 
#   c("BGN-E1-C4-I300000-A0.999-T15",
#     "BGY-E0-C4-I8000-A0.8-T12",
#     "BGY-E1-C4-I300000-A0.999-T15",
#     "FAM-E0-C4-I20000-A0.99-T12")

# # Hyperparameters of models that were "almost there"
# model_folder_names <- 
#   c("BGN-E1-C4-I8000-A0.9-T12",
#     "BMY-E0-C4-I8000-A0.95-T12",
#     "BNN-E0-C4-I8000-A0.95-T12",
#     "BNN-E1-C4-I8000-A0.95-T12")

################################################################################
################################################################################
# model_params        <dataset><ll>-E<te>-C<chains>-I<iter>-A<ad>-T<td>

#     dataset         # BG = biogeographic realms,  GL = global, BM = biomes, 
                      # LT = latitude-trop/not
#     te              # taxonomic effort 0=no taxonomic effort, 
                      # 1=publication taxonomic effort
#     ll              # whether using lat lon data (Y) or 
                      # global.distribution data (N)
#     chains          # stan's number of chains
#     iter            # stan's number of iterations
#     ad              # stan's adapt_delta
#     td              # stan's max tree depth

# note: GL and BM always Y; the rest can be either Y or N
################################################################################
################################################################################