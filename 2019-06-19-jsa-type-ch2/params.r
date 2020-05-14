
# Parameters
#############

# Model parameters
model_folder_names <- c("GLY-E0-C4-I8000-A0.8-T12") # fast run
# model_folder_names <- c("BMY-E0-C4-I20000-A0.999-T15",
#                         "GEN-E0-C4-I20000-A0.999-T15",
                        
#                         "BGY-E1-C4-I300000-A0.999-T15", # should take 9 days
                        
#                         "GLY-E0-C4-I300000-A0.99-T15",
#                         "BMY-E0-C4-I300000-A0.999-T15",
#                         "GEN-E0-C4-I300000-A0.999-T15",
#                         "BNN-E0-C4-I300000-A0.999-T15",
#                         "GLY-E0-C4-I300000-A0.999-T15")

# model_params        <dataset><ll>-E<te>-C<chains>-I<iter>-A<ad>-T<td>
#     dataset         # BG = biogeographic realms,  GL = global, BM = biomes, LT = latitude-trop/not
#     te              # taxonomic effort 0=no taxonomic effort, 1=publication taxonomic effort
#     ll              # whether using lat lon data (Y) or global.distribution data (N)
#     chains          # stan's number of chains
#     iter            # stan's number of iterations
#     ad              # stan's adapt_delta
#     td              # stan's max tree depth
# note: GL and BM always Y; the rest can be either Y or N
