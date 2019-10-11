print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO ANALYSES SCRIPTS FOR BEE TYPE DATA")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# If running script from elsewhere
# setwd("C:/Dev/msc-thesis-code/")
# source(paste0(dir_script_ed, "analysis.r"))

# Set up
#############
options(scipen = 999)

# Model parameters
#############
model_params <- list(
    dataset = "GL", # BG = biogeographic realms,  GL = global, BM = biomes, LT = latitude (trop or not)
    ll = "Y",       # whether using lat lon data (Y) or global.distribution data (N)
    chains = 4,     # stan's number of chains
    iter = 100000,  # stan's number of iterations
    ad = 0.99,      # stan's adapt_delta
    td = 15         # stan's max tree depth
)   # note: GL always Y; BM always N; the rest can be either Y or N

# Scripts
#############
# Initialize identifier
source('2019-07-15-edie-et-al/init_a.r')
model_identifier <- paste0(
    model_params$dataset, model_params$ll, "-",
    "C", as.character(model_params$chains), "-",
    "I", as.character(model_params$iter), "-",
    "A", as.character(model_params$ad), "-",
    "T", as.character(model_params$td))
dir_model_folder <- paste0(dir_analysis_edie_tmp, "/", model_identifier)
dir.create(dir_model_folder); dir.create(file.path(dir_model_folder, 'output'))
filepath_log <- paste0(dir_model_folder, "/model.log"); file.create(filepath_log)

# Analysis scripts
source(paste0(dir_script_ed, 'analysis0.r')) # data prep
source(paste0(dir_script_ed, 'analysis1.r')) # data prep
source(paste0(dir_script_ed, 'analysis2.r')) # model fitting
source(paste0(dir_script_ed, 'analysis3.r')) # post
source(paste0(dir_script_ed, 'analysis4.r')) # forecast
source(paste0(dir_script_ed, 'analysis5.r')) # plot

# TODO: figure out a way to store params for models
