# Set up
#############
source('2019-07-15-edie-et-al/init_a.r') # init

# Parameters
#############
# chosen_speeds <- c('fast', 'slow1', 'slow3')
# chosen_indices <- c(2, 3)
# chosen_speeds <- c('fast')
# chosen_indices <- c(3)
chosen_speeds <- c('fast') 
chosen_indices <- c(6, 5, 4, 3, 2, 1) # 6 options

# Settings (predefined)
speeds <- list(slow1 = list(iterations = 100000,
                            adapt_delta = 0.999,
                            tree_depth = 15,
                            chains = 4),
               slow2 = list(iterations =  200000, 
                            adapt_delta = 0.999,
                            tree_depth = 15,
                            chains = 4),
               slow3 = list(iterations =  300000, 
                            adapt_delta = 0.999,
                            tree_depth = 15,
                            chains = 4),
               fast = list(iterations =  8000, 
                           adapt_delta = 0.8,
                           tree_depth = 12,
                           chains = 4))

model_params_combinations <- function(chosen_speed) {
    list(
        list(dataset = "GL", 
                ll = "Y",       
                chains = speeds[[chosen_speed]]$chains,     
                iter = speeds[[chosen_speed]]$iterations,  
                ad = speeds[[chosen_speed]]$adapt_delta,      
                td = speeds[[chosen_speed]]$tree_depth),
        list(dataset = "BG", 
                ll = "Y",       
                chains = speeds[[chosen_speed]]$chains,     
                iter = speeds[[chosen_speed]]$iterations,  
                ad = speeds[[chosen_speed]]$adapt_delta,      
                td = speeds[[chosen_speed]]$tree_depth),
        list(dataset = "BG", 
                ll = "N",       
                chains = speeds[[chosen_speed]]$chains,     
                iter = speeds[[chosen_speed]]$iterations,  
                ad = speeds[[chosen_speed]]$adapt_delta,      
                td = speeds[[chosen_speed]]$tree_depth),
        list(dataset = "LT", 
                ll = "Y",       
                chains = speeds[[chosen_speed]]$chains,     
                iter = speeds[[chosen_speed]]$iterations,  
                ad = speeds[[chosen_speed]]$adapt_delta,      
                td = speeds[[chosen_speed]]$tree_depth),
        list(dataset = "LT", 
                ll = "N",       
                chains = speeds[[chosen_speed]]$chains,     
                iter = speeds[[chosen_speed]]$iterations,  
                ad = speeds[[chosen_speed]]$adapt_delta,      
                td = speeds[[chosen_speed]]$tree_depth),
        list(dataset = "BM", 
                ll = "Y",       
                chains = speeds[[chosen_speed]]$chains,     
                iter = speeds[[chosen_speed]]$iterations,  
                ad = speeds[[chosen_speed]]$adapt_delta,      
                td = speeds[[chosen_speed]]$tree_depth)
    )
} 

run_edie_analysis_loop <- function() {

    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    speed_len <- length(chosen_speeds); param_len <- length(chosen_indices)
    print(paste0(Sys.time(), " --- Start modelling loop for ", speed_len, " x ", 
                 param_len, " = ", speed_len*param_len, " combinations."))
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")

    for (j in 1:speed_len) {
        for (i in 1:param_len) {
            chosen_index <- chosen_indices[i]; chosen_speed <- chosen_speeds[j]
            print("#############################################################")
            print("#############################################################")
            print("#############################################################")
            print(paste0(Sys.time(), " --- Modelling for chosen_index = ", chosen_index, 
                        "; speed = ", chosen_speed))
            model_params <- model_params_combinations(chosen_speed)[[chosen_index]]
            # print(model_params)
            tryCatch({
                source(paste0(dir_script_ed, 'analysis.r'))
            }, error=function(e) {print(paste0("ERROR: ", conditionMessage(e)))})
        }
    }

}

run_edie_analysis_loop()
