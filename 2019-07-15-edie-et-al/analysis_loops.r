# Init script
source('2019-07-15-edie-et-al/init_a.r')

# Parameters
chosen_speeds <- c('fast', 'slow1', 'slow3')
chosen_indices <- c(2, 3)

# Settings (predefined)
speeds <- list(slow1 = list(iterations =  100000, 
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
               fast = list(iterations =  50000, 
                           adapt_delta = 0.99,
                           tree_depth = 12,
                           chains = 4))

model_params_combinations <- list(
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

if (length(chosen_indices) == 1 & length(chosen_speeds) == 1) {
    chosen_index <- chosen_indices[1]
    chosen_speed <- chosen_speeds[1]
    print(paste0("Modelling for chosen_index = ", chosen_index))
    model_params <- model_params_combinations[[chosen_index]]
    source(paste0(dir_script_ed, 'analysis.r'))
} else {
    for (j in 1:length(chosen_speeds)) {
        for (i in 1:length(chosen_indices)) {
            chosen_index <- chosen_indices[i]; chosen_speed <- chosen_speeds[j]
            print(paste0("Modelling for chosen_index = ", chosen_index))
            model_params <- model_params_combinations[[chosen_index]]
            source(paste0(dir_script_ed, 'analysis.r'))
        }
    }

}
