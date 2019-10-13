# Init script
source('2019-07-15-edie-et-al/init_a.r')

# Parameters
chosen_speed <- 'slow'
chosen_indices <- c(3)

# Settings (predefined)
speeds <- list(slow = list(iterations =  100000, 
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

if (length(chosen_indices) == 1) {
    chosen_index <- chosen_indices[1]
    print(paste0("Modelling for chosen_index = ", chosen_index))
    model_params <- model_params_combinations[[chosen_index]]
    source(paste0(dir_script_ed, 'analysis.r'))
} else {
    for (i in 1:length(chosen_indices)) {
        chosen_index <- chosen_indices[i]
        print(paste0("Modelling for chosen_index = ", chosen_index))
        model_params <- model_params_combinations[[chosen_index]]
        source(paste0(dir_script_ed, 'analysis.r'))
    }

}
