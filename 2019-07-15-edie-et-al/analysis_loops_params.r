# Settings (predefined)
speeds <- list(
     slow1 = list(iterations = 100000,
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
     fast = list(iterations =  12000,
                 adapt_delta = 0.9,
                 tree_depth = 12,
                 chains = 4))

combinations <- list(list(dataset = "GL", ll = "Y"),  # 1
                         list(dataset = "BG", ll = "Y"),  # 2
                         list(dataset = "BG", ll = "N"),  # 3
                         list(dataset = "LT", ll = "Y"),  # 4
                         list(dataset = "LT", ll = "N"),  # 5
                         list(dataset = "BM", ll = "Y"),  # 6
                         list(dataset = "BN", ll = "N"))  # 7

create_model_params <- function(chosen_index, chosen_speed, chosen_effort) {
     combination <- append(combinations[[chosen_index]], speeds[[chosen_speed]])
     combination <- append(combination, list(te=chosen_effort))
     combination
}

create_model_params_combi <- function(chosen_speeds, chosen_indices, chosen_efforts) {

     model_param_list <- list()

     speed_len <- length(chosen_speeds)
     indices_len <- length(chosen_indices)
     effort_len <- length(chosen_efforts)

     counter = 1
     for (j in 1:speed_len) {
          for (i in 1:indices_len) {
               for(k in 1:effort_len) {
                    model_param_list[[counter]] <- 
                         create_model_params(chosen_indices[i], chosen_speeds[j], chosen_efforts[k])
                    counter = counter + 1
               }
          }
     }
     model_param_list
}