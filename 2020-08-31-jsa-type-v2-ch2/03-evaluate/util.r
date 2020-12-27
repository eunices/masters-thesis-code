get_loo <- function(model) {

    # model refers to the model identifier

    model_params <- parse_model_identifier(model)
    model_dirs <- initialize_model_params(model_params)
    model_dir <- model_dirs[1]

    # load zero inflated fits
    load(paste0(model_dir, "fit.data"))       # as "fit"

    m_log_lik <- extract_log_lik(
        fit, 
        parameter_name = "log_lik", 
        merge_chains = FALSE
    )

    m_r_eff <- relative_eff(exp(m_log_lik), cores = 2)

    m_loo <- loo(m_log_lik, r_eff = m_r_eff, cores = 2)
    m_loo
}


list_to_df <- function(x, i) {
    x <- setDT(tstrsplit(as.character(x), ", ", fixed=TRUE))[]
    names(x) <- paste0("year_", 1:dim(x)[2])
    
    x[] <- lapply(x, function(x) {
        x <- gsub("c\\(|\\)", "", x)
        as.numeric(x)
    })

    x$group <- 1:dim(x)[1]
    x$sim <- i
    x
}


get_sampling_info <- function(chosen_model) {
    model_params <- parse_model_identifier(chosen_model)
    dir_model_folder <- initialize_model_params(model_params)[1]

    file <- paste0(dir_model_folder, "output/chain_sampling.txt")
    chain_sampling <- readLines(file)
    chain_sampling[1]

}
