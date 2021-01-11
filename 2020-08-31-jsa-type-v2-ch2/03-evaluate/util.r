get_loo <- function(model) {

    # model refers to the model identifier
    model_dir <- paste0(dir_analysis_edie_model, model, "/")

    # load zero inflated fits
    load(paste0(model_dir, "fit.data"))       # as "fit"

    # m_log_lik <- extract_log_lik(
    #     fit, 
    #     parameter_name = "log_lik", 
    #     merge_chains = FALSE
    # )

    # manual/ alternative code to `extract_log_lik`
    m_log_lik <- as.array(fit, pars = "log_lik")
    m_log_lik <- m_log_lik[,,apply(m_log_lik, 3, sd) != 0]

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

get_model_dir <- function(chosen_model) {
    paste0(dir_analysis_edie_model, chosen_model, "/")
}

get_sampling_info <- function(chosen_model) {
    dir_model <- get_model_dir(chosen_model)
    file <- paste0(dir_model, "output/chain_sampling.txt")
    chain_sampling <- readLines(file)
    chain_sampling[1]
}

get_duration_info <- function(chosen_model) {
    dir_model <- get_model_dir(chosen_model)
    file <- paste0(dir_model, "model.log")
    chain_sampling <- readLines(file)
    str <- "Model time elapsed: "
    duration <- chain_sampling[grepl(str, chain_sampling)]
    duration <- duration[length(duration)]
    gsub(str, "", duration)
}


