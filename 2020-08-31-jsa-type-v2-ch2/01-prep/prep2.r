# Summarize offsets (publication and number of PTEs) per year
# for each biogeographic realm/ group

source('2020-08-31-jsa-type-v2-ch2/01-prep/init.r')
source('2020-08-31-jsa-type-v2-ch2/01-prep/prep2/describers.r')
source('2020-08-31-jsa-type-v2-ch2/01-prep/prep2/publications.r')

print(paste0(Sys.time(), " --- make dataset for offset"))

# Read data
input_filepath <- paste0(dir_model_folder, "data.csv")
data <- read_escaped_data_v2(input_filepath)

# Count offset

# By publication
if (model_params$te <= 1) {

    # Get publications
    n_pub <- get_yearly_publications(data)
    n_pub <- n_pub[year <= cutoff_ch2]

    write.csv(
        n_pub, paste0(dir_model_folder, "offset.csv"), 
        row.names = F, na = "", fileEncoding = "UTF-8"
    )

}

# By number of PTEs
if (model_params$te == 2) {
    
    n_des <- get_yearly_describers(data)
    n_des <- n_des[year <= cutoff_ch2]

    fwrite(n_des, paste0(dir_model_folder, "offset.csv"), na = "")

}

# ggplot(n_des) + 
#     geom_line(aes(x=years, y=N_des)) +
#     facet_grid(rows = vars(group))
































