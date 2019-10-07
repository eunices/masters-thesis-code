# Libraries
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(sf))
suppressMessages(library(anytime))
suppressMessages(library(ggmap))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

# Variables
wd <- 'C:/Dev/msc-thesis-code'
dir_data <- 'data/2019-05-23-ascher-bee-data/'
dir_script <- '2019-06-19-ascher-type-data/'
source('keys.R')
source(paste0(dir_script, 'helper.R'))


# Initialize google api for geocoding
register_google(key = geocode_api)
