# Set up
source('2019-06-19-jsa-type-ch1/init.r')

# Libraries
library(zoo)
library(ggrepel)

# Parameters
theme <- theme_minimal(base_size=7)
year_end <- 2018
dir_plot <- "C:\\Users\\ejysoh\\Dropbox\\msc-thesis\\research\\_figures\\_ch1\\"

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 1
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/fig-1.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 2
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/fig-2.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 3
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/fig-3.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 4  Prop species describing <=N species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/fig-4.r")


#########################################################################################
# Supporting Information
#########################################################################################

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2A Table 1 - Paragraph on taxonomic effort
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/si-A.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2B Fig - Histogram of active years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/si-B-fig-1.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2C Fig 1
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/si-C-fig-1.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2D Table 1
# Describers profile - one large monograph towards end of life, or many small?
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/si-D-table-1.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2E Fig 2,3
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/si-E-fig-2,3.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2F Tables - Ancillary info on describers
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2019-06-19-jsa-type-ch1/plots_main/si-F-tables.r")

