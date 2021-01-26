# Set up
source('2020-08-31-jsa-type-v2-ch1/init.r')

# Libraries
library(zoo)
library(ggrepel)
library(gridExtra)

library(forecast)
library(tseries)
library(Kendall)
library(segmented)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 1
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/fig-1.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 2
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/fig-2.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 3
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/fig-3.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Fig. 4  Prop species describing <=N species
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/fig-4.r")


#########################################################################################
# Supporting Information
#########################################################################################

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2A Table 1 - Paragraph on taxonomic effort
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/si-A.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2B Fig - Histogram of active years
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/si-B-fig-1.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2C Fig 1
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/si-C-fig-1.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2D Table 1
# Describers profile - one large monograph towards end of life, or many small?
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/si-D-table-1.r")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - SI 2E Tables - Ancillary info on describers
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
source("2020-08-31-jsa-type-v2-ch1/plots_main/si-E-tables.r")
