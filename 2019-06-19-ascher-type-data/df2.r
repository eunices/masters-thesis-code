source('2019-06-19-ascher-type-data/init.r')

# Libraries
#############
library(dplyr)
library(tidyr)
library(data.table)

# Parameters
#############

# loop_2 <- 'Y'
loop_2 <- 'N'

# Scripts
#############
source('2019-06-19-ascher-type-data/df2.1.r', local=T)
source('2019-06-19-ascher-type-data/df2.2.r', local=T)
source('2019-06-19-ascher-type-data/df2.3.r', local=T)
source('2019-06-19-ascher-type-data/df2.4.r', local=T)
source('2019-06-19-ascher-type-data/df2.5.r', local=T)
source('2019-06-19-ascher-type-data/df2.6.r', local=T)

# TODO: create network dataframe; to examine collaborativeness 
# TODO: further cleaning on whether currently alive
# TODO: write generic code to merge differences
