# Modified from https://gist.github.com/andrewheiss/926b9d60a26e29f6bf32

# Load libraries
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(spdep)
library(data.table)

# # Load shapefiles
# countries <- readOGR('data/geo/1_separate/gadm/shp_all_levels/gadm36_0.shp')
countries <- readOGR('data/geo/ne_50m_admin_0_countries_lakes/ne_50m_admin_0_countries_lakes.shp')

# Extract the ISO codes and map them to the numeric row names
country.names <- data_frame(id = row.names(countries@data),
                            country_iso3 = as.character(countries@data$ADM0_A3))
country.names2 <- data_frame(id = row.names(countries@data),
                             country_name = as.character(countries@data$NAME_EN))
country.names$neighbor_iso3 <- country.names$country_iso3 
country.names2$neighbor_name <- country.names2$country_name 

# Determine which countries are neighbors
# Adapted from http://stackoverflow.com/a/32318128/120898
#
# spdep::poly2nb/nb2mat method is faster and more accurate than rgeos::gTouches
#
# gTouches gives wrong results; doesn't see Russia-North Korea border; is suuuuuuuper slow
#   neighbor.matrix <- gTouches(countries, byid=TRUE)
#
neighbor.list <- poly2nb(countries)
neighbor.matrix <- nb2mat(neighbor.list, style="B", zero.policy=TRUE)
colnames(neighbor.matrix) <- rownames(neighbor.matrix)

# Clean up and transform the neighbor matrix
all.neighbors <- as.data.frame(neighbor.matrix) %>%
  mutate(country = row.names(.)) %>%  # Convert row names to actual column
  gather(neighbor, present, -country) %>%  # Convert to long
  filter(present == 1) %>%  # Only look at cells with a match
  # Add country names
  left_join(select(country.names, -neighbor_iso3), by=c("country" = "id")) %>%
  left_join(select(country.names, -country_iso3), by=c("neighbor" = "id")) %>%
  left_join(select(country.names2, -neighbor_name), by=c("country" = "id")) %>%
  left_join(select(country.names2, -country_name), by=c("neighbor" = "id")) %>%
  filter(country_iso3 != "-99", neighbor_iso3 != "-99")  # Remove missing countries
  # select(contains("iso3"))  # Just get the ISO columns
head(all.neighbors)

all.neighbors <- data.table(all.neighbors)
all.neighbors <- all.neighbors[, c("country_iso3", "neighbor_iso3", "country_name", "neighbor_name")]

#
#   country_iso3 neighbor_iso3
# 1          CHN           AFG
# 2          IRN           AFG
# 3          PAK           AFG
# 4          TJK           AFG
# 5          TKM           AFG
# 6          UZB           AFG

write.csv(all.neighbors, "data/lookup/2019-11-1-country-adjacent.csv", fileEncoding="UTF-8", row.names=F)

neighbor.summary <- all.neighbors %>%
  group_by(country_iso3) %>%
  summarise(num = n()) %>%
  arrange(desc(num))
neighbor.summary
