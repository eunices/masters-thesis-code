# An exercise on species disribution modelling with Megachile sculpturalis
# R packages to use: SDMTools, dismo, biomod, hSDM

# 02

# Setup
source('keys.R')
setwd(working_dir)


# Load libraries
library(data.table)
library(dismo)
library(maptools)
library(rgdal)
library(sp)
library(ggmap)
library(dplyr)


# Initialize variables
data(wrld_simpl)


# Params
map_crs = '+proj=longlat +datum=WGS84'

# clean data where every point would be used, prior to subsetting
data_in = 'data/2019-05-27-ejys-gbif-data/0018967-190415153152247-clean.csv' 


# Read dataset
df = fread(data_in, integer64="character")
df_cols = names(df)
if(any(df_cols == "V1")) {
    df$V1 = NULL
}

# Quick checks
necessary_cols = c(
    'decimalLatitude',
    'decimalLongitude',
    'scientificName',
    'year',
    'month',
    'day', 
    'countryCode',
    'institutionCodeShort'
)

# Summary of papers
# nest: shady, above ground, resin from conifers/ maple gum
# first recorded in North America 1990s
# humid, subtropical to temperate climates

# Approach of training and testing
# Train on native range (parameters by AIC?)
# Test on exotic range

# Check dataset length
dim(df)

# Check which have coords
df[,.(.N), by=.(hasCoordinate)] # retaining the original information prior to geocoding
df[is.na(decimalLatitude),] # but all are geocoded

# Check institution codes
df[,.(.N), by=.(institutionCodeShort, basisOfRecord)][order(basisOfRecord),]
df[basisOfRecord == "HUMAN_OBSERVATION",.(.N), by=.(institutionCode, basisOfRecord)][order(basisOfRecord),]
df[institutionCode == "", ..necessary_cols]

# Check issues
# see https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
df[,.(.N), by=.(issue)]
# issues don't seem very serious

# Check sex
df[,.(.N), by=.(sex)]
# many specimens do not have a gender assigned
test = dcast(df[!is.na(month) & sex != ""], month ~ sex, value.var="scientificName")
dcast(df[!is.na(month) & sex != ""], month ~ sex, value.var="scientificName")[, .SD/Reduce(`+`, .SD)*100, month]
# interesting to see males emerging earlier
test[,.(p_value_less_0.05 = binom.test(c(FEMALE, MALE), p=0.5)$p.value < 0.05), 
    by=.(month, MALE, FEMALE)]
# can perform binomial tests to test for statistical significance

# Check geography
df[countryCode %in% c("US"),.(.N), by=.(stateProvince)][order(-N)]
dcast(df[countryCode %in% c("US")], year ~ stateProvince, value.var="scientificName")

# Check month
df[,.(.N), by=.(month)][order(month)]
# mainly spotted from Jun - Aug
df_map = df[month %in% c("2", "4","5","11", "12"), ..necessary_cols]
# df_map = df[month %in% c("11"), ..necessary_cols]
df_map
mp = ggplot() + borders("world", colour="gray70", fill="gray80", ylim=c(20, 50)) 
mapPoints = mp + 
    geom_point(
        aes(x = decimalLongitude, y = decimalLatitude, colour=as.factor(month)), 
        data = df,
        alpha = .8,
        ) + theme_minimal()
mapPoints + labs(color='Month', x='Longtitude', y='Latitude')
dev.off()

# April records (max temp 20-23 deg C)
df[month=="4"]
# Feb record (max temp -4 to 2 deg C)
df[month=="2"] # seems like outlier
# Nov records (max temp 24 deg C)
df[month=="11"]
# Dec records (max temp 24 deg C)
df[month=="12"]

df[is.na(month), verbatimEventDate]
df[is.na(month), ..necessary_cols] # may not be useful if there is no date

df2= df
df2$decade = paste0(substr(df2$year, 0, 3), "0s")
df2[decade=="NA0s"]$decade = "No year"


maps = ggplot() +
  borders("state") +
  scale_size_area() +
  coord_quickmap()

mapPoints = maps + geom_point(
        aes(x = decimalLongitude, y = decimalLatitude, colour=as.factor(decade)), 
        data = df2[countryCode=="US"],
        alpha = .2, size=3
        ) + theme_minimal()
mapPoints + labs(color='Month', x='Longtitude', y='Latitude')
dev.off()


# Check year
df[is.na(year), ..necessary_cols]
df2 = df
df2$decade = paste0(substr(df2$year, 0, 3), "0s")
df2[decade=="NA0s"]$decade = "No year"

df2$addValue = "No georef"
df2[issue=="GEOREFERENCED_GOOGLE_API_WITH_HIGHER_GEOG"]$addValue = "Geo ref (higher geo)"
df2[issue=="GEOREFERENCED_GOOGLE_API"]$addValue = "Geo ref"

df2$addValue = factor(df2$addValue, levels=c("No georef", "Geo ref", "Geo ref (higher geo)"))

tib = df2 %>%
  group_by(decade, addValue) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n))
ggplot(data=tib, aes(x=decade, y=freq, fill=addValue), fill="black") +
  geom_bar(stat="identity") + theme_minimal() + labs(x="Decade", y="% of specimens", fill="Georeference status")

year = df2[,.(.N), by=.(decade, addValue)][order(decade)]
year
ggplot(data=year, aes(x=decade, y=N, fill=addValue), fill="black") +
  geom_bar(stat="identity") + theme_minimal() + labs(x="Decade", y="Number of specimens", fill="Georeference status")
dev.off(); year = NULL; df2 = NULL
year = df2[,.(.N), by=.(decade)][order(decade)]
year

# Check country codes
df[,.(.N), by=.(countryCode)][order(countryCode)]
yearCountry=dcast(df[countryCode != "" & countryCode != "JP"], countryCode + year ~ ., value.var="scientificName")
names(yearCountry)[3] = "count"

ggplot(data = yearCountry, aes(year, countryCode, fill = count))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "green", 
   midpoint = 0, space = "Lab", name="N occurrences") +
  theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
    size = 10, hjust = 1)) +
 coord_fixed() + labs(x="Year", y="Country code")


df[,.(.N), by=.(hasCoordinate, countryCode)][order(countryCode, hasCoordinate)]
# Added records from China, from Discover Life

# For modelling
m = df

# Convert to geodataframe
coordinates(m) = ~decimalLongitude + decimalLatitude
crs(m) = crs(wrld_simpl)

# Spatial checks
sp_join = over(m, wrld_simpl)
m@data$countryJoin = sp_join$FIPS
m@data$countryJoin2 = sp_join$NAME

# Check NAs
plot(wrld_simpl)
points(m[is.na(m@data$countryJoin),], col='red')
m2 = as.data.table(m@data) 
m2[, .(.N), by=.(countryJoin2)]
cols = necessary_cols[-1:-2]

# Check mismatches
country_do_not_match = m@data[m@data$countryCode!=m@data$countryJoin,]
dim(country_do_not_match)
summary = as.data.frame(unique(country_do_not_match[,c("countryCode", "countryJoin", "countryJoin2")]))
summary[order(summary$countryCode),]
cols = c(necessary_cols[-1:-3], "countryJoin")
# All is good based on country mapping


yearCountry=dcast(m2[countryCode != "" & countryCode != "JP"], countryJoin2 + year ~ ., value.var="scientificName")
names(yearCountry)[3] = "count"

ggplot(data = yearCountry, aes(year, countryJoin2, fill = count))+
 geom_tile(color = "white")+
 scale_fill_gradient2(low = "blue", high = "red", mid = "green", 
   midpoint = 0, space = "Lab", name="N occurrences") +
  theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
    size = 10, hjust = 1)) +
 coord_fixed() + labs(x="Year", y="Country")