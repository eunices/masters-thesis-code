# Information about code:
# This code corresponds to launching shiny visualisations for my MSc thesis.
# Refer to eda1.1_shiny and eda2.1_shiny folders.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Launch shiny apps
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Set up
source('keys.r')

# Launch apps
shiny::runApp('2019-06-19-ascher-type-data-shiny/eda1.1_shiny')
shiny::runApp('2019-06-19-ascher-type-data-shiny/eda2.1_shiny')

# Deploy apps
rsconnect::setAccountInfo(name='ejysoh', token=shiny_token, secret=shiny_secret)
rsconnect::deployApp('2019-06-19-ascher-type-data-shiny/eda1.1_shiny', account="ejysoh")
rsconnect::deployApp('2019-06-19-ascher-type-data-shiny/eda2.1_shiny', account="ejysoh")