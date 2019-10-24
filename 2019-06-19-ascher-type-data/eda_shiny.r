source('keys.r')
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - Launch shiny apps
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Launch apps
shiny::runApp('2019-06-19-ascher-type-data/eda1.1_shiny')
shiny::runApp('2019-06-19-ascher-type-data/eda2.1_shiny')

# Deploy apps
rsconnect::setAccountInfo(name='ejysoh',
			              token=shiny_token1,
			              secret=shiny_secret1)
rsconnect::deployApp('2019-06-19-ascher-type-data/eda1.1_shiny', account="ejysoh")
rsconnect::setAccountInfo(name='ejysoh',
			              token=shiny_token2,
			              secret=shiny_secret2)
rsconnect::deployApp('2019-06-19-ascher-type-data/eda2.1_shiny', account="ejysoh")