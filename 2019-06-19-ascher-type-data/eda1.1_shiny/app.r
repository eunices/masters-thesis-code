# Visualising resource flow - from taxonomist country of residence to type locality
library(shiny)
library(data.table)
library(networkD3)

# Default settings
default_threshold <- 50

# Read data (residing in data folder)
lu <- fread("data/lookup-country-codes.csv",
           encoding="UTF-8", na.strings="")
t <- fread("data/2019-09-22-flow-map-type-loc-des-country.csv",
           encoding="UTF-8", na.strings="")

# Parameters
threshold <- default_threshold

# Create labels
nodes_lab <- paste0(lu$DL, " - ", lu$Country)

# Update data function
update <- function(threshold, country=NA, typeInput=c()) {
  t2 <- t[no_flow == "FALSE"]; no_flow <- t[no_flow=="TRUE"]

  if (is.na(country) | country == "All") {
    t2 <- t2[N>threshold]
  } else {
    if((1 %in% typeInput & 2 %in% typeInput)) {
      t2 <- t2[N>threshold & (ori == country | des == country)]
    } else if (1 %in% typeInput) {
      t2 <- t2[N>threshold & (ori == country)]
    } else if (2 %in% typeInput) {
      t2 <- t2[N>threshold & (des == country)]
    } else if (length(typeInput) <= 0) {
      nodes <- data.frame(idx=integer(), label=character())
      t2 <- data.frame(ori=character(), des=character())
    }
  }

  if (dim(t2)[1] == 0) {
    nodes <- data.frame(idx=integer(), label=character())
    t2 <- data.frame(ori=character(), des=character())
  } else {

    nodes <- unique(data.frame(label=c(t2$ori, t2$des)))
    nodes <- data.table(nodes); nodes <- nodes[order(label)]
    nodes$idx <- seq(0, dim(nodes)[1]-1, 1)
    nodes <- merge(nodes, no_flow[, c("ori", "N")], by.x="label", by.y="ori", all.x=T, all.y=F)

    t2 <- merge(t2, nodes[, c("label", "idx")], by.x="ori", by.y="label", all.x=T, all.y=F)
    t2 <- merge(t2, nodes[, c("label", "idx")], by.x="des", by.y="label", all.x=T, all.y=F, suffixes=c("_from", "_to"))
    # t2$Nodesize <- t2$N/threshold
  }
  list(t2=t2, nodes=nodes)
}


# UI component of shiny app
ui <- fluidPage(
  verticalLayout(
    inputPanel(
      sliderInput("thresholdInput", "Minimum threshold", 
                  min=0, max=floor(sort(t$N)[length(t$N)-1]/10)*10, value=default_threshold, post=" N spp.",
                  step=5),
      selectInput("countryInput", "Country (sorted by Discover Life codes)",
                  c("All", nodes_lab), selected=NULL, multiple=F),
      conditionalPanel(
        condition = "input.countryInput != 'All'",
        checkboxGroupInput("typeInput", "Type (donor or receipient)", 
                          choices = list("Origin" = 1,
                                          "Destination" = 2),
                          selected = c(1,2))
      )

    ),
    mainPanel(
      sidebarLayout(
        sidebarPanel(tableOutput("networkTable"), br(),
                     "Legend:", br(),
                     "Src = Source i.e., country of taxonomist", br(),
                     "Tgt = Target i.e. country of type species", br(),
                     "N = N species described", width=2), 
        mainPanel(forceNetworkOutput("network", height="500px"), width=10)
      ),
      width=12
    )
  )
)



# Server component of shiny app
server <- function(input, output) {

  # Output plots
  clickJS <- 'd3.selectAll(".node").on("click", function(d){ alert(d.name + ": " + d.nodesize + "spp."); })'
  output$network <- renderForceNetwork({
    parsed_country <- unlist(strsplit(input$countryInput, " - "))[1]
    if(parsed_country == "All") {
      updated <- update(input$thresholdInput, parsed_country)
    } else {
      updated <- update(input$thresholdInput, parsed_country, input$typeInput)
    }

    if (dim(updated$t2)[1] == 0) {

    } else {
      forceNetwork(
        Links = updated$t2, Nodes = updated$nodes,
        Source = "idx_from", Target = "idx_to", Value = "N", 
        NodeID = "label", Group="label", Nodesize="N", arrow=T, zoom=T, opacity=.8, 
        radiusCalculation = JS("Math.sqrt(d.nodesize, 2)"),
        linkWidth = JS("function(d) { return Math.log(d.value, 2); }"),
        charge=-20000/(input$thresholdInput+1), fontFamily="Calibri", 
        fontSize=20, opacityNoHover = 1, clickAction=clickJS)
    }
  })

  output$networkTable <- renderTable({
    parsed_country <- unlist(strsplit(input$countryInput, " - "))[1]
    if(parsed_country == "All") {
      updated <- update(input$thresholdInput, parsed_country)
    } else {
      updated <- update(input$thresholdInput, parsed_country, input$typeInput)
    }
    if(dim(updated$t2)[1] >=1) {
      tableshow <- updated$t2[,c("ori", "des", "N")]
      names(tableshow) <- c("Src", "Tgt", "N")
      tableshow
    } else { data.frame(Comment=c("No data")) }

  })

  # output$plot <- renderPlot({
  #   plot(rnorm(100))
  # })

}

# Run app
shinyApp(ui = ui, server = server)
