library(shiny)
library(ggplot2)
library(rinat)
library(DT)
library(leaflet)

# create user interface
ui <- fluidPage(
  
  # Application title
  titlePanel("SOAR: species occupancy aggregator in R"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("species_name", "Species name", "Bald eagle"),
      checkboxInput("checkbox", label = "Input Latitude and Longitude bounds", value = FALSE),
      conditionalPanel (
        condition = "input.checkbox == true",
        textInput("Lat_low", "Latitude lower range", "90"),
        textInput("Lat_high", "Latitude upper range", "-90"),
        textInput("Long_low", "Longitude lower range", "180"),
        textInput("Long_high", "Longitude upper range", "-180")
      ),
      actionButton("do", "Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      #h3(textOutput("species_name")),
      tabsetPanel(
        tabPanel("Map", leafletOutput("world_map")),
        tabPanel("Raw Data", DT::dataTableOutput("raw_data")),
        tabPanel("SOAR Derived Fields")
      )
    )
  )
)

# create server which is the logic of the app
server <- function(input, output) {
  
  
  inat_data <- eventReactive(input$do, {
    if (input$checkbox == TRUE) {
             bounds <- c(input$Lat_high, input$Long_high,input$Lat_low, input$Long_low)
          } else bounds <- NULL
    #To Do: provide user option for maxresults argument
    inat_result <- get_inat_obs(taxon_name = input$species_name, bounds = bounds)
    coords_na <- apply(inat_result[ , c('longitude', 'latitude')], 1,
                       function(x) any(is.na(x)))
    inat_result[!coords_na, ]
  })
  output$raw_data <- DT::renderDataTable(expr = inat_data())
  
  
  #fileData <- write.csv(INATdata, file = "INATdata.csv", row.names = FALSE)
  
  pal <- colorFactor(palette = c("red", "orange", "navy"),
                     domain = c("casual", "needs_id", "research"))
  
  output$world_map <- renderLeaflet({
    leaflet(inat_data()) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addCircleMarkers(lng = ~ longitude, lat = ~latitude ,
                       radius = ~ifelse(quality_grade == "research", 6, 3),
                       color = ~pal(quality_grade),
                       stroke = FALSE, fillOpacity = 0.5
      ) %>%
      addLegend("bottomright", pal = pal, values = ~quality_grade,
                labels = c('casual', 'needs id', 'research'),
                title = "The quality of the records"
      )
  })
}

# Create a Shiny app object
#shinyApp(ui = ui, server = server)

#' SOAR package Graphic User Interface
#'
#' User interface of the soar package.
#'
#' @param port char. The TCP port that the application should listen on (see
#'   \code{\link[shiny]{runApp}} for more details).
#' @param host char. The IPv4 address that the application should listen on (see
#'   \code{\link[shiny]{runApp}} for more details).
#' @param working.directory char. Directory in which the application will run.
#'
#' @return Open a window with a shiny app to use the soar package with an
#'   user-friendly interface.
#'
#' @examples
#' \dontrun{
#' gui()
#' }
#'
#' @export
gui <- function(port = getOption("shiny.port"),
                host = getOption("shiny.host", "127.0.0.1")) {
  
  shiny::runApp(shinyApp(ui = ui, server = server),
                display.mode = "normal", port = port, host = host)
  rm(ui, server, envir = .GlobalEnv)
}