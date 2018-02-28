library(shiny)
library(ggplot2)
library(rinat)
library(DT)

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
         h3(textOutput("species_name")),
         tabsetPanel(
           tabPanel("Map", plotOutput("world_map")),
           tabPanel("Raw Data", DT::dataTableOutput("raw_data")),
           tabPanel("SOAR Derived Fields")
         )
           #plotOutput("world_map")
           #plotOutput("state_map")
           #dataTableOutput("data")
       )
    )
)

# create server which is the logic of the app
server <- function(input, output) {
    observeEvent(input$do, {

        output$species_name <- renderText({input$species_name})
        
        if (input$checkbox == TRUE) {
        bounds <- c(input$Lat_high, input$Long_high, input$Lat_low, input$Long_low)
        } else {bounds <- NULL}
        
        INATdata <- get_inat_obs(taxon_name = input$species_name, bounds = bounds)
        #fileData <- write.csv(INATdata, file = "INATdata.csv", row.names = FALSE)
        output$raw_data <- DT::renderDataTable({expr = INATdata})
                    
        output$world_map <- renderPlot({
            data <- get_inat_obs(taxon_name = input$species_name, bounds = bounds)
            #data_map <- map(database ="world", xlim=c(input$Long_high, input$Long_low), ylim=c(input$Lat_high, input$Lat_low))
            #points(data)
            data_map <- inat_map(data, map = "world", plot = FALSE)
            data_map + borders("state") + theme_bw()
           # data_map + xlim=c(input$Long_high, input$Long_low) + ylim=c(input$Lat_high, input$Lat_low)
        })
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

