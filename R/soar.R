library(shiny)
library(ggplot2)
library(rinat)

# create user interface
ui <- fluidPage(
  
    # Application title
    titlePanel("SOAR: species occupancy aggregator"),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
       sidebarPanel(
          textInput("species_name", "Species name", "Bald eagle"),
          actionButton("do", "Submit")
       ),
    
       # Show a plot of the generated distribution
       mainPanel(
           h3(textOutput("species_name")),
           plotOutput("world_map")
           #plotOutput("state_map")
           #dataTableOutput("data")
       )
    )
)

# create server which is the logic of the app
server <- function(input, output) {
    observeEvent(input$do, {

        output$species_name <- renderText({input$species_name})

        output$world_map <- renderPlot({
            data <- get_inat_obs(query = input$species_name)
            data_map <- inat_map(data, "world", plot = FALSE)
            data_map + borders("state") + theme_bw()
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