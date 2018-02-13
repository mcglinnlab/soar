library(shiny)
library(rinat)
library(maps)

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
            map('world')
            points(latitude ~ longitude, data = data,
                   col = 'red', pch = 19, cex = 0.25)
            
            #map('state')
            #points(latitude ~ longitude, data = data,
            #      col = 'red', pch = 19, cex = 0.25)
        })
    
    #output$state_map <- renderPlot({
    #    map('state')
    #    points(latitude ~ longitude, data = output$data,
    #           col = 'red', pch = 19, cex = 0.25)
    #})
    })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)