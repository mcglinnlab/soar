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
            data_map = inat_map(data, "world", plot = FALSE)
            data_map + borders("state") + theme_bw()
        })
    })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)