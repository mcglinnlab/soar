library(shinydashboard)
library(ggplot2)
#want to switch functionality from rinat to rgbif
library(rinat)
library(DT)
library(leaflet)
library(rgbif)

#create a user interface
ui <- dashboardPage(
  #App title
  dashboardHeader(title = 'SOAR'),
  
  #sidebar with input fields
  dashboardSidebar(
    #get species name
    textInput("species_name", "Species name", "Bald eagle"),
    textInput("rank", "Taxonomic rank", "species"),
    
    #get login information
    textInput("gbif_username", "Gbif Username", "Ex: GbifUser1313"),
    textInput("gbif_pass", "Gbif Passord", "Ex: GbifPass2424"),
    textInput("gbif_email", "Gbif Email", "Ex: JaneSmith@generic.com"),
    
    #input latlong bounding box
    checkboxInput("latLongcheckbox", label = "Input Geographical Boundaries with Latitude and Longitude", 
                  value = FALSE),
    conditionalPanel (
      condition = "input.latLongcheckbox == true",
      textInput("Lat_low", "Latitude lower range", "90"),
      textInput("Lat_high", "Latitude upper range", "-90"),
      textInput("Long_low", "Longitude lower range", "180"),
      textInput("Long_high", "Longitude upper range", "-180")
    ),
    
    #input bounding box with political boundaries
    checkboxInput("politicalcheckbox", label = "Input Geographical boundaries by country"),
    conditionalPanel(
      condition ="input.politicalcheckbox == true",
      textInput("political_boundary", "Country Name", "US")
      ),
   
     #input date bounding information for gbif
    checkboxInput("datecheckbox", label = "Sort by date", value = FALSE),
    conditionalPanel (
      condition = "input.datecheckbox == true",
      textInput("from_date", "Show results from this month:", "mm/yyyy"),
      textInput("to_date", "to this month:", "mm/yyyy")
    ),
      
    actionButton("do", "Submit")
  ),
  
  #show results here
  dashboardBody(
    tabsetPanel(
      tabPanel("Map",  h4("Please allow a few minutes to retrieve data after pressing 'Submit'"),
               leafletOutput("world_map")),
      tabPanel("Raw Data", DT::dataTableOutput("raw_data")),
      tabPanel("Download table", h2("Ability to download the table will be added here later"))
    )
    
  )
)
#server- logic of the app
server <- function(input, output) {
  
  gbif_data <- eventReactive(input$do, {
    sp_key <- name_suggest(q =input$species_name, rank = input$rank)
    #filter by country
    if (input$politicalcheckbox){ country <- input$political_boundary}
    else {country <- NULL}
    #filter by date
    if (input$datecheckbox){
      toM <- substr(input$to_date, 1,2)
      fromM<- substr(input$from_date, 1,2)
      toY <- substr(input$to_date, 4, 7)
      fromY <- substr(input$from_date, 4, 7)
    }
    else{toM <- NULL 
    fromM<- NULL
    toY <- NULL
    fromY <- NULL}
    #filter by lat/long
    if (input$latLongcheckbox){
      latLow <- input$Lat_low
      latHigh <- input$Lat_high
      longLow <- input$Long_low
      longHigh <- input$Long_high
    }
    else{
      latLow <- NULL
      latHigh <- NULL
      longLow <- NULL
      longHigh <- NULL
    }
    res = occ_download(paste("decimalLatitude >=", latLow), paste("decimalLatitude <=", latHigh),
                       paste("decimalLongitude >=", longlow), paste("decimalLongitude <=", longHigh),
                       paste("year >=", fromY), paste("year <=", toY), paste("month >=", fromM),
                       paste("month <=", toM), paste("country =", country), 
                       paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                       user = input$gbif_username, pwd = input$gbif_pass, 
                       email = input$gbif_email)
#loops so that it checks every 30 seconds to see if meta$status is "SUCCEEDED" or "KILLED"
    continue <- TRUE
    while(continue){
      meta <- occ_download_meta(res)
       if (meta$status == "SUCCEEDED"){
        continue <- FALSE 
        dat <- occ_download_get(res[1], overwrite = TRUE) %>%
          occ_download_import()
       }
#fix this to actually handle the case      
      if (meta$status == "KILLED"){
        "An error has occured with your request"
        continue <- FALSE
      }
      #30 second delay --  Extend?
      Sys.sleep(30)
    }
      
  })
  
#NEEDS TO BE ERASED AND PHASED OUT  
  inat_data <- eventReactive(input$do, {
    if (input$latLongcheckbox == TRUE) {
      bounds <- c(input$Lat_high, input$Long_high,input$Lat_low, input$Long_low)
    } else bounds <- NULL
    
    #To Do: provide user option for maxresults argument
    inat_result <- get_inat_obs(taxon_name = input$species_name, bounds = bounds)
    coords_na <- apply(inat_result[ , c('longitude', 'latitude')], 1,
                       function(x) any(is.na(x)))
    inat_result[!coords_na, ]
  })
  
#!!!!!!!!!!!!!!!!!!!!!!-  Has been changed to include gbif_data rather than inat_data    
  output$raw_data <- DT::renderDataTable(expr = gbif_data())
  
  
  #fileData <- write.csv(gbif_data, file = "SOARdata.csv", row.names = FALSE)
  
  pal <- colorFactor(palette = c("red", "orange", "navy"),
                     domain = c("casual", "needs_id", "research"))
  
  output$world_map <- renderLeaflet({
#!!!!!!!!!!!!!!!!!!!!!!-  Has been changed to include gbif_data rather than inat_data    
    leaflet(gbif_data()) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addCircleMarkers(lng = ~ longitude, lat = ~latitude ,
                       radius = ~ifelse(quality_grade == "research", 6, 3),
                       color = ~pal(quality_grade),
                       stroke = FALSE, fillOpacity = 0.5
      ) %>%
#Remove This, there are no longer 'grades' to the observation
      addLegend("bottomright", pal = pal, values = ~quality_grade,
                labels = c('casual', 'needs id', 'research'),
                title = "The quality of the records"
      )
  })
}

shinyApp(ui, server)