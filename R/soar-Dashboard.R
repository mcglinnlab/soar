library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(rgbif)
library(shinycssloaders)

minChoice <- function() {
  Gbif_fields <- read.csv("gbif_fields.csv", as.is = TRUE)
  # [row,col] cols: colName, MetaData, Minimal, Default, Custom
  lis<-c()
  lis2 <- c()
  for (i in 1:length(Gbif_fields[,1])){
    if (Gbif_fields[i,3] == 1) {
      lis<- c(lis, paste(Gbif_fields[i,1]))
      lis2 <- c(lis2,i)
    }
  }
  finalList <- list()
  for (i in 1:length(lis)) {
    finalList[[lis[i]]] <- as.double(lis2 [i])
  }
  
  return(finalList)
  
}
defChoice <- function() {
  Gbif_fields <- read.csv("gbif_fields.csv", as.is = TRUE)
  # [row,col] cols: colName, MetaData, Minimal, Default, Custom
  lis<-c()
  lis2 <- c()
  for (i in 1:length(Gbif_fields[,1])){
    if (Gbif_fields[i,4] == 1) {
      lis<- c(lis, paste(Gbif_fields[i,1]))
      lis2 <- c(lis2,i)
    }
  }
  finalList <- list()
  for (i in 1:length(lis)) {
    finalList[[lis[i]]] <- as.double(lis2 [i])
  }
  
  return(finalList)
  
}
cusChoice <- function() {
  Gbif_fields <- read.csv("gbif_fields.csv", as.is = TRUE)
  # [row,col] cols: colName, MetaData, Minimal, Default, Custom
  lis<-c()
  lis2 <- c()
  for (i in 1:length(Gbif_fields[,1])){
    if (Gbif_fields[i,5] == 1) {
      lis<- c(lis, paste(Gbif_fields[i,1]))
      lis2 <- c(lis2,i)
    }
  }
  finalList <- list()
  for (i in 1:length(lis)) {
    finalList[[lis[i]]] <- as.double(lis2 [i])
  }
  
  return(finalList)
  
}
#create a user interface
ui <- dashboardPage(
  #App title
  dashboardHeader(title = 'SOAR'),
  
  #sidebar with input fields
  dashboardSidebar(
    #get species name
    textInput("species_name", "Species name", "Caretta caretta"),
    textInput("rank", "Taxonomic rank", "species"),
    
    #get login information
    textInput("gbif_username", "Gbif Username", "Ex: GbifUser1313"),
    passwordInput("gbif_pass", "Gbif Password", "Ex: GbifPass2424"),
    textInput("gbif_email", "Gbif Email", "Ex: JaneSmith@generic.com"),
    
    #input latlong bounding box
    checkboxInput("latLongcheckbox", label = "Input Geographical Boundaries with Latitude and Longitude", 
                  value = FALSE),
    conditionalPanel (
      condition = "input.latLongcheckbox == true",
      textInput("Lat_low", "Latitude lower range", "-90"),
      textInput("Lat_high", "Latitude upper range", "90"),
      textInput("Long_low", "Longitude lower range", "-180"),
      textInput("Long_high", "Longitude upper range", "180")
    ),
    
    #input bounding box with political boundaries
    checkboxInput("politicalcheckbox", label = "Input Geographical boundaries by country"),
    conditionalPanel(
      condition ="input.politicalcheckbox == true",
      textInput("political_boundary", "Country Name", "Country Code")
      ),
   
     #input date bounding information for gbif
    checkboxInput("datecheckbox", label = "Sort by date", value = FALSE),
    conditionalPanel (
      condition = "input.datecheckbox == true",
      textInput("from_date", "Show results from this month:", "mm/yyyy"),
      textInput("to_date", "to this month:", "mm/yyyy")
    ),
  
    #input specific download key
    checkboxInput("downKey", label = "Specify Download Key", value = FALSE),
    conditionalPanel (
      condition = "input.downKey == true",
      textInput("down_key", "Download Key:", "" )
    ),
      
    actionButton("do", "Submit")
  ),
   
  #show results here
  dashboardBody(
    tabsetPanel(
      tabPanel("Map", withSpinner(leafletOutput("world_map")),
#Add downsampling options here
               box(title = "Down-sample Tools", "Option 1:",
                actionButton("DefDown", "Down-Sample data using simple random sampling"), br(),
                "Option 2:", 
                textInput("GridCellSize", "Grid Cell Size: "), textInput("MinCellDen", "Minimum Cell Density:"), 
                actionButton("CusDown", "Down-Sample data with custom settings"))),
      tabPanel("Raw Data", DT::dataTableOutput("raw_data")),
      tabPanel("Download table",
               radioButtons("tableCols", label = "Columns in Downloadable table",
                                  choices = list("Minimal" = 1, "Default" = 2, "All Columns" = 3, "Custom" = 4), 
                                  selected = 1),
#ADD - Download link for table
               downloadButton('downloadData', label = "Download Table"),
               conditionalPanel(condition = "input.tableCols == 1", 
                                checkboxGroupInput("tableMin", label = h4("Minimal Options:"), choices =
                                                   minChoice(), 
                                                   selected = c(133, 134, 175, 220, 229))),
               conditionalPanel(condition = "input.tableCols == 2",
                                checkboxGroupInput("tableDef", label= h4("Default Options:"), choices = defChoice(),
                                                   selected = c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230))
                                ),
               conditionalPanel(condition = "input.tableCols == 3",
                                h4("Table will contain all columns.")),
               conditionalPanel(condition = "input.tableCols == 4",
                                #checkboxes with 235 options
                                checkboxGroupInput("tableCus", label= h4("Select Options (Defaults Options Selected):"), choices = cusChoice(),
                              selected = c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230))
                                )
               )
      
    )
    
  )
)
#server- logic of the app
server <- function(input, output) {
  
  gbif_data <- eventReactive(input$do, {
    #filter by name
    sp_key <- name_suggest(q =input$species_name, rank = input$rank)$key[1]
    #filter by country
    if (input$politicalcheckbox){ country <- input$political_boundary}
    #filter by date
    if (input$datecheckbox){
      toM <- substr(input$to_date, 1,2)
      fromM<- substr(input$from_date, 1,2)
      toY <- substr(input$to_date, 4, 7)
      fromY <- substr(input$from_date, 4, 7)
    }
    #filter by lat/long
    if (input$latLongcheckbox){
      latLow <- input$Lat_low
      latHigh <- input$Lat_high
      longLow <- input$Long_low
      longHigh <- input$Long_high
    }
#empty fields cannot be set to NULL, this sees what data is added and runs the correct
#request depending on what fields are full. It's really big
    if (input$downKey){
      continue = FALSE
      dat <- occ_download_get(key = toString(input$down_key), overwrite = TRUE) %>% occ_download_import()
      return(dat)
    }
    else if (input$latLongcheckbox){
      continue = TRUE
      if (input$datecheckbox & input$politicalcheckbox){
        res = occ_download(paste("decimalLatitude >=", latLow), paste("decimalLatitude <=", latHigh),
                           paste("decimalLongitude >=", longLow), paste("decimalLongitude <=", longHigh),
                           paste("year >=", fromY), paste("year <=", toY), paste("month >=", fromM),
                           paste("month <=", toM), paste("country =", country), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else if (input$datecheckbox){
        res = occ_download(paste("decimalLatitude >=", latLow), paste("decimalLatitude <=", latHigh),
                           paste("decimalLongitude >=", longLow), paste("decimalLongitude <=", longHigh),
                           paste("year >=", fromY), paste("year <=", toY), paste("month >=", fromM),
                           paste("month <=", toM), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else if (input$politicalcheckbox){
        res = occ_download(paste("decimalLatitude >=", latLow), paste("decimalLatitude <=", latHigh),
                           paste("decimalLongitude >=", longLow), paste("decimalLongitude <=", longHigh),
                           paste("country =", country), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else{
        res = occ_download(paste("decimalLatitude >=", latLow), paste("decimalLatitude <=", latHigh),
                           paste("decimalLongitude >=", longLow), paste("decimalLongitude <=", longHigh),
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
    }
    else if (input$datecheckbox){
      continue = TRUE
      if (input$politicalcheckbox){
        res = occ_download(paste("year >=", fromY), paste("year <=", toY), paste("month >=", fromM),
                           paste("month <=", toM), paste("country =", country), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else{
        res = occ_download(paste("year >=", fromY), paste("year <=", toY), paste("month >=", fromM),
                           paste("month <=", toM), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
    }
    else if (input$politicalcheckbox){
      continue = TRUE
      res = occ_download(paste("country =", country), 
                         paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                         user = input$gbif_username, pwd = input$gbif_pass, 
                         email = input$gbif_email)
    }
    else{
      continue = TRUE
      res = occ_download(paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                         user = input$gbif_username, pwd = input$gbif_pass, 
                         email = input$gbif_email)
    }
    
#loops so that it checks every 30 seconds to see if meta$status is "SUCCEEDED" or "KILLED"
    while(continue){
      meta = occ_download_meta(res)
       if (meta$status == "SUCCEEDED"){
        continue = FALSE
        dat <- occ_download_get(res[1], overwrite = TRUE) %>%
          occ_download_import()
        return(dat)
       }     
      if (meta$status == "KILLED"){
        continue = FALSE
        #Returns: ERROR:Bad Request(HTTP 400)
      }
      #30 second delay --  Extend?
      Sys.sleep(30)
    }
      
  })
  
#Displays Entire Dataset
  #-Changed gbif_data() to downloadInfo() to only show cols selected by user
  output$raw_data <- DT::renderDataTable(expr = downloadInfo(),options=list(autoWidth = TRUE,scrollX=TRUE))
  
#Default Cols? [,c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230)]
#Makes A Downloadable Table for only the columns selected
  downloadInfo <- function(){
    gDat <- gbif_data()
    if(input$tableCols == 1){
      return(gDat[ , as.numeric(input$tableMin)])
    }
    else if(input$tableCols == 2){
      return(gDat[ , as.numeric(input$tableDef)])
    }
    else if(input$tableCols == 3){
      return(gDat)
    }
    else{
      return(gDat[ , as.numeric(input$tableCus)])
    }
  }
  
  output$downloadData <- downloadHandler(
    filename = function(){"gbifDat.csv"},
    content = function(file){
      write.csv(downloadInfo(), file)
    }
  )
  
  output$world_map <- renderLeaflet({
    leaflet(gbif_data()) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      
      addCircleMarkers(lng = ~ decimalLongitude, lat = ~ decimalLatitude,
                       radius = 3,
                       color = 'red',
                       stroke = FALSE, fillOpacity = 0.5
      )
  })
}

shinyApp(ui, server)