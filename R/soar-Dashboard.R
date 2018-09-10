library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(rgbif)
library(shinycssloaders)

choice <- function(input_num) {
  Gbif_fields <- read.csv("gbif_fields.csv", as.is = TRUE)
  # Note to Self -- [row,col] cols: colName, MetaData, Minimal, Default, Custom
  #Sets lis to a list of the row names selected and sets selected to a list of values in the column
  if (input_num ==1){
    lis<-Gbif_fields$Column.Name[Gbif_fields$Minimal == 1]
    selected <- Gbif_fields$Minimal
  }
  else if (input_num == 2) {
    lis<-Gbif_fields$Column.Name[Gbif_fields$Default == 1]
    selected <- Gbif_fields$Default
  }
  else {
    lis<-Gbif_fields$Column.Name[Gbif_fields$Custom == 1]
    selected <- Gbif_fields$Custom
  }
  lis2 <- c()
  #Sets lis2 to a list of each row number that corresponds to a value in lis
  for (i in 1:length(Gbif_fields$Column.Name)){
    if (selected[i] == 1) {
      lis2 <- c(lis2,i)
    }
  }
  final_list <- list()
  #combines lis and lis2 into a format that can be used by the choices variabe in checkboxGroupInput()
  for (i in 1:length(lis)) {
    final_list[[lis[i]]] <- as.double(lis2 [i])
  }
  return(final_list)
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
    checkboxInput("lat_long_check_box", label = "Input Geographical Boundaries with Latitude and Longitude", 
                  value = FALSE),
    conditionalPanel (
      condition = "input.lat_long_check_box == true",
      textInput("Lat_low", "Latitude lower range", "-90"),
      textInput("Lat_high", "Latitude upper range", "90"),
      textInput("Long_low", "Longitude lower range", "-180"),
      textInput("Long_high", "Longitude upper range", "180")
    ),
    
    #input bounding box with political boundaries
    checkboxInput("political_checkbox", label = "Input Geographical boundaries by country"),
    conditionalPanel(
      condition ="input.political_checkbox == true",
      textInput("political_boundary", "Country Name", "Country Code")
      ),
   
     #input date bounding information for gbif
    checkboxInput("date_checkbox", label = "Sort by date", value = FALSE),
    conditionalPanel (
      condition = "input.date_checkbox == true",
      textInput("from_date", "Show results from this month:", "mm/yyyy"),
      textInput("to_date", "to this month:", "mm/yyyy")
    ),
  
    #input specific download key
    checkboxInput("down_key_checkbox", label = "Specify Download Key", value = FALSE),
    conditionalPanel (
      condition = "input.down_key_checkbox == true",
      textInput("down_key", "Download Key:", "" )
    ),
      
    actionButton("do", "Submit")
  ),
   
  #show results here
  dashboardBody(
    tabsetPanel(
      tabPanel("Map", withSpinner(leafletOutput("world_map"))),
      tabPanel("Raw Data", DT::dataTableOutput("raw_data")),
      tabPanel("Download Table",
               radioButtons("table_cols", label = "Columns in Downloadable table",
                                  choices = list("Minimal" = 1, "Default" = 2, "All Columns" = 3, "Custom" = 4), 
                                  selected = 1),
#ADD - Download link for table
               downloadButton('download_data', label = "Download Table"),
               conditionalPanel(condition = "input.table_cols == 1", 
                                checkboxGroupInput("table_min", label = h4("Minimal Options:"), choices =
                                                   choice(1), 
                                                   selected = c(133, 134, 175, 220, 229))),
               conditionalPanel(condition = "input.table_cols == 2",
                                checkboxGroupInput("table_def", label= h4("Default Options:"), choices = choice(2),
                                                   selected = c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230))
                                ),
               conditionalPanel(condition = "input.table_cols == 3",
                                h4("Table will contain all columns.")),
               conditionalPanel(condition = "input.table_cols == 4",
                                #checkboxes with 235 options
                                checkboxGroupInput("table_cus", label= h4("Select Options (Defaults Options Selected):"), choices = choice(3),
                              selected = c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230))
                                )
               ),
              
      tabPanel("Clean Data", 
               h4("Flag possibly erronious data based on:"),
               checkboxInput("cap_prox_box", label = "Proximity to capitals", value = FALSE),
               checkboxInput("cen_prox_box", label = "Proximity to country centroids", value = FALSE),
               checkboxInput("lat_long_check", label = "Actual coordinate location vs. country specified by the data (It is recomended that this not be selected if oceanic species are involved)", value = FALSE),
               checkboxInput("rec_dup", label = "Duplications of records", value = FALSE), 
               checkboxInput("iden_lat_long", label = "Records with identical coordinates", value = FALSE),
               checkboxInput("gbif_prox", label = "Proximity to Gbif headquarters", value = FALSE),
               checkboxInput("bio_prox", label = "Proximity to biodiversity institutions", value = FALSE),
               checkboxInput("out_box", label = "Outliers", value = FALSE),
               checkboxInput("sea_box", label = "Locaton relative to the oceans (check if records include only terrestrial organisms)", value = FALSE),
               checkboxInput("urb_prox", label = "Proximity to urban areas", value = FALSE),
               checkboxInput("equ_zero_zero", label = "Equal latitude and longitude, plain zeros, and proximity to point 0/0", value = FALSE),
              
               
              #These are conditional, only available if their corresponding box is checked
               conditionalPanel(condition = "input.cap_prox_box == true",
                                numericInput("cap_rad", "Radius around capitals (degrees)", 0.1)
                                ),
               conditionalPanel(condition = "input.cen_prox_box == true",
                                numericInput("cen_rad", "Side length of rectangle around country centroids (degrees)", 0.01),
                                selectInput("cen_detail", "Test around country centroids, province centroids, or both", 
                                            choices = list("Country" = 1, "Province" = 2, "Both" = 3), selected = 3)
                                ),
               conditionalPanel(condition = "input.bio_prox == true | input.gbif_prox == true",
                                numericInput("inst_rad", "Radius around biodiversity institutions, including GBIF (degrees)", 0.001)
                                ),
               conditionalPanel(condition = "input.out_box == true", 
                                selectInput("out_type", "What outlier test would you like to use?", 
                                            choices = list("Boxplot distance of records from each other" = 1, 
                                                           "Mean absolute deviation of the distance of records from each other" = 2,
                                                           "Minimum distance to next record is greater than selected" = 3)
                                            #1 = "outlier", 2 = "mad", 3 = "distance"
                                            ),
                                conditionalPanel(condition = "input.out_type == 2", 
                                                 numericInput("out_mtp","Multiplier for interquartile range",3) ),
                                conditionalPanel(condition = "input.out_type == 3",
                                                 numericInput("out_td","Minimum distance of a record to all other records to be identified as an outlier (km)",1000) ),
                                numericInput("out_size", "Minimum number of records to run the taxon-specific outlier test", 7)
                                ),
               conditionalPanel(condition = "input.equ_zero_zero == true",
                                numericInput("zero_rad", "Radius around 0/0 (degrees)", 0.5)
               ),
               #provide these (do not require as input):
                #capitals.ref
                #centroids.ref
                #country.ref
                #inst.ref
                #seas.ref
                #urban.ref
               selectInput("value_input", "What kind of output would you like?", 
                           choices = list("Spatial Valid - A separate document with what data was flagged and for what test" = 1, "Clean - the flagged data is automatically removed" = 2), selected = 1),
               #verbose = False
              actionButton("do_clean", "Submit")
               ),
      tabPanel("Alternative Coordinates",
               h4("Directions:"),
               h4("If you wish to see possible alternatives for outliers click on the outlier. You will then be given options (Purple) including swapped x and y coordinates, changing the sign on the x and y coordinates, and a few others.
                  select the one you wish to replace the old coordinate set with or on the origional point to exclude it. Select the stop button if none of the 
                  alternatives are correct but you do not wish to exlude the point. When a record is changed, all rcords with identical coordinates will be 
                  changed in the same way.")
               #Add the output for the table here
               ),
      tabPanel("Detect Bias"),
      tabPanel("Download Cleaned Data", 
               h4("'True' means the data passed the tests indicated, 'False' means it failed"),
               DT::dataTableOutput("clean_data"))
    )
    
  )
)
#server- logic of the app
server <- function(input, output) {
  
  gbif_data <- eventReactive(input$do, {
    #filter by name
    sp_key <- name_suggest(q =input$species_name, rank = input$rank)$key[1]
    #filter by country
    if (input$political_checkbox){ country <- input$political_boundary}
    #filter by date
    if (input$date_checkbox){
      to_m <- substr(input$to_date, 1,2)
      from_m<- substr(input$from_date, 1,2)
      to_y <- substr(input$to_date, 4, 7)
      from_y <- substr(input$from_date, 4, 7)
    }
    #filter by lat/long
    if (input$lat_long_check_box){
      low_lat <- input$Lat_low
      high_lat <- input$Lat_high
      low_long <- input$Long_low
      high_long <- input$Long_high
    }
#empty fields cannot be set to NULL, this sees what data is added and runs the correct
#request depending on what fields are full. It's really big
    if (input$down_key_checkbox){
      continue = FALSE
      dat <- occ_download_get(key = toString(input$down_key), overwrite = TRUE) %>% occ_download_import()
      return(dat)
    }
    else if (input$lat_long_check_box){
      continue = TRUE
      if (input$date_checkbox & input$political_checkbox){
        res = occ_download(paste("decimalLatitude >=", low_lat), paste("decimalLatitude <=", high_lat),
                           paste("decimalLongitude >=", low_long), paste("decimalLongitude <=", high_long),
                           paste("year >=", from_y), paste("year <=", to_y), paste("month >=", from_m),
                           paste("month <=", to_m), paste("country =", country), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else if (input$date_checkbox){
        res = occ_download(paste("decimalLatitude >=", low_lat), paste("decimalLatitude <=", high_lat),
                           paste("decimalLongitude >=", low_long), paste("decimalLongitude <=", high_long),
                           paste("year >=", from_y), paste("year <=", to_y), paste("month >=", from_m),
                           paste("month <=", to_m), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else if (input$political_checkbox){
        res = occ_download(paste("decimalLatitude >=", low_lat), paste("decimalLatitude <=", high_lat),
                           paste("decimalLongitude >=", low_long), paste("decimalLongitude <=", high_long),
                           paste("country =", country), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else{
        res = occ_download(paste("decimalLatitude >=", low_lat), paste("decimalLatitude <=", high_lat),
                           paste("decimalLongitude >=", low_long), paste("decimalLongitude <=", high_long),
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
    }
    else if (input$date_checkbox){
      continue = TRUE
      if (input$political_checkbox){
        res = occ_download(paste("year >=", from_y), paste("year <=", to_y), paste("month >=", from_m),
                           paste("month <=", to_m), paste("country =", country), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else{
        res = occ_download(paste("year >=", from_y), paste("year <=", to_y), paste("month >=", from_m),
                           paste("month <=", to_m), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
    }
    else if (input$political_checkbox){
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
  #-Changed gbif_data() to download_info() to only show cols selected by user
  output$raw_data <- DT::renderDataTable(expr = download_info(),options=list(autoWidth = TRUE,scrollX=TRUE))
  
#Display Cleaned Dataset based on criteria  
  output$clean_data <- DT::renderDataTable(expr = clean_info(),options=list(autoWidth = TRUE,scrollX=TRUE))
  
  #Clean Data based on input
  clean_info <- eventReactive(input$do_clean, {
    dat = CleanCoordinates(x= gbif_data(),lon = "decimalLongitude", lat = "decimalLatitude",
                           species = "species", countries = "countryCode",
                           capitals = input$cap_prox_box,
                           centroids = input$cen_prox_box,
                           countrycheck = input$lat_long_check,
                           duplicates = input$rec_dup,
                           equal = input$iden_lat_long,
                           GBIF = input$gbif_prox,
                           institutions = input$bio_prox,
                           outliers = input$out_box,
                           seas = input$sea_box,
                           urban = input$urb_prox,
                           zeros = input$equ_zero_zero,
                           capitals.rad = input$cap_rad,
                           centroids.rad = input$cen_rad,
                           centroids.detail = cen_detail_val(),
                           inst.rad = input$inst_rad,
                           outliers.method = out_type_val(),
                           outliers.mtp = input$out_mtp,
                           outliers.td = input$out_td,
                           outliers.size = input$out_size,
                           zeros.rad = input$zero_rad,
#PROVIDE THESE-!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!           
#                           capitals.ref = ,
#                           centroids.ref = ,
#                           country.ref = ,
#                           inst.ref = ,
#                           seas.ref = ,
#                           urban.ref = ,
                           value = value_input_val(),
                           verbose = FALSE,
                           report = FALSE
    )
    return(dat)
  })
  
  cen_detail_val <- function(){
    if (input$cen_detail == 1){
      return("country")
    }
    else if(input$cen_detail == 2){
      return("provinces")
    }
    else{
      return("both")
    }
  }
  value_input_val <- function(){
    if (input$value_input == 1){
      return("spatialvalid")
    }
    else {
      return("clean")
    }
  }
  out_type_val <- function(){
    if (input$out_type == 1){
      return("quantile")
    }
    else if (input$out_type == 2){
      return("mad")
    }
    else{
      return("distance")
    }
  }
  
  
#Default Cols? [,c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230)]
#Makes A Downloadable Table for only the columns selected
  download_info <- function(){
    gDat <- gbif_data()
    if(input$table_cols == 1){
      return(gDat[ , as.numeric(input$table_min)])
    }
    else if(input$table_cols == 2){
      return(gDat[ , as.numeric(input$table_def)])
    }
    else if(input$table_cols == 3){
      return(gDat)
    }
    else{
      return(gDat[ , as.numeric(input$table_cus)])
    }
  }
  
  output$download_data <- downloadHandler(
    filename = function(){"gbifDat.csv"},
    content = function(file){
      write.csv(download_info(), file)
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