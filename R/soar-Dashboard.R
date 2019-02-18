library(digest)
library(htmltools)
library(mime)
library(shiny) #Display
library(shinydashboard)  #Display
library(ggplot2) #for the plots
library(DT)  #For the tables
library(leaflet)  #Maps
library(jsonlite)  
library(rgbif)  #GBIF data
library(raster)  #for working with rasters
library(shinycssloaders)  #This makes the loading symbols
library(CoordinateCleaner)  #Cleans the Data
library(rnaturalearth)  #Maps for leaflet

choice <- function(input_num) {
  Gbif_fields <- read.csv("../data/gbif_fields.csv", as.is = TRUE)
  Country_codes <- read.csv("../data/countryCodes.csv", as.is = TRUE)
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
  else if (input_num == 3){
    lis<-Gbif_fields$Column.Name[Gbif_fields$Custom == 1]
    selected <- Gbif_fields$Custom
  }
  else {
    lis <- Country_codes$CountryCodes
    selected <- Country_codes$CountryCodes
    lis2 <- Country_codes$CountryCodes
    final_list <- list()
    for (i in 1:304){
      final_list[[lis[i]]] <- lis2 [i]
    }
    return(final_list)
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
    selectInput("rank", "Taxonomic rank", 
                choices = list("Species" = "Species","Genus" = "Genus", "Family" = "Family","Order" = "Order", 
                               "Class" = "Class", "Phylum" = "Phylum", "Kingdom" = "Kingdom"), selected = "Species"),
    
    #get login information
    textInput("gbif_username", "Gbif Username", "Ex: GbifUser1313"),
    passwordInput("gbif_pass", "Gbif Password", "Ex: GbifPass2424"),
    textInput("gbif_email", "Gbif Email", "Ex: JaneSmith@generic.com"),
    
    #input latlong bounding box
    checkboxInput("lat_long_check_box", label = "Input Geographical Boundaries with Latitude and Longitude", 
                  value = FALSE),
    conditionalPanel (
      condition = "input.lat_long_check_box == true && input.input_file_checkbox == false && input.down_key_checkbox == false",
      numericInput("Lat_low", "Latitude lower range", "-90", min = -90, max = 90),
      numericInput("Lat_high", "Latitude upper range", "90", min = -90, max = 90),
      numericInput("Long_low", "Longitude lower range", "-180", min = -180, max = 180),
      numericInput("Long_high", "Longitude upper range", "180", min = -180, max = 180)
    ),
    
    #input bounding box with political boundaries
    checkboxInput("political_checkbox", label = "Input Geographical boundaries by country"),
    conditionalPanel(
      condition ="input.political_checkbox == true && input.input_file_checkbox == false && input.down_key_checkbox == false",
      selectInput("political_boundary", "Country Name", choices = choice(4), selected = "US")
      ),
   
     #input date bounding information for gbif
    checkboxInput("date_checkbox", label = "Sort by date", value = FALSE),
    conditionalPanel (
      condition = "input.date_checkbox == true && input.input_file_checkbox == false && input.down_key_checkbox == false",
      dateInput("from_date", "Show results from this date:"),
      dateInput("to_date", "to this date:")
    ),
  
    #input specific download key
    checkboxInput("down_key_checkbox", label = "Specify Download Key", value = FALSE),
    conditionalPanel (
      condition = "input.down_key_checkbox == true && input.input_file_checkbox == false",
      textInput("down_key", "Download Key:", "" )
    ),
    
    #input file name to import
    checkboxInput("input_file_checkbox", label = "Import Gbif Data File", value = FALSE),
    conditionalPanel(
      condition = "input.input_file_checkbox == true",
      textInput("file_name", "Gbif Data File(format: file_name.zip):" ,"")
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
                #capitals.ref -- works fine on its own
                #centroids.ref -- works fine on its own
                #country.ref -- Works fine with rnaturalearth added in
                #inst.ref -- works fine on its own
                #seas.ref -- Works fine on its own
                #urban.ref
               selectInput("value_input", "What kind of output would you like?", 
                           choices = list("Spatial Valid - A separate document with what data was flagged and for what test" = 1, "Clean - the flagged data is automatically removed" = 2), selected = 1),
               #verbose = False
              actionButton("do_clean", "Submit")
               ),
      tabPanel("Detect Bias",
               h4("Temporal Bias", h5("Information can become less accurate depending on when it was collected. These percentages are for refrence in determining the accuracy of your dataset."),
                  h6(textOutput("pre1950"),"% of the dataset was collected before 1950"),
                  h6(textOutput("pre1970"),"% of the dataset was collected before 1970"),
                  h6(textOutput("pre1990"),"% of the dataset was collected before 1990")),
                  h5(), #For a new line
                  h5("The plots below shows the number of observtions in the current dataset per year compared with the 
                     number of observations reported to Gbif in total per year. Take this into consideration when comparing
                     the number of observations from one year to another. The red line is the fetched dataset while the black 
                     line is the total number of observations for that year.", withSpinner(plotOutput("temporal_bias_plot"))),
                  h5(withSpinner(plotOutput("temporal_bias_comparison_plot") )),
               h4("Spatial Bias"),
                  h5("The map below shows the occurrences in the selected dataset (black) overlaid with all 
                     occurrences in the Gbif database (red). Take this into consideration when 
                     comparing how many occurrences are reported in one area rather than another.",
                     withSpinner(leafletOutput("spatial_bias_map")) ),
                  h5("The plot below shows the correlation between the number of observations
                     per map pixel for the selected dataset and the number of observations per
                     map pixel for the overall Gbif dataset. Take this into consideration when 
                     comparing how many occurrences are reported in one area rather than another.
                     Note that, due to the fact that any data pulled from Gbif is a subset of their
                     database, no points can appear to the left of the line drawn",
                     withSpinner(plotOutput("spatial_bias_plot")))),
      tabPanel("Download Cleaned Data", 
               h4("'True' means the data passed the tests indicated, 'False' means it failed"),
               downloadButton('download_clean_data', label = "Download Table"),
               withSpinner(DT::dataTableOutput("clean_data")))
    )
  )
)
#server- logic of the app
server <- function(input, output) {
  
  gbif_data <- eventReactive(input$do, {
    #import file
    if (input$input_file_checkbox){
    data_file_name = input$file_name
    }
    #filter by name
    sp_key <- name_suggest(q =input$species_name, rank = input$rank)$key[1]
    #filter by country
    if (input$political_checkbox){ country <- input$political_boundary}
    #filter by date
    if (input$date_checkbox){
      to_m <- substr(input$to_date, 6,7)
      from_m<- substr(input$from_date, 6,7)
      to_y <- substr(input$to_date, 1, 4)
      from_y <- substr(input$from_date, 1, 4)
    }
    else{
      to_m <- substr(Sys.Date(), 6,7)
      from_m<- "1"
      to_y <- substr(Sys.Date(), 1, 4)
      from_y <- "1600"
    }
    #filter by lat/long
    if (input$lat_long_check_box){
      if (is.null(input$Lat_low ) ||  is.null(input$Lat_high) ||  is.null(input$Long_low) || is.null(input$Long_high)){
        low_lat <- -90
        high_lat <- 90
        low_long <- -180
        high_long <- 180
      }
      else{
      low_lat <- input$Lat_low
      high_lat <- input$Lat_high
      low_long <- input$Long_low
      high_long <- input$Long_high
    }
    }
    else {
      low_lat <- -90
      high_lat <- 90
      low_long <- -180
      high_long <- 180
    }
    
    if (input$input_file_checkbox) {
      #Works, but says file does not exist
      file = substring(data_file_name,1,23)
      dat <- occ_download_import(key = file)
      return(dat)
    }
    else if (input$down_key_checkbox){
      continue = FALSE
      dat <- occ_download_get(key = toString(input$down_key), overwrite = TRUE) %>% occ_download_import()
      return(dat)
    }
    else{
      continue = TRUE
      if (input$political_checkbox){
        res = occ_download(paste("decimalLatitude >=", low_lat), paste("decimalLatitude <=", high_lat),
                           paste("decimalLongitude >=", low_long), paste("decimalLongitude <=", high_long),
                           paste("year >=", from_y), paste("year <=", to_y), paste("month >=", from_m),
                           paste("month <=", to_m), paste("country =", country), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
      else{
        res = occ_download(paste("decimalLatitude >=", low_lat), paste("decimalLatitude <=", high_lat),
                           paste("decimalLongitude >=", low_long), paste("decimalLongitude <=", high_long),
                           paste("year >=", from_y), paste("year <=", to_y), paste("month >=", from_m),
                           paste("month <=", to_m), 
                           paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                           user = input$gbif_username, pwd = input$gbif_pass, 
                           email = input$gbif_email)
      }
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
                           urban = FALSE,
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
                           value = value_input_val(),
 #PROVIDE THESE-?         
                           #capitals.ref = ,
                           #centroids.ref = ,
                           #country.ref = ,
                           #inst.ref = ,
                           #seas.ref = ,
                           #urban.ref = ,
                           verbose = FALSE,
                           report = FALSE
    )
    return(dat)
  })
 
  #Help determine meaningful input for clean_info 
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
  
  #Determine percentages of the dataset that was collected prior to 1990,1970, and 1950
  temporal_bias <- function(num){
    dat = gbif_data()
    #narrow dat down so that it only contains the years
    year = dat$year
    pre1990 = 0
    pre1970 = 0
    pre1950 = 0
    for (i in seq_along(year)) {
      if (year[i] < 1950){
        pre1950 = pre1950 + 1
        pre1970 = pre1970 + 1
        pre1990 = pre1990 + 1
      }
      else if (year[i] < 1970) {
        pre1970 = pre1970 + 1
        pre1990 = pre1990 + 1
      }
      else if (year[i] < 1990) {
        pre1990 = pre1990 + 1
      }
    }
    
    if (num == 1){
    return((pre1990/(length(year)))*100)
    }
    else if (num == 2) {
    (pre1970/(length(year)))*100
    }
    else {
    (pre1950/(length(year)))*100
    }
  }
  #actually pass back the data
  output$pre1950 <- renderText(
    temporal_bias(3)
  )
  output$pre1970 <- renderText(
    return(temporal_bias(2))
  )
  output$pre1990 <- renderText(
    return(temporal_bias(1))
  )
  
  #Currenlty just gets the data for the graph ready
  #will eventually be an output for the data
  temporal_bias_data <-function(num){
    dat <- gbif_data()$year
    currYear <- as.numeric(substring(Sys.Date(),1,4));
    #FILL IN FOR FULL DATA LATER
    if (num == 3){
      #Doing this rather than the whole download sequence allows it to read the 
      #file (of the same name as the key) rather than downloading the dataset again.
      #This also speeds up the code significantly
    fullDat <- occ_download_import(key = "0019554-181003121212138") 
    fullDat <- fullDat$year
    fullDatYear <- list()
    
    for (i in 1:(currYear-1599)) {fullDatYear[i] = 0}
    
    for (i in 1:length(fullDat)){
      curr = as.numeric(fullDat[i])
      fullDatYear[curr - 1599] = (as.numeric(fullDatYear[curr-1599])) + 1
    }
    }
    if (num == 4){
      #FILL IN FOR FULL DATA LATER
      #Doing this rather than the whole download sequence allows it to read the 
      #file (of the same name as the key) rather than downloading the dataset again.
      #This also speeds up the code significantly
      fullDat <- occ_download_import(key = "0019554-181003121212138") 
    }
    datYear <- list()
    years <- list()
    
    for (i in 1:(currYear-1599)) {datYear[i] = 0}
    for (i in 1:(currYear-1599)) {years[i] = i + 1599}
    for (i in 1:length(dat)){
      curr = as.numeric(dat[i])
      datYear[curr - 1599] = (as.numeric(datYear[curr-1599])) + 1
    }

    if (num == 1) {return(years)}
    if (num == 2) {return(datYear)}
    if (num == 3) {return(fullDatYear)}
    if (num == 4) {return(fullDat)}
  }
  
  output$temporal_bias_plot <- renderPlot({
    Year <-temporal_bias_data(1)
    Number_Of_Observations <-temporal_bias_data(3)
    plot(Year, Number_Of_Observations, type = "l")
    lines(Year, temporal_bias_data(2), col = "red")
  })
  
  output$temporal_bias_comparison_plot <- renderPlot({
    Species_Of_Interest_Observations_Per_Year <- temporal_bias_data(2);
    Total_Observations_Per_Year <- temporal_bias_data(3);
    #DOESN'T LOOK RIGHT -- ( Was due to the very small dataset in use)
    plot(Total_Observations_Per_Year, Species_Of_Interest_Observations_Per_Year)
  })
  
  #Output a plot that shows the range of where all Gbif data is from Vs. where 
  #only the selected data is from
  output$spatial_bias_map <- renderLeaflet({
    #Full Data -> temporal_bias_data(4) -- red
    #Sample Data -> gbif_data() -- black
    leaflet(temporal_bias_data(4)) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      
      addCircleMarkers(lng = ~ decimalLongitude, lat = ~ decimalLatitude,
                       radius = 3,
                       color = 'black',
                       stroke = FALSE, fillOpacity = 0.5
      ) %>%
      addCircleMarkers(data = gbif_data(), lng = ~ decimalLongitude, lat = ~ decimalLatitude,
                       radius = 3,
                       color = "red",
                       stroke = FALSE, 
                       fillOpacity = 0.5)
  })
  
  output$spatial_bias_plot <- renderPlot({
    interest <- spatial_bias_raster("selected_sp")
    Total <- spatial_bias_raster("all_sp")
    plot(Total,interest, pch=1, cex=1,
         xlab = "Total Gbif occurrences per cell",
        ylab = "Species of Inerest occurrences per cell")
    dat <- getValues(interest)
    fullDat <- getValues(Total)
    corr = cor(fullDat,dat, use = "complete.obs")
    legend("topleft", paste("r = ", corr),bty = "n")
    abline(a=0,b=1)
  })
  
  #Create a rasta file that can be used in a comparison graph for how many 
  #results are in a cell per gbif vs. how many results are in a cell for 
  #the collected dataset
  spatial_bias_raster <- function(input = c("selected_sp", "all_sp")){
    #create and return the raster files depending on input
    # 1 = downloaded dataset
    # 2 = Full dataset
    if (input == "selected_sp"){
      dat = gbif_data();
      crds = cbind(dat$decimalLongitude, dat$decimalLatitude)
      crds_sp = SpatialPoints(crds, proj4string = CRS("+proj=longlat +ellps=WGS84"))
      r = raster(nrows=116, ncols=364, xmn=-20037.51, xmx=20002.49, ymn=-6396.115,
                 ymx=6363.885, crs = CRS("+proj=cea +units=km +ellps=WGS84"))
      crds_cea = spTransform(crds_sp, CRSobj = CRS(proj4string(r)))
      ct = rasterize(crds_cea, r, fun='count')
      return(ct)
    }
    else {
      dat = temporal_bias_data(4);
      crds = cbind(dat$decimalLongitude, dat$decimalLatitude)
      crds_sp = SpatialPoints(crds, proj4string = CRS("+proj=longlat +ellps=WGS84"))
      r = raster(nrows=116, ncols=364, xmn=-20037.51, xmx=20002.49, ymn=-6396.115,
                 ymx=6363.885, crs = CRS("+proj=cea +units=km +ellps=WGS84"))
      crds_cea = spTransform(crds_sp, CRSobj = CRS(proj4string(r)))
      ct = rasterize(crds_cea, r, fun='count')
      return(ct)
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
  
  output$download_clean_data <- downloadHandler(
    filename = function(){"CleanGbifDat.csv"},
    content = function(file){
      write.csv(clean_info() ,file)
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