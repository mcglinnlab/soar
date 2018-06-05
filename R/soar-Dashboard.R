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
    textInput("species_name", "Species name", "Caretta caretta"),
    textInput("rank", "Taxonomic rank", "species"),
    
    #get login information
    textInput("gbif_username", "Gbif Username", "Ex: GbifUser1313"),
    textInput("gbif_pass", "Gbif Password", "Ex: GbifPass2424"),
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
      
    actionButton("do", "Submit")
  ),
  
  #show results here
  dashboardBody(
    tabsetPanel(
      tabPanel("Map",  h4("Please allow a few minutes to retrieve data after pressing 'Submit'"),
               leafletOutput("world_map")),
      tabPanel("Raw Data", DT::dataTableOutput("raw_data")),
      tabPanel("Download table", 
              # h2("Ability to download the table will be added here"),
               radioButtons("tableCols", label = "Columns in Downloadable table",
                                  choices = list("Default" = 1, "All Columns" = 2, "Custom" = 3), 
                                  selected = 1),
               conditionalPanel(condition = "input.tableCols == 1",
                                checkboxGroupInput("tableDef", label= h4("Default Options:"), choices = list("Refrences" = 43, "Rights" = 47,
                                                   "Rights holder" = 48, "Institution Code" = 60, "Information Withheld" = 65,
                                                   "Catalog Number" = 69, "Sex" = 75, "Life Stage" = 76, "Verbatim Event Date" = 106,
                                                   "Country Code" = 121, "Decimal Latitude" = 133, "Decimal Longitude" = 134, 
                                                   "Coordinate Unceartanty" = 135, "Taxon ID" = 175, "Scientific Name" = 183,
                                                   "Kingdom" = 191, "Phylum" = 192, "Class" = 193, "Order" = 194, "Family" = 195, 
                                                   "Genus" = 196, "Subgenus" = 197, "Specific Epithet" = 198, 
                                                   "Infraspecific Epithet" = 199, "Taxon Rank" = 200, "Dataset Key" = 207, 
                                                   "Has Geospatial Issues" = 219, "Species" = 229, "Generic Name" = 230
                                                   ),
                                                   selected = c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230))
                                ),
               conditionalPanel(condition = "input.tableCols == 2",
                                h4("Table will contain all 235 columns.")),
               conditionalPanel(condition = "input.tableCols == 3",
#ADD                            #checkboxes with 235 options
                                checkboxGroupInput("tableCus", label= h4("Select Options (Defaults Options Selected):"), choices = list(
                                  "Gbif ID" = 1, "Abstract" = 2, "Access Rights" =  3, "AccrualMethod" = 4, "Accrual Periodically" = 5, 
                                  "Accrual Policy" = 6, "Alternative" = 7, "Audience" = 8, "Available" = 9, "Bibliographic Citation" = 10,
                                  "Conforms To" = 11, "Contributer" = 12, "Coverage" = 13, "Created" = 14, "Creator" = 15, "Date" = 16, 
                                  "Date Accepted" = 17, "Date Copyrighted" = 18, "Date Submitted" = 19, "Description" = 20, "Education Level" = 21, 
                                  "Extent" = 22, "Format" = 23, "Has Format" = 24, "Has Part" = 25, "Has Version" = 26, "Identifier" = 27,
                                  "Instructional Method" = 28, "Is Format Of" = 29, "Is Part Of" = 30, "Is Referenced By" = 31,
                                  "Is Replaced By" = 32, "Is Required By" = 33 ,"Is Version Of" = 34, "Issued" = 35, "Language" = 36, 
                                  "Licence" = 37, "Mediator" = 38, "Medium" = 39, "Modified" = 40, "Provenance" = 41,
                                  "Publisher" = 42, "References" = 43, "Relation" = 44, "Replaces" = 45, "Requires" = 46,
                                  "Rights" = 47, "Rights Holder" = 48, "Source" = 49, "Spatial" = 50, "Subject" = 51, 
                                  "Table Of Contents" = 52, "Temporal" = 53, "Title" = 54, "Type" = 55, "Valid" = "56", "Institution ID" = 57,
                                  "Collection ID" = 58, "Dataset ID" = 59, "Institution Code" = 60, "Collection Code" = 61, 
                                  "Dataset Name" = 62, "Owner Institution Code" = 63, "Basis Of Record" = 64, 
                                  "Information Withheld" = 65, "Data Generalization" = 66, "Dynamic Properties" = 67, "Occurrence ID" = 68,
                                  "Catalog Number" = 69, "Record Number" = 70, "Recorded By" = 71, "Individual Count" = 72,
                                  "Oranism Quantity" = 73, "Organism Quantity Type" = 74, "Sex" = 75, "Life Stage" = 76,
                                  "Reproductive Condition" = 77, "Behavior" = 78, "Establishment Means" = 79, "Occurrence Status" = 80,
                                  "Preparations" = 81, "Disposition" = 82, "Associated References" = 83, "Associated Sequences" = 84, 
                                  "Associated Taxa" = 85, "Other Catalog Numbers" = 86, "Occurrence Remarks" = 87, "OrganismID" = 88, 
                                  "Organism Name" = 89, "Organism Scope" = 90, "Associated Occurrences" = 91, "Associated Organisms" = 92,
                                  "Previous Identifications" = 93, "Organism Remarks" = 94, "Material Sample ID" = 95, "Event ID" = 96,
                                  "Parent Event ID" = 97, "Field Number" = 98, "Event Date" = 99, "Event Time" = 100, "Start Day Of Year" = 101,
                                  "End Day of Year" = 102, "Year" = 103, "Month" = 104, "Day" = 105, "Verbatim Event Date" = 106, "Habitat" = 107,
                                  "Sampling Protocal" = 108, "Sampling Effort" = 109, "Sample Size Value" = 110, "Sample Size Unit" = 111,
                                  "Field Notes" = 112, "Event Remarks" = 113, "Location ID" = 114, "Higher Geography ID" = 115, "Higher Geography" = 116,
                                  "Continent" = 117, "Water Body" = 118, "Island Group" = 119, "Island" = 120, 
                                  "Country Code" = 121, "State Province" = 122, "Country" = 123, "Municipality" = 124,
                                  "Locality" = 125, "Verbatim Locality" = 126, "Verbatim Elevation" = 127,
                                  "Verbatim Depth" = 128, "Minimum Distance Above Surface In Meters" = 129, 
                                  "Maximum Distance Above Surface In Meters" = 130, "Location According To" = 131,
                                  "Location Remarks" = 132, "Decimal Latitude" = 133, "Decimal Longitude" = 134, 
                                  "Coordinate Unceartainty In Meters" = 135, "Coordinate Precision" = 136, 
                                  "Point Radius Spatial Fit" = 137, "Verbatim Coordinate System" = 138, "Verbatim SRS" = 139,
                                  "Footprint WKT" = 140, "Footprint SRS" = 141, "Footprint Spatial Fit" = 142,
                                  "Georeference By" = 143, "Georeference Date" = 144, "Georeference Protocal" = 145, 
                                  "Georeference Sources" = 146, "Georeference Verification Status" = 147, 
                                  "Georeference Remarks" =  148, "Geological Context ID" = 149, "Earliest Eon or Lowest Eonothem" = 150,
                                  "Latest Eon Or Highest Enothem" = 151, "Earliest Era Or Lowest Erathem" = 152, "Latest Era Or Highest Erathem" = 153,
                                  "Earliest Period or Lowest System" = 154, "Latest Period Or Highest System" = 155, 
                                  "Earliest Epoch Or Lowest Series" = 156, "Latest Epoch or Highest Series" = 157, 
                                  "Earliest Age Or Lowest Stage" = 158, "Latest Age Or Highest Stage" = 159,
                                  "Lowest Biostratigraphic Zone" = 160, "Highest Biostratigraphic Zone" = 161, 
                                  "Lithostratigraphic Terms" = 162, "Group" = 163, "Formation" = 164, "Member" = 165,
                                  "Bed" = 166, "Identification ID" = 167, "Identification Qualifier" = 168, "Type Status" = 169,
                                  "Identified By" = 170, "Date Identified" = 171, "Identification References" = 172, 
                                  "Identification Verification Status" = 173, "Identification Remarks" = 174, 
                                  "Taxon ID" = 175, "Scientific Name ID" = 176, "Accepted Name Usage ID" = 177, 
                                  "Parent Name Usage ID" = 178, "Origional Name Usage ID" = 179, "Name According To ID" = 180,
                                  "Name Published In ID" = 181, "Taxon Concept ID" = 182, "Scientific Name" = 183, 
                                  "Accepted Name Usage" = 184, "Parent Name Usage" = 185, "Origional Name Usage" = 186,
                                  "Name According To" = 187, "Name Published In" = 188, "Name Published In Year" = 189,
                                  "Higher Classification" = 190, "Kingdom" = 191, "Phylum" = 192, "Class" = 192, "Order" = 193, 
                                  "Family" = 195, "Genus" = 196, "Subgenus" = 197, "Specific Epithet" = 198, 
                                  "Infraspecific Epithet" = 199, "Taxon Rank" = 200, "Verbatim Taxon Rank" = 201, 
                                  "Vernacular Name" = 202, "Nomenclatural Code" = 203, "Toxonomic Status" = 204, 
                                  "Nomeclatural Status" = 205, "Taxon Remarks" = 206, "Dataset Key" = 207, "Publishing Country" = 208,
                                  "Last Interpreted" = 209, "Elevation" = 210, "Elevation Accuracy" = 211, "Depth" = 212,
                                  "Depth Accuracy" =  213, "Distance Above Surface" = 214, "Distance Above Surface Accuracy" = 215,
                                  "Issue" = 216, "Media Type" = 217, "Has Coordinate" = 218, "Has Geospatial Issues" = 219,
                                  "Taxon Key" = 220, "Kingdom Key" = 221, "Phylum Key" = 222, "Class Key" =  223, 
                                  "Order Key" = 224, "Family Keys" = 225, "Genus Key" = 226, "Subgenus Key" = 227, 
                                  "Species Key" = 228, "Species" = 229, "Generic Name" = 230, "Typified Name" = 231, 
                                  "Protocal" = 232, "Last Parsed" = 233, "Last Crawled" = 234, "Repatriated" = 235
                                ),selected = c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230))
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
    if (input$latLongcheckbox){
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
      res = occ_download(paste("country =", country), 
                         paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                         user = input$gbif_username, pwd = input$gbif_pass, 
                         email = input$gbif_email)
    }
    else{
      res = occ_download(paste("taxonKey =", sp_key), 'hasCoordinate = TRUE',
                         user = input$gbif_username, pwd = input$gbif_pass, 
                         email = input$gbif_email)
    }
    
#loops so that it checks every 30 seconds to see if meta$status is "SUCCEEDED" or "KILLED"
    continue = TRUE
    while(continue){
      meta = occ_download_meta(res)
       if (meta$status == "SUCCEEDED"){
        continue = FALSE
        dat <- occ_download_get(res[1], overwrite = TRUE) %>%
          occ_download_import()
        return (dat)
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
  output$raw_data <- DT::renderDataTable(expr = gbif_data(),options=list(autoWidth = TRUE,scrollX=TRUE))
  
#For Tab 3
#Default Cols? [,c(43,47,48,60,65,69,75,76,106,121,133:135,175,183,191:200,207,219,229,230)]
#Make A Downloadable Table for only the columns selected
  
  
  output$world_map <- renderLeaflet({
    leaflet(gbif_data()) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      
      addCircleMarkers(lng = ~ decimalLongitude, lat = ~ decimalLatitude,
                       radius = 3,  #~ifelse(quality_grade == "research", 6, 3),
                       color = 'red',  #~pal(quality_grade),
                       stroke = FALSE, fillOpacity = 0.5
      )# %>%
#Remove This, there are no longer 'grades' to the observation
      #addLegend("bottomright", pal = pal, values = ~quality_grade,
      #          labels = c('casual', 'needs id', 'research'),
      #          title = "The quality of the records"
      #)
  })
}

shinyApp(ui, server)