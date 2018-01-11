
#df<-read.csv("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/SpeciesObservations/BionetRecords/obsSmall2.csv")

library(shiny)
library(leaflet)
library(Hmisc)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("SOS data loader"),
  
  # Sidebar with where you load csv file and select columns 
  sidebarLayout(
    sidebarPanel(
      fileInput("uploadedfile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xlsx", ".xls")),
      selectInput("spec_column", "Column with species names:", choices = NULL),
      selectInput("species", "Select species:", choices = "")
    ),
    
  # Show a plot of the generated distribution
  mainPanel(
      leafletOutput("mymap")
    )
  )
)


server = function(input, output, session){
  
  #get dataset
  info <- eventReactive(input$uploadedfile, {
    req(input$uploadedfile)
    # read in data 
    fext <- tools::file_ext(input$uploadedfile)[1]#get extension
    if(fext == "csv"){
      df <- read.csv(input$uploadedfile$datapath)
    } 
    if(fext %in% c("xls","xlsx")){
      df <- as.data.frame(read_excel(input$uploadedfile$datapath))
    }
    outVar<-names(df)
    updateSelectInput(session, "spec_column", "Column with species names:", 
                      choices = outVar, selected="")
    
    return(df)
  })
  
  
  #Based on the column selected get the species names
  # i want this to update the values of the selected variable
  outVar2 <- reactive({
    f <- info()
    if (input$spec_column == "") return()
    sort(unique(f[, input$spec_column])) #####should I refer to me data as info?
  })
  #
  
  observeEvent(input$spec_column, {
    updateSelectInput(session, "species", choices = outVar2())
  })
  
  
  output$mymap<- renderLeaflet({
    df<-info()
    leaflet() %>%
      addTiles() %>%
      addCircles(df$Longitude_, df$Latitude_G,
                 opacity = .7,
                 fill = TRUE,
                 popup = paste(
                   '<strong>Site:</strong>', capitalize(as.character(df$Descriptio)), '<br>',
                   '<strong>Accuracy (m):</strong>', df$Accuracy,'<br>',
                   '<strong>Date of last observation:</strong>', df$DateLast,'<br>',
                   '<strong>Number of individuals:</strong>', df$NumberIndi))%>%
      addMiniMap(tiles = providers$Esri.WorldStreetMap,
                 toggleDisplay = TRUE,
                 position = "bottomleft")
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
  