#
# This is a Shiny web application to assist in the selection of sites for species undert the "Save our Species" programme. You can run the application by clicking
# the 'Run App' button above.

options(shiny.maxRequestSize=30*1024^2)#change the maximum file size

#source("www/R/plotWindrose.R", local = T)


library(shiny)
library(leaflet)
library(Hmisc)
library(raster)
library(sp)
library(rgdal)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("SOS site selection tool"),
   
   # Sidebar with where you load csv file and select columns 
   sidebarLayout(
      sidebarPanel(
        fileInput("uploadedfile", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv", ".xlsx", ".xls")),
        selectInput("spec_column", "Column with species names:", choices = NULL),
        selectInput("species", "Select species:", choices = NULL),
        selectInput("lat_column", "Column with latitude:", choices = NULL), 
        selectInput("long_column", "Column with longitude:", choices = NULL),
        actionButton("env", "Extract environmental data")
        
      ),
      
  # Show a plot of the generated distribution
  mainPanel(
      leafletOutput("mymap"),
      absolutePanel(top = 25, right = 20, width = 150, draggable = TRUE,
                    selectInput("bmap", "Select base map", 
                                choices = c("Esri.WorldImagery",
                                           "OpenStreetMap.Mapnik"), 
                                selected = "OpenStreetMap.Mapnik"))
      )
   )
)


########################

# define where data is and what columns to use

server <- function(input, output,session) {
   
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
    vars <- names(df)#vector of column names
    # Identify columns interested in
    updateSelectInput(session, "spec_column", "Column with species names:", 
                      choices = vars, selected="")
    updateSelectInput(session, "lat_column", "Column with latitude:", 
                      choices = vars, selected="")
    updateSelectInput(session, "long_column", "Column with longitude:", 
                      choices = vars, selected="")
    return(df)#return the data
  }) 
  

  #select species name
  outVar2 <- reactive({
    f <- info()
    if(is.null(input$spec_column))return()
    sort(unique(f[, input$spec_column])) 
  })
  observeEvent(input$spec_column, {
    updateSelectInput(session, "species", choices = outVar2())
  })
  
  # Reactive expression for data subsetted based on species user has selected
  spData <- reactive({
    f <- info()
    if(is.null(input$species))return()
    f$SPECIES <- f[, input$spec_column]
    f2<- subset(f,SPECIES == input$species)
    return(f2)
  })
  
  
######################  Map   ######################
 output$mymap<- renderLeaflet({
    #runif(input$lat_column)
    #runif(input$long_column)
   # #if(is.null(input$lat_column))return()
   #if(is.null(input$long_column))return()
    spdat<-spData()
    spdat$lat <- spdat[, input$lat_column]
    spdat$long <- spdat[, input$long_column]

    
     leaflet() %>%
    #   addTiles() %>%
    #isolate(leaflet() %>%
              addProviderTiles(input$bmap) %>%
      addCircles(spdat$long, spdat$lat,
                 opacity = .7,
                 fill = TRUE,
                 popup = paste(
                   '<strong>Site:</strong>', capitalize(as.character(spdat$Descriptio)), '<br>',
                   '<strong>Accuracy (m):</strong>', spdat$Accuracy,'<br>',
                   '<strong>Date of last observation:</strong>', spdat$DateLast,'<br>',
                   '<strong>Number of individuals:</strong>', spdat$NumberIndi))%>%
      addMiniMap(tiles = providers$Esri.WorldStreetMap,
                 toggleDisplay = TRUE,
                 position = "bottomleft")

  })
####################################################
#Extrat Environmental data
###################################################
  
  EnvDat <- eventReactive(input$env, {
    #runif(input$lat_column)
    #runif(input$long_column)
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

