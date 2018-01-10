#
# This is a Shiny web application to assist in the selection of sites for species undert the "Save our Species" programme. You can run the application by clicking
# the 'Run App' button above.

#############
#change the maximum file size
#############
options(shiny.maxRequestSize=30*1024^2)#change the maximum file size


library(shiny)
library(leaflet)

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
        selectInput("lat_column", "Column with latitude:", choices = c("wait","for","it")), 
        selectInput("long_column", "Column with longitude:", choices = NULL),
        selectInput("spec_column", "Column with species names:", choices = NULL)
        
        # selectInput("species", label = "Select species:",
        #             choices = sort(sp$Scientific.name), multiple=TRUE, selectize=FALSE, 
        #             selected = sort(sp$Scientific.name)[1])
        # 
      ),
      
  # Show a plot of the generated distribution
  mainPanel(
      textOutput("table_display"),  ###TODO change to map
      leafletOutput("mymap")
      )
   )
)

# define where data is and what columns to use

####TO DO - rename lat and long column by the values that were selected
####To DO - interactively select species and subset data based on this
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
    
    vars <- names(df)
    #spec<-unique(df$)
    
    # Update select input immediately after clicking on the action button.
    updateSelectInput(session, "lat_column", "Column with latitude:", 
                      choices = vars, selected="")
    updateSelectInput(session, "long_column", "Column with longitude:", 
                      choices = vars, selected="")
    updateSelectInput(session, "spec_column", "Column with species names:", 
                      choices = vars, selected="")
    
    
    return(df)
  })

  
  output$table_display <- renderText({
    f <- info()
    names(f)
  })
  
######################  Map   ######################
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
####################################################
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

