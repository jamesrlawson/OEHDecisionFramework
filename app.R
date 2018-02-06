#TODO: fix error in mapping area when app first opened - lat and longs are not defined
#TODO: add warning to "Environmental variation" tab - envrironmental data must be extractd first
#TODO: change base maps names by adding ifelse function in server
#TODO: ideally you should interactively be able to open shapefile
#TODO: and symbol to show when data is being read in and when fetch enviro data is working

options(shiny.maxRequestSize=30*1024^2)#change the maximum file size

source("/Users/daisy/repos/OEHDecisionFramework/AppFunctions/extractEnviroData.R", local = T)
source("/Users/daisy/repos/OEHDecisionFramework/AppFunctions/plotEnviroHists.R", local = T)


library(shiny)
library(leaflet)
library(Hmisc)
library(raster)
library(sp)
library(rgdal)
library(shinythemes)
library(ggplot2)
library(foreign)
library(maptools)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel("SOS site selection tool"),
                navbarPage(title="",
                           tabPanel("Observations",
                                    # Sidebar with where you load csv file and select columns 
                                    sidebarLayout(
                                      sidebarPanel(
                                        fileInput("uploadedfile", "Choose observation file",
                                                  multiple = FALSE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv", ".xlsx", ".xls")),
                                        selectInput("spec_column", "Column with species names:", choices = NULL),
                                        selectInput("species", "Select species:", choices = NULL),
                                        selectInput("lat_column", "Column with latitude:", choices = NULL), 
                                        selectInput("long_column", "Column with longitude:", choices = NULL),
                                        selectInput("SOSspecies", 
                                                    "Select management site species name:", 
                                                    choices=NULL),
                                        actionButton("env", "Press to fetch enviro. data")
                                        
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Map", 
                                                   
                                                   leafletOutput("mymap"),
                                                   absolutePanel(top = 45, right = 20, width = 150, draggable = TRUE,
                                                                 selectInput("bmap", "Select base map", 
                                                                             choices = c("Esri.WorldImagery",
                                                                                         "OpenStreetMap.Mapnik"), 
                                                                             selected = "OpenStreetMap.Mapnik"))
                                                   
                                          ),
                                          
                                          tabPanel("Number of populations",
                                                  #add text about determining the number of populations
                                                  h4(strong("CRITERIA 1: NUMBER OF POPULATIONS")),
                                                  h5("How many populations/sites should be managed to maximise likelihood of long-term viability?"),
                                                  h5("Should all known locations of the species be managed?"),
                                                  br(),
                                                  strong("Populations are defined as ‘geographically or otherwise distinct groups of individuals within the same species, between which there is little demographic or genetic exchange (typically one successful migrant individual or gamete per year or less’ (NSW Scientific Committee 2014)."),
                                                  br(),
                                                  br(),
                                                  p("The determination of what constitutes the optimal number of managed populations and the difference between the terms ‘effective population size’ and ‘population size’ (N) also needs to be defined. There is no general optimal number of populations. However the IUCN consider a species: ‘vulnerable’ if ≤ 10 locations: ‘endangered’ if ≤ 5 locations and ‘critically endangered if a single location (IUCN Standards and Petitions Subcommittee 2014)."),
                                                  br(),  
                                                  selectInput("pop_number", "Number of populations:", choices = NULL),
                                                  br(), 
                                                  strong("*For the purposes of this decision framework, where populations total less than five, it is recommended that all sites are managed.") 
                                                   
                                          ),
                                          
                                          tabPanel("Environmental variation"
                                                    ,
                                                    plotOutput(outputId = "distPlot")
                                          
                                                   #add histograms
                                          ),
                                          tabPanel("Summary"
                                                   #add text of findings and button to print out pdf
                                          )
                                          
                                        )
                                      )
                                    )
                                    
                           ),
                           tabPanel("Site assessment",
                                    # Sidebar with where you load csv file and select columns 
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Managment sites"
                                          ),
                                          tabPanel("Cluster analysis"
                                                   #add histograms
                                          ),
                                          tabPanel("Site summary"
                                                   #add text of findings and button to print out pdf
                                          )
                                        )
                                      )
                                      
                                    )
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
  
  #Select managment site, species name, 
  #****this step insures that if there is differences in the names, then they are resolved***
  
  sites<-readOGR("AppEnvData/ManagmentSites/OEHManagmentSites.shp")
  siteSP<-sort(unique(sites@data$SciName))
  updateSelectInput(session, "SOSspecies", "Select management site species name:",
                      choices = siteSP, selected="")
  
  
  ####### Extract Environmental data and capad ################
  EnvDat <- eventReactive(input$env, {
    req(input$env)
    req(input$SOSspecies)
    spdat<-spData()
    spdat$lat <- spdat[, input$lat_column]
    spdat$long <- spdat[, input$long_column]
    dat<-EnvExtract(spdat$lat,spdat$long)
    
    #select site data
    coords <- dat[,c("long","lat")]
    coordinates(coords) <-c("long","lat")
    proj4string(coords)<-crs(sites)
    
    sp<-input$SOSspecies
    managmentSite <- sites[sites$SciName == sp,]
    dat<-cbind(dat,over(coords,managmentSite,returnList = FALSE))
    return(dat)
  })
  
  
  
  
  ######################  Map   ######################
  output$mymap<- renderLeaflet({
    #runif(input$lat_column)
    #runif(input$long_column)
    # #if(is.null(input$lat_column))return()
    #if(is.null(input$long_column))return()
    # require(input$lat_column)
    # require(input$long_column)
    # 
    #select species data
    spdat<-spData()
    spdat$lat <- spdat[, input$lat_column]
    spdat$long <- spdat[, input$long_column]
    
    #select site data
    sp<-input$SOSspecies
    SPsite <- sites[sites$SciName == sp,]
    
    #main map
    leaflet() %>%
      addProviderTiles(input$bmap) %>%
      #location of managment sites
      addCircles(spdat$long, spdat$lat,#locations of species
                 fill = TRUE,
                 radius = 10)%>%
      addPolygons(data=SPsite, weight = 3, color = "red", fillColor = "red", opacity = 0.5)%>%
      addCircles(spdat$long, spdat$lat,#add the points again but make them clear, this allows for popup of info
                 fill = FALSE,
                 color = "#00ff0001",
                 opacity = 0.7,
                 radius = 500,
                 popup = paste(
                   '<strong>Site:</strong>', capitalize(as.character(spdat$Descriptio)), '<br>',
                   '<strong>Accuracy (m):</strong>', spdat$Accuracy,'<br>',
                   '<strong>Date of last observation:</strong>', spdat$DateLast,'<br>',
                   # '<strong>Number of individuals:</strong>', paste(spdat$NumberIndi, "- A value of 0 indicates unknown number of individuals.")))%>%
                   '<strong>Number of individuals:</strong>', 
                   ifelse(spdat$NumberIndi==0, 
                          "Unknown number of individuals",
                          spdat$NumberIndi)))%>%
      #map in bottom left corner
      addMiniMap(tiles = providers$Esri.WorldStreetMap,
                 toggleDisplay = TRUE,
                 position = "bottomleft")
    
  })

  
  
   
  
  ################ Population number #################
  popNumber<-c(1:5,">5 and <10",">= 10 and < 20", ">= 20")
  updateSelectInput(session, "pop_number", "Number of populations:", 
                    choices = popNumber, selected="")
  
  ################ Environmental variation ###########
  
  # #elevation
  #  allEl<-EnvDat$elev
  #  subset(EnvDat,!is.na(EnvDat$SiteName))
  # # 
  
  output$distPlot <- renderPlot({
    env<-EnvDat()
    
    # allSite<-EnvDat["elev"]
    # sosSite<-subset(EnvDat,!is.na(EnvDat$SiteName))["elev"]
    # 
    # #elevation
    # allEl<-EnvDat["elev"]
    # subset(EnvDat,!is.na(EnvDat$SiteName))["elev"]
    # 
    EnvPlot(env["elev"],subset(env,!is.na(env$SiteName))["elev"])
    
 

    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
