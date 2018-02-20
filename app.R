
#TODO: add warning to "Environmental variation" tab - envrironmental data must be extractd first - m
      #make check that species name in environmental data matches the extracted data  - the environmental data has to go away if the species name or managment species name is changed

#TODO: ideally you should interactively be able to open shapefile, 
        #this does not seem to be possible in shiny at this time
        #SOS managment site files sits in folder in same location as app, this must be maintained for app to work
#TODO: and symbol to show when data is being read in and when fetch enviro data is working
#TODO: make population warning pop up when there are too few populations"*For the purposes of this decision framework, where populations total less than five, it is recommended that all sites are managed."
#TODO make rainfall variability stacked data
#TODO: clip data to NSW extents?
options(shiny.maxRequestSize=30*1024^2)#change the maximum file size

source("AppFunctions/extractEnviroData.R", local = T)
source("AppFunctions/plotEnviroHists.R", local = T)


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
                                      
                                      # Show a plot of the observations
                                      mainPanel(
                                        leafletOutput("mymap"),
                                        absolutePanel(top = 45, right = 20, width = 150, draggable = TRUE,
                                                      selectInput("bmap", "Select base map", 
                                                                  choices =  c("Base map",
                                                                               "Satellite imagery"), 
                                                                  selected = "Base map")),
                                        
                                         
                                          h3(strong(div(textOutput("sp_warning"), style="color:red"))),
                                          h4(strong("The species you have selected is:")),
                                          h4(strong(em(textOutput("selected_sp")))),
                                          
                                         textOutput("obs_number")
                                        # textOutput("nsw_sites")
                                    
                                        
                                      )
                                    )
                                    
                           ),
                           tabPanel("Number of populations",
                                    #add text about determining the number of populations
                                    h4(strong("CRITERIA 1: NUMBER OF POPULATIONS IN NSW")),
                                    h5("How many populations/sites should be managed to maximise likelihood of long-term viability?"),
                                    h5("Should all known locations of the species be managed?"),
                                    br(),
                                    strong("Populations are defined as ‘geographically or otherwise distinct groups of individuals within the same species, between which there is little demographic or genetic exchange (typically one successful migrant individual or gamete per year or less’ (NSW Scientific Committee 2014))."),
                                    br(),
                                    br(),
                                    p("The determination of what constitutes the optimal number of managed populations and the difference between the terms ‘effective population size’ and ‘population size’ (N) also needs to be defined. There is no general optimal number of populations. However the IUCN consider a species: ‘vulnerable’ if ≤ 10 locations: ‘endangered’ if ≤ 5 locations and ‘critically endangered if a single location (IUCN Standards and Petitions Subcommittee 2014)."),
                                    br(),  
                                    selectInput("pop_number", "Number of populations:", choices = NULL),
                                    br(), 
                                    strong("*For the purposes of this decision framework, where populations total less than five, it is recommended that all sites are managed.") 
                                    
                           ) # Sidebar with where you load csv file and select columns 
                           
                           ,
                           tabPanel("Environmental variation",
                                    h4(strong("Current environmental conditions"),
                                    plotOutput(outputId = "currentPlot",height = "600px"),
                                    br(),
                                    h4(strong("Future climatic conditions"),
                                    plotOutput(outputId = "futurePlot",height = "300px")
                                  
                                    )  
                                    )  
                           ),
                           
                           
                           tabPanel("Site Selection"
                                    
                                    
                                    
                                    #add text of findings and button to print out pdf
                           ),
                           
                           
                           tabPanel("Summary"
                                    #add text of findings and button to print out pdf
                           )
                           
                           #add histograms
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
  
  ############ data selection summary
  
  
  output$selected_sp <- renderText({
    req(input$species)
    input$species
  })
  
  output$sp_warning <- renderText({
    req(input$species)
    req(input$SOSspecies)
    if(grepl(input$species,input$SOSspecies) != TRUE) {
      "Species selected and management site species do not match"
    }
  })
  
  output$obs_number <- renderText({
    req(input$env)
    req(input$SOSspecies)
    Env<-EnvDat()
    obsCount<-nrow(Env)
    sosCount<-nrow(subset(Env,!is.na(Env$SiteName)))
    sosPer<-round(sosCount/obsCount*100,2)
    paste0("The total number of observations is ",
           obsCount,
           " and ",
           sosPer, 
           "% of observations are within managent sites." )
    
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
    
    #get base map name
    
    if(input$bmap== "Base map"){
      mapType<-"OpenStreetMap.Mapnik"
    }
    if(input$bmap== "Satellite imagery"){
      mapType<-"Esri.WorldImagery"
    }
    
    
    #select site data
    sp<-input$SOSspecies
    SPsite <- sites[sites$SciName == sp,]
    
    #main map
    leaflet() %>%
      addProviderTiles(mapType) %>%
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
  
  output$currentPlot <- renderPlot({
    Env<-EnvDat()
    CurClimPlot(Env,subset(Env,!is.na(Env$SiteName)))
  })
  
  output$futurePlot <- renderPlot({
    Env<-EnvDat()
    futClimPlot(Env,subset(Env,!is.na(Env$SiteName)))
  })
  

  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)