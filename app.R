#TODO automate the extraction of environmental data, make it reactive
#TODO: add warning to "Environmental variation" tab - envrironmental data must be extractd first - m
#make check that species name in environmental data matches the extracted data  - the environmental data has to go away if the species name or managment species name is changed

#TODO: and symbol to show when data is being read in and when fetch enviro data is working
#TODO: make population warning pop up when there are too few populations"*For the purposes of this decision framework, where populations total less than five, it is recommended that all sites are managed."
#TODO: clip data to NSW extents?
#TODO: add historical data
#TODO: on tab 4 it says "cluster" above the map
options(shiny.maxRequestSize=30*1024^2)#change the maximum file size

source("AppFunctions/extractEnviroData.R")
source("AppFunctions/plotEnviroHists.R")
source("AppFunctions/ClusterAnalysis.R")
source("AppFunctions/findLocations.R")


library(shiny)
library(leaflet)
library(leaflet.extras)
library(Hmisc)
library(raster)
library(sp)
library(rgdal)
library(shinythemes)
library(ggplot2)
library(foreign)
library(maptools)
library(ggplot2)
library(lemon)
library(cluster)
library(shinycssloaders)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                titlePanel("SOS site selection tool"),
                navbarPage(title="",
                           tabPanel("1. Observations",
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
                                       actionButton("env", "Load environmental data")
                                        
                                      ),
                                      
                                      # Show a plot of the observations
                                      mainPanel(
                                        leafletOutput("mymap"),
                                        h3(strong(div(textOutput("sp_warning"), style="color:red"))),
                                        h4(strong("The species you have selected is:")),
                                        h4(strong(em(textOutput("selected_sp")))),
                                        
                                        textOutput("obs_number")
                                        # textOutput("nsw_sites")
                                        
                                        
                                      )
                                    )
                                    
                           ),
                           tabPanel("2. Number of populations",
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
                           tabPanel("3. Environmental variation",
                                    h4(strong("Current environmental conditions"),
                                       plotOutput(outputId = "currentPlot",height = "600px"),
                                       br(),
                                       h4(strong("Future climatic conditions"),
                                          plotOutput(outputId = "futurePlot",height = "300px")
                                          
                                       )  
                                    )  
                           ),
                           
                           
                           tabPanel("4. Site Selection",
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        strong("Select variables for cluster analysis:"),
                                        checkboxInput("tmax", "Avg. annual Tmax", FALSE),
                                        checkboxInput("rain", "Avg. annual rainfall", FALSE),
                                        checkboxInput("rainVar", "Avg. annual rainfall variability", FALSE), 
                                        checkboxInput("elev", "Elevation", FALSE),
                                        checkboxInput("soils", "Soil type", FALSE),
                                        numericInput('clusters', 'Cluster count',2,
                                                     min = 2, max = 9),
                                        span(textOutput("envVarPrompt"), style="color:darkred")
                                      ),
                                      
                                      mainPanel(
                                        textOutput("clustersText"),
                                        #A variety of metrics exist to help choose the number of clusters to be extracted in a cluster analysis.
                                        #We use silhouette width, an internal validation metric which is an aggregated measure of how similar an
                                        #observation is to its own cluster compared its closest neighboring cluster. The metric can range from
                                        #-1 to 1, where higher values are better.
                                        
                                        #sets location for base leaflet map and make dropdown menu to select the backgroudn map
                                        selectInput("variable", "Display Variable", c(
                                          "Cluster" = "cluster",
                                          "Avg. annual Tmax" = "tmax",
                                          "Avg. annual rainfall" = "rain",
                                          "Avg. annual rainfall variability" = "rainVar",
                                          "Elevation" = "elev",
                                          "Soil type" = "soil"
                                        )),
                                        checkboxInput("incSoS", "Include all SoS sites in hist outputs?", TRUE),
                                        leafletOutput('ClusterPlot'),
                                        plotOutput('SelectedCurrentPlot',height = "600px"),
                                        plotOutput('SelectedFuturePlot',height = "300px"))
                                    )
                                    
                           ),
                           
                           
                           tabPanel("5. Summary"
                                    #add text of findings and button to print out pdf
                           )
                           
                           #add histograms
                )
)




########################

# define where data is and what columns to use

server <- function(input, output,session) {
  
  ############ 1. Observations ###############  
  
  info <- eventReactive(input$uploadedfile, {
    req(input$uploadedfile)
    
    # Get file extension
    fext <- tools::file_ext(input$uploadedfile)[1]
    if(fext == "csv"){
      df <- read.csv(input$uploadedfile$datapath)
    } 
    if(fext %in% c("xls","xlsx")){
      df <- as.data.frame(read_excel(input$uploadedfile$datapath))
    }
    vars <- names(df)
    
    # check for standard column names
    sci <- NA
    if('Scientific' %in% vars) {
      sci <- 'Scientific'
    }
    lat <- NA
    if('Latitude_G' %in% vars) {
      lat <- 'Latitude_G'
    }
    lon <- NA
    if('Longitude_' %in% vars) {
      lon <- 'Longitude_'
    }
    
    # Identify columns interested in
    updateSelectInput(session, "spec_column", "Column with species names:", 
                      choices = vars, selected=sci)
    updateSelectInput(session, "lat_column", "Column with latitude:", 
                      choices = vars, selected=lat)
    updateSelectInput(session, "long_column", "Column with longitude:", 
                      choices = vars, selected=lon)
    
    # Return the data
    return(df)
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
    
    if(isTruthy(input$species) && isTruthy(input$spec_column)){
      f$SPECIES <- f[, input$spec_column]
      f2<- subset(f,SPECIES == input$species)
      return(f2)
    } else {
      return(NULL)
    }
  })
  
  #Select managment site, species name
  sites <- readOGR("AppEnvData/ManagmentSites/OEHManagmentSites.shp")
  siteSP <- sort(unique(sites@data$SciName))
  
  # if species name is found in management sites data, autoselect it in the SOSspecies selector
  observeEvent(input$species, {
    if(input$species %in% siteSP) {
    updateSelectInput(session, "SOSspecies", "Select management site species name:",
                      choices = siteSP, selected=input$species)
    } else {
      updateSelectInput(session, "SOSspecies", "Select management site species name:",
                        choices = siteSP, selected="")
    }
  })
  
  #Error text if species observations and Managment sites species do not match
  output$sp_warning <- renderText({
    req(input$species)
    req(input$SOSspecies)
    if(grepl(input$species,input$SOSspecies) != TRUE) {
      "Species selected and management site species do not match"
    }
  })
  
  #get species name that has been selected to use in description
  output$selected_sp <- renderText({
    req(input$species)
    input$species
  })
  
  #summary text
  output$obs_number <- renderText({
    
    if(isTruthy(input$env) && isTruthy(input$SOSspecies)){
      # req(input$env)
      # req(input$SOSspecies)
      Env <- EnvDat()
      obsCount <- nrow(Env)
      sosCount <- nrow(subset(Env, !is.na(Env$SiteName)))
      sosPer <- round(sosCount/obsCount*100,2)
      sprintf("The total number of observations is %s and %s%% of observations are within management sites.",
              obsCount, sosPer)
    } else {
      "Select species and retrieve environmental data first."
    }
             
  })
  
  #  Map of observations  
  output$mymap<- renderLeaflet({
   
    spdat <- spData()
    
    req(input$long_column, input$lat_column, 
        input$SOSspecies, input$species, input$spec_column)

    
    #  if(nrow(spdat)<1)
    #    return(NULL)
    # #runif(input$lat_column)
    # #runif(input$long_column)
    # if(is.null(input$lat_column))return(NULL)
    # if(is.null(input$long_column))return(NULL)
    # # require(input$lat_column)
    # require(input$long_column)
    # 
    #select species data
    spdat <- spData()
    spdat$lat <- spdat[, input$lat_column]
    spdat$long <- spdat[, input$long_column]
    
    # Select site data
    SPsite <- sites[sites$SciName == input$SOSspecies,]
    
    # merge with SoS mgmt site data
    coords <- spdat[,c("long","lat")]
    coordinates(coords) <-c("long","lat")
    proj4string(coords)<-crs(sites)
    spdat <- cbind(spdat, sp::over(coords,SPsite,returnList = FALSE))
    
    # Main map
    leaflet() %>%
      # addProviderTiles(mapType) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%

      #location of managment sites
      addPolygons(data=SPsite, 
                  weight = 3, 
                  color = "red", 
                  fillColor = "red", 
                  opacity = 0.5,
                  popup = paste(
                    '<strong>SoS Site:</strong>', capitalize(as.character(SPsite$SiteName)), '<br>'  
                  )) %>%
      addCircles(data = spdat[is.na(spdat$SiteName),],
                       ~long, ~lat,#locations of species,
                       radius = 10,
                   #    weight = 5,
                        color = 'blue',
                        fill = TRUE,
                        fillColor = 'blue',
                        opacity = 1,
                        fillOpacity = 1,
                       group = 'Not site managed')%>%
      addCircles(data = spdat[!is.na(spdat$SiteName),],
                       radius = 10,
                  #     weight = 5,
                       ~long, ~lat,#locations of species
                        color = "#E69F00",
                        fill = TRUE,
                        fillColor = "#E69F00",
                        opacity = 1,
                        fillOpacity = 1,
                        group = 'SoS Site Managed') %>%
      addCircles(spdat$long, spdat$lat,#add the points again but make them clear, this allows for popup of info
                 fill = FALSE,
                 color = "#00ff0001",
                 opacity = 0,
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
                 position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Satellite", "OpenStreetMap"),
        overlayGroups = c('SoS Site Managed', 'Not site managed'),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  
  
  
  
  ################ 2. Population number #################
  popNumber<-c("< 5", ">5 and <10",">= 10 and < 20", ">= 20")
  updateSelectInput(session, "pop_number", "Number of populations:", 
                    choices = popNumber, selected="")
  
  
  
  
  
  
  
  
  ################ 3. Environmental variation ###########
  
  # Extract Environmental data and capad 
  EnvDat <- eventReactive(input$env, {
    # req(input$env)
    
    withProgress(message = 'Loading environmental data', value = 1, {
      
      req(input$SOSspecies, input$long_column, input$lat_column)
      
      spdat <- spData()
      spdat$lat <- spdat[, input$lat_column]
      spdat$long <- spdat[, input$long_column]
      dat <- EnvExtract(spdat$lat,spdat$long)
      
      #select site data
      coords <- dat[,c("long","lat")]
      coordinates(coords) <-c("long","lat")
      proj4string(coords)<-crs(sites)
      
      sp <- input$SOSspecies
      managmentSite <- sites[sites$SciName == sp,]
      dat <- cbind(dat, sp::over(coords,managmentSite,returnList = FALSE))
      return(dat)
    })
  })
  
  #plots of Environmental data - code for function is in "AppFunctions/plotEnviroHists.R")
  output$currentPlot <- renderPlot({
    Env <- EnvDat()
    CurClimPlot(Env,subset(Env,!is.na(Env$SiteName))) 
  })
  
  output$futurePlot <- renderPlot({
    Env<-EnvDat()
    futClimPlot(Env,subset(Env,!is.na(Env$SiteName)))
  })
  
  
  
  
  
  
  ################ 4.Site Selection ###########
  
  # list to store the selections for tracking
  data_of_click <- reactiveValues(clickedMarker = list())
  
  #make a vector of environmental variables to use
  variablesUSE <- reactive({
    tmax <- input$tmax
    rain <- input$rain
    rainVar <- input$rainVar
    elev <- input$elev
    soil <- input$soils
    allVars <- c('tmax','rain','rainVar','elev','soil')
    vars <- allVars[c(tmax, rain, rainVar, elev, soil)]
    
    print(vars)
    return(vars)
  })
  
  
  # prompt user to select data
  
  output$envVarPrompt <- renderText({
    req(length(variablesUSE()) < 2) 
    paste0('Please select two or more environmental variables')
  })
  
  #A variety of metrics exist to help choose the number of clusters to be extracted in a cluster analysis.
  #We use silhouette width, an internal validation metric which is an aggregated measure of how similar an
  #observation is to its own cluster compared its closest neighboring cluster. The metric can range from
  #-1 to 1, where higher values are better.
  output$clustersText <- renderText({
    
    req(sum(c(input$tmax, input$rain, input$rainVar, input$elev, input$soils) %in% TRUE) > 1) # more than one env var needs to be selected to run the analysis
    
    vars <- variablesUSE()
    Env <- EnvDat()
    clustersSuggested <- ClusterNumbers(Env, vars)
    paste0("The best three suggested numbers of clusters,",
           " based on a measure of how similar each observation is to",
           " its own cluster compared its closest neighbouring cluster, are ",
           clustersSuggested[1], ", ", clustersSuggested[2],
           ", and ",
           clustersSuggested[3],
           "." )
  })
  
  
 # base plot
  output$ClusterPlot <- renderLeaflet({
    Env <- EnvDat()

    #main map
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") %>%
      fitBounds(min(Env$long), min(Env$lat), max(Env$long), max(Env$lat)) %>%
      addLayersControl(
        baseGroups = c("Satellite", "OpenStreetMap"),
        options = layersControlOptions(collapsed = FALSE)
      )

  })
  
  #reactively run the cluster analysis based on the variables and number of clusters selected in the side bar
  clusDat <- reactive({
    
    req(sum(c(input$tmax, input$rain, input$rainVar, input$elev, input$soils) %in% TRUE) > 1)
   # req(input$tmax %in% TRUE)
    
    df<-EnvCluserData(EnvDat(),
                  variablesUSE(),
                  input$clusters)
    df$locationID<-paste0(1:nrow(df),"ID")
    df$secondLocationID <- paste(as.character(df$locationID), "_selectedLayer", sep="")
    return(df)
    
  })
  
  
  proxy <- leafletProxy("ClusterPlot")#this allows you to keep adding things to the map just by calling proxy
  # 

  observe({
    EnvClus <- clusDat()
    #make coordinates from the EnvClus, this will be used when selecting points for SOS managment sites
  
    #set colouring options for factors and numeric variables
    colorBy <- input$variable
    if (colorBy == "tmax" |colorBy =="rain" |colorBy =="elev") {
      # Color and palette if the values are  continuous.
      colorData <- EnvClus[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    } else {
      colorData <- EnvClus[[colorBy]]
      pal <- colorFactor("viridis", colorData)
    }


    #updating points on map based on selected variable and menu to draw polygons
    # proxy %>% addCircles(data = EnvClus,
    proxy %>% 
      
      addCircleMarkers(data = EnvClus[is.na(EnvClus$SiteName),],
                 # radius = 1500,
                 lat = ~lat,
                 lng = ~long,
                 fillColor = pal(colorData),
                 fillOpacity = 1,
                 color = pal(colorData),
                 weight = 2,
                 stroke = TRUE,
                 # highlightOptions = highlightOptions(color = "deeppink",
                 #                                     fillColor="deeppink",
                 #                                     opacity = 1.0,
                 #                                     weight = 3,
                 #                                     bringToFront = FALSE),
                 group = 'Not site managed') %>%
      
      addCircleMarkers(data = EnvClus[!is.na(EnvClus$SiteName),],
                         # radius = 1500,
                         lat = ~lat,
                         lng = ~long,
                         fillColor = pal(colorData),
                         fillOpacity = 1,
                         color = "#E69F00",
                         weight = 3,
                         stroke = TRUE,
                         # highlightOptions = highlightOptions(color = "deeppink",
                         #                                     fillColor="deeppink",
                         #                                     opacity = 1.0,
                         #                                     weight = 3,
                         #                                     bringToFront = FALSE),
                         group = 'SoS Site Managed') %>%
      
      addLayersControl(
        baseGroups = c("Satellite", "OpenStreetMap"),
        overlayGroups = c("SoS Site Managed", "Not site managed")
      ) %>%
      
      #bringToFront = FALSE makes it so that selected points stay on the top layer
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                          ,color = 'black'
                                                                          ,weight = 2)),
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'black'
                                                                              ,weight = 2)),
        circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                          ,color = 'black'
                                                                          ,weight = 2)),
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  })
  
  
  
  
  # ############subsetting obseration to get those inside the polygons ##################
  observeEvent(input$ClusterPlot_draw_new_feature,{
    
    EnvClus <- clusDat()
    ClusCoordinates <- SpatialPointsDataFrame(EnvClus[,c('long', 'lat')] , EnvClus)
    #tells r-shiny that if the user draws a shape return all teh uighe locations based on the location ID
    #Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$ClusterPlot_draw_new_feature
                                     , location_coordinates = ClusCoordinates
                                     , location_id_colname = "locationID")

    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }

    # look up clusDat by ids found
    selected <- subset(EnvClus, locationID %in% data_of_click$clickedMarker)
    proxy <- leafletProxy("ClusterPlot")
    proxy %>% addCircles(data = selected,#this is a new layer over the previous ones of the selected points
                         radius = 3500,#this makes a circle of a slightly larger size around the points
                         lat = selected$lat,
                         lng = selected$long,
                         fillColor = "transparent",#by not setting the full color you can see the variable (i.e cluster or soiltype) within the red circle
                         fillOpacity = 1,
                         color = "red",
                         weight = 2,
                         stroke = T,
                         layerId = as.character(selected$secondLocationID),
                         highlightOptions = highlightOptions(color = "hotpink",
                                                             opacity = 1.0,
                                                             weight = 2,
                                                             bringToFront = FALSE))

  })
  
  
  #function that removes selected locations since the user can delete all shapes     
  observeEvent(input$ClusterPlot_draw_deleted_features,{
    # loop through list of one or more deleted features/ polygons
    for(feature in input$ClusterPlot_draw_deleted_features$features){
      # get ids for locations within the bounding shape
      bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = ClusCoordinates
                                         , location_id_colname = "secondLocationID")
      
      
      # remove second layer representing selected locations
      proxy <- leafletProxy("ClusterPlot")
      proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
      
      first_layer_ids <- subset(clusDat, secondLocationID %in% bounded_layer_ids)$locationID
      
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                 %in% first_layer_ids]
      
    }
  })
  
  # #observer event to make table
  observeEvent(input$ClusterPlot_draw_deleted_features, {
    #find bounds of the shape
    found_in_bounds < - findLocations(shape = input$ClusterPlot_draw_deleted_features,
                                      location_coordinates = ClusCoordinates, 
                                      location_id_colname = "locationID")
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker < - append(data_of_click$clickedMarker, id, 0) } } })
  
  output$SelectedLocations <- reactive({
    EnvClus <- clusDat()
    SelectedLocations <- subset(EnvClus, locationID %in% data_of_click$clickedMarker)
    # return this output
    SelectedLocations
  })
  
  selectedLocations <- reactive({
    EnvClus <- clusDat()
    selectedLocations <- subset(EnvClus, locationID %in% data_of_click$clickedMarker)
    if(nrow(selectedLocations > 0)) {
      if(input$incSoS) {
        selectedLocations <- rbind(selectedLocations, EnvClus[!is.na(EnvClus$SiteName),]) %>% dplyr::distinct(locationID, .keep_all=TRUE)
      }
    }
    
    # return this output
    selectedLocations
  })
  
  
  #use the plotEnviroHist funciton to make plots
  output$SelectedCurrentPlot <- renderPlot({
    selectedDat <- selectedLocations()
    if (nrow(selectedDat)<1)#this removes the error message in the plotting function, if no locations selected, no plots
      return(NULL)
    selectedDat <- selectedLocations()
    CurClimPlot(clusDat(),selectedDat)
  })
  
  output$SelectedFuturePlot <- renderPlot({
    selectedDat <- selectedLocations()
    if (nrow(selectedDat)<1)#this removes the error message in the plotting function, if no locations selected, no plots
      return(NULL)
    selectedDat <- selectedLocations()
    futClimPlot(clusDat(),selectedDat)
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)