
library(shiny)
#get test data
library(rgdal)
require(cluster) 
library(leaflet)
library(leaflet.extras)
library(sp)

source("AppFunctions/extractEnviroData.R", local = T)
source("AppFunctions/plotEnviroHists.R", local = T)
source("AppFunctions/ClusterAnalysis.R", local = T)
source("AppFunctions/findLocations.R", local = T)



# source: https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm
#clusDat <- read.csv('/Users/daisy/repos/OEHDecisionFramework/AppEnvData/SpeciesObservations/obsSmall2.csv')


sp<-"Acacia acanthoclada"
spdat<-read.csv("AppEnvData/SpeciesObservations/SOSflora.csv",header=TRUE)
spdat<-subset(spdat,Scientific==sp)
sites<-readOGR("AppEnvData/ManagmentSites/OEHManagmentSites.shp")

spdat$lat <- spdat[, "Latitude_G"]
spdat$long <- spdat[, "Longitude_"]
dat<-EnvExtract(spdat$lat,spdat$long)

#select site data
coords <- dat[,c("long","lat")]
coordinates(coords) <-c("long","lat")
proj4string(coords)<-crs(sites)

managmentSite <- sites[sites$SciName == sp,]
EnvDat<-cbind(dat,over(coords,managmentSite,returnList = FALSE))
################################ 
#perform cluster analysis
variablesUSE <- c("soil", "elev", "rain", "tmax", "rainVar") #this needs to be reacitve
clusters<-4 #this needs to be reactive
clusDat<-  EnvCluserData(EnvDat,variablesUSE,clusters) #make reactive



shinyApp(
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    
    # Application title
    titlePanel("Cluster analysis"),
    
    # Sidebar to select inputs
    sidebarLayout(
      sidebarPanel(
        selectInput("tmax", "Avg. annual Tmax", c("yes","no")),
        selectInput("rain", "Avg. annual rainfall", c("yes","no")),
        selectInput("rainVar", "Avg. annual rainfall variability", c("yes","no")), 
        selectInput("elev", "Elevation", c("yes","no")),
        selectInput("soils", "Soil type", c("yes","no")),
        numericInput('clusters', 'Cluster count',2,
                     min = 2, max = 9)
      ),
      
      mainPanel( 
        # Choices for the drop-downs menu, colour the points by selected variable in map, "cluster", "tmax", etc. are the names in the data after the cluster analysis is run
        vars <- c(
          "Cluster" = "cluster",
          "Avg. annual Tmax" = "tmax",
          "Avg. annual rainfall" = "rain",
          "Avg. annual rainfall variability" = "rainVar",
          "Elevation" = "elev",
          "Soil type" = "soil"
        ),
        
        #sets location for base leaflet map and make dropdown menu to select the backgroudn map
        leafletOutput('ClusterPlot'),
        absolutePanel(top = 45, right = 20, width = 150, draggable = TRUE,
                      selectInput("bmap", "Select base map", 
                                  choices =  c("Base map",
                                               "Satellite imagery"), 
                                  selected = "Base map"),
                      selectInput("variable", "Display Variable", vars)),
        #dummy plot
        plotOutput(outputId = "SelectedcurrentPlot",height = "600px")
        
        
      )
    )
    
  ),
  
  server <- function(input, output) {
    
    
    
    # generate two set of unique location IDs
    #the unique id’s are needed to color the locations we select. 
    clusDat$locationID<-paste0(1:nrow(clusDat),"ID")
    clusDat$secondLocationID <- paste(as.character(clusDat$locationID), "_selectedLayer", sep="")
    #make coordinates from the clusDat, this will be used when selecting points for SOS managment sites
    ClusCoordinates <- SpatialPointsDataFrame(clusDat[,c('long', 'lat')] , clusDat)
    
    
    ################################################# section one #################################################
    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())
    
    ################################################# section two #################################################
    # base map
    output$ClusterPlot <- renderLeaflet({
      #get base map name
      if(input$bmap== "Base map"){
        mapType<-"OpenStreetMap.Mapnik"
      }
      if(input$bmap== "Satellite imagery"){
        mapType<-"Esri.WorldImagery"
      }
      #main map
      leaflet() %>%
        addProviderTiles(mapType) %>%
        fitBounds(min(clusDat$long), min(clusDat$lat), max(clusDat$long), max(clusDat$lat))
    })
      
    proxy <- leafletProxy("ClusterPlot")#this allows you to keep adding things to the map just by calling proxy
    
    #set colouring options for factors and numeric variables
    observe({
      colorBy <- input$variable
      if (colorBy == "tmax" |colorBy =="rain" |colorBy =="elev") {
        # Color and palette if the values are  continuous.
        colorData <- clusDat[[colorBy]]
        pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      } else {
        colorData <- clusDat[[colorBy]]
        pal <- colorFactor("viridis", colorData)
      }

    #updating points on map based on selected variable and menu to draw polygons
    proxy %>% addCircles(data = clusDat,
                   radius = 3000,
                   lat = clusDat$lat,
                   lng = clusDat$long,
                   fillColor = pal(colorData),
                   fillOpacity = 1,
                   color = pal(colorData),
                   weight = 2,
                   stroke = T,
                   layerId = as.character(clusDat$locationID),
                   highlightOptions = highlightOptions(color = "deeppink",
                                                       fillColor="deeppink",
                                                       opacity = 1.0,
                                                       weight = 3,
                                                       bringToFront = FALSE)) %>%#bringToFront = FALSE makes it so that selected points stay on the top layer
        addDrawToolbar(
          targetGroup='Selected',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                            ,color = 'black'
                                                                            ,weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                ,color = 'black'
                                                                                ,weight = 3)),
          circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                            ,color = 'black'
                                                                            ,weight = 3)),
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
    })
   
    
    ############subsetting obseration to get those inside the polygons ##################
    observeEvent(input$ClusterPlot_draw_new_feature,{
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
      selected <- subset(clusDat, locationID %in% data_of_click$clickedMarker)
      
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
      found_in_bounds < - findLocations(shape = input$ClusterPlot_draw_deleted_features, location_coordinates = ClusCoordinates, location_id_colname = "locationID")
      for(id in found_in_bounds){
        if(id %in% data_of_click$clickedMarker){
          # don't add id
        } else {
          # add id
          data_of_click$clickedMarker < - append(data_of_click$clickedMarker, id, 0) } } })

    selectedLocations <- reactive({
      selectedLocations <- subset(clusDat, locationID %in% data_of_click$clickedMarker)
      # return this output
      selectedLocations
    })

    #output$TableSelectedData <- renderDataTable(iris)
    output$plot1 <- renderPlot({
      selectedDat <- selectedLocations()
      hist(selectedDat$tmax)
    })
  },
  options = list(height = 400)
)




shinyApp(ui = ui, server = server)