#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
       
       # Choices for drop-downs menu to colour points by selected variable in map
       vars <- c(
          "Cluster" = "cluster",
          "Avg. annual Tmax" = "tmax",
          "Avg. annual rainfall" = "rain",
          "Avg. annual rainfall variability" = "rainVar",
          "Elevation" = "elev",
          "Soil type" = "soil"
       ),
       
       #base leaflet map
       leafletOutput('ClusterPlot'),
       absolutePanel(top = 45, right = 20, width = 150, draggable = TRUE,
                     selectInput("bmap", "Select base map", 
                                 choices =  c("Base map",
                                              "Satellite imagery"), 
                                 selected = "Base map"),
                     selectInput("variable", "Display Variable", vars)
                     
                     )
     )
   )
   
)








# Define server logic required to draw a histogram
server <- function(input, output) {
  ################## in the real app this aleady  exists
  #get the data set up
  source("AppFunctions/extractEnviroData.R", local = T)
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
  
  # 
  # allSite<-dat
  # sosSite<-subset(dat,!is.na(dat$SiteName))
  
  #perform cluster analysis
  variablesUSE <- c("soil", "elev", "rain", "tmax", "rainVar") #this needs to be reacitve
  clusters<-4 #this needs to be reactive
  clusDat<-  EnvCluserData(EnvDat,variablesUSE,clusters) #make reactive
  coordinates <- SpatialPointsDataFrame( clusDat[,c('long', 'lat')] , clusDat)#reactive?
  
  
  # generate set of unique location IDs for second layer of selected locations.
  #the unique id’s are needed to color the locations we select. 
  #The reason for this is that the new color is not a color change, 
  #but a newly added map layer. Each location in our dataset could at any point in 
  #time be represented by either one or two layers, so each one needs two unique id’s.
  clusDat$locationID <- paste0(as.character(1:nrow(clusDat)), "_ID")
  clusDat$secondLocationID <- paste0(clusDat$LocationID, "_selectedLayer")
  
  
  
  #empty leaflet plot
  output$ClusterPlot <- renderLeaflet({
      # draw the histogram with the specified number of bins
      #hist(clusDat$cluster, breaks =5, col = 'darkgray', border = 'white'
       
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
  
  # list to store the selections for tracking
  data_of_click <- reactiveValues(clickedMarker = list())
  
  
  #updating points on map based on selected variable and menu to draw polygons
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

    leafletProxy("ClusterPlot", data = clusDat) %>% #adds points to the graph
      clearShapes() %>%
      addCircles(~long, ~lat, 
                 radius=5000,
                 fillOpacity=0.4, 
                 fillColor=pal(colorData),
                 weight = 2,
                 stroke = T,
                 layerId = as.character(clusDat$locationID),
                 highlightOptions = highlightOptions(color = "mediumseagreen",
                                                     opacity = 1.0,
                                                     weight = 2,
                                                     bringToFront = TRUE)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, 
                layerId="colorLegend")%>% #legend for varibales
      
      addDrawToolbar( #toolbar to drawshapes
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
  
  
  
  ############## list to store the selections for tracking ###############
  data_of_click <- reactiveValues(clickedMarker = list())
  
  
  ############################################### section three #################################################
  observeEvent(input$mymap_draw_new_feature,{
    #Only add new layers for bounded locations
    found_in_bounds <- findLocations(shape = input$mymap_draw_new_feature
                                     , location_coordinates = coordinates
                                     , location_id_colname = "locationID")
    
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
      }
    }
    
    # look up airports by ids found
    selected <- subset(airports, locationID %in% data_of_click$clickedMarker)
    
    proxy <- leafletProxy("mymap")
    proxy %>% addCircles(data = selected,
                         radius = 5000,
                         lat = selected$Latitude,
                         lng = selected$Longitude,
                         fillColor = "wheat",
                         fillOpacity = 1,
                         color = "mediumseagreen",
                         weight = 3,
                         stroke = T,
                         layerId = as.character(selected$secondLocationID),
                         highlightOptions = highlightOptions(color = "hotpink",
                                                             opacity = 1.0,
                                                             weight = 2,
                                                             bringToFront = TRUE))
    
  })
#   
#   ############################################### section four ##################################################
#   observeEvent(input$mymap_draw_deleted_features,{
#     # loop through list of one or more deleted features/ polygons
#     for(feature in input$mymap_draw_deleted_features$features){
#       
#       # get ids for locations within the bounding shape
#       bounded_layer_ids <- findLocations(shape = feature
#                                          , location_coordinates = coordinates
#                                          , location_id_colname = "secondLocationID")
#       
#       
#       # remove second layer representing selected locations
#       proxy <- leafletProxy("mymap")
#       proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
#       
#       first_layer_ids <- subset(airports, secondLocationID %in% bounded_layer_ids)$locationID
#       
#       data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
#                                                                  %in% first_layer_ids]
#     }
#   })
# },
# 
# options = list(height = 400)
# )
#   
  
}

# Run the application 
shinyApp(ui = ui, server = server)

