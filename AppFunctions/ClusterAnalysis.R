#For more information on mixed data cluster analysis - https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
#Gower distance - For each variable type, a particular distance metric that works well for that type is used and scaled to fall between 0 and 1.

#A variety of metrics exist to help choose the number of clusters to be extracted in a cluster analysis.
#We use silhouette width, an internal validation metric which is an aggregated measure of how similar an
#observation is to its own cluster compared its closest neighboring cluster. The metric can range from
#-1 to 1, where higher values are better.

#get test data

# sp<-"Acacia baueri subsp. aspera"
# spdat<-read.csv("AppEnvData/SpeciesObservations/obsSmall2.csv")
# spdat<-subset(spdat,Scientific==sp)
# 
# spdata <- spdat[,c('Latitude_G', 'Longitude_')] 
# 
# dat <- EnvExtract(spdata)
# 
# #select site data
# coords <- dat[,c("long","lat")]
# coordinates(coords) <-c("long","lat")
# proj4string(coords)<-crs(sites)
# 
# 
# sp.AOO_poly <- spdat %>% getAOOraster(.,1) %>% rasterToPolygons(.)
# 
# # sp <- input$SOSspecies
# sites<-readOGR("AppEnvData/ManagmentSites/OEHManagmentSites.shp")
# 
# projAEA <- crs("+init=epsg:3577 +proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m")
# managmentSite <- sites[sites$SciName == sp,] %>% spTransform(.,projAEA)
# 
# dat <- cbind(dat, sp::over(sp.AOO_poly,managmentSite,returnList = FALSE))
# 
# 
# require(cluster) # for gower similarity and pam
# #require(Rtsne) # for t-SNE plot
# #require(ggplot2) # for visualization
# variablesUSE <- c("soil", "elev", "rain", "tmax", "rainVar")
# #clusters<-4


# return a df of occurrence lat longs, env data and cluster ID's
EnvCluserData <- function(Env, variablesUSE, clusters) {
  
  print('running EnvClusterData')
  
  # get data that is needed
  dat <- Env
  

  #clean data for use
  datCluster <- dat[, variablesUSE]
  if(!is.atomic(datCluster)){ # only do this if 
    datCluster$soil <- if (is.element("soil", variablesUSE))factor(datCluster$soil)
  #  datCluster$rainVar <- if (is.element("rainVar", variablesUSE))factor(datCluster$rainVar)
 }
  # #define (dis)similarity between observations
  gower_dist <- daisy(datCluster,
                      metric = "gower")
  
  #cluster analysis
  pam_fit <- pam(gower_dist, diss = TRUE, k = clusters)
  dat$cluster <- pam_fit$clustering
  
  return(dat)
  
}



ClusterNumbers <- function(Env, variablesUSE) {
  
 # browser()
  
  # get data that is needed
  dat <- Env
  #clean data for use
  datCluster <- dat[, variablesUSE]
  datCluster$soil <- if (is.element("soil", variablesUSE))factor(datCluster$soil)
 # datCluster$rainVar <- if (is.element("rainVar", variablesUSE))factor(datCluster$rainVar)
  #clean data for use
  
  #define (dis)similarity between observations
  gower_dist <- daisy(datCluster,
                      metric = "gower")
  
  #determine suggested number of clusters
  sil_width <- c()
 #  for (i in 1:9) {
  for (i in 1:6) {
    pam_fit <- pam(gower_dist,
                   diss = TRUE,
                   k = i+1) #clusters from 2:7
    sil_width[i] <- pam_fit$silinfo$avg.width
  }
  
  sil_dat <- data.frame(cbind(sil_width, 2:7))
  sil_dat<-sil_dat[order(sil_width,decreasing = TRUE),] #order them so best performers are first in dataframe

  return(sil_dat$V2[1:3])
  
}
  


