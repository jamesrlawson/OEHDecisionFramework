#For more information on mixed data cluster analysis - https://www.r-bloggers.com/clustering-mixed-data-types-in-r/
#Gower distance - For each variable type, a particular distance metric that works well for that type is used and scaled to fall between 0 and 1.

#A variety of metrics exist to help choose the number of clusters to be extracted in a cluster analysis.
#We use silhouette width, an internal validation metric which is an aggregated measure of how similar an
#observation is to its own cluster compared its closest neighboring cluster. The metric can range from
#-1 to 1, where higher values are better.

# #get test data
# library(rgdal)
# 
# source("AppFunctions/extractEnviroData.R", local = T)
# 
# sp <- "Acacia acanthoclada"
# spdat <-
#   read.csv("AppEnvData/SpeciesObservations/SOSflora.csv", header = TRUE)
# spdat <- subset(spdat, Scientific == sp)
# sites <- readOGR("AppEnvData/ManagmentSites/OEHManagmentSites.shp")
# 
# spdat$lat <- spdat[, "Latitude_G"]
# spdat$long <- spdat[, "Longitude_"]
# dat <- EnvExtract(spdat$lat, spdat$long)
# 
# 
require(cluster) # for gower similarity and pam
require(Rtsne) # for t-SNE plot
require(ggplot2) # for visualization
# variablesUSE <- c("soil", "elev", "rain", "tmax", "rainVar")
# #clusters<-4

EnvCluserData <- function(Env, variablesUSE,clusters) {
  # get data that is needed
  dat <- Env
  #clean data for use
  datCluster <- dat[, variablesUSE]
  datCluster$soil <- if (is.element("soil", variablesUSE))factor(datCluster$soil)
  datCluster$rainVar <- if (is.element("rainVar", variablesUSE))factor(datCluster$rainVar)

  # #define (dis)similarity between observations
  gower_dist <- daisy(datCluster,
                      metric = "gower")
  
  #cluster analysis
  pam_fit <- pam(gower_dist, diss = TRUE, k = clusters)
  dat$cluster <- pam_fit$clustering
  
  return(dat)
  
}



ClusterNumbers <- function(Env, variablesUSE) {
  # get data that is needed
  dat <- Env
  #clean data for use
  datCluster <- dat[, variablesUSE]
  datCluster$soil <- if (is.element("soil", variablesUSE))factor(datCluster$soil)
  datCluster$rainVar <- if (is.element("rainVar", variablesUSE))factor(datCluster$rainVar)
  #clean data for use
  
  #define (dis)similarity between observations
  gower_dist <- daisy(datCluster,
                      metric = "gower")
  
  #determine suggested number of clusters
  sil_width <- c()
  for (i in 1:9) {
    pam_fit <- pam(gower_dist,
                   diss = TRUE,
                   k = i+1) #clusters from 2:10
    sil_width[i] <- pam_fit$silinfo$avg.width
  }
  
  sil_dat <- data.frame(cbind(sil_width, 2:10))
  sil_dat<-sil_dat[order(sil_width,decreasing = TRUE),] #order them so best performers are first in dataframe

  return(sil_dat$V2[1:3])
  
}
  


