#Gower distance - For each variable type, a particular distance metric that works well for that type is used and scaled to fall between 0 and 1.

#A variety of metrics exist to help choose the number of clusters to be extracted in a cluster analysis.
#We use silhouette width, an internal validation metric which is an aggregated measure of how similar an
#observation is to its own cluster compared its closest neighboring cluster. The metric can range from
#-1 to 1, where higher values are better.

#get test data
library(rgdal)

source("AppFunctions/extractEnviroData.R", local = T)

sp <- "Acacia acanthoclada"
spdat <-
  read.csv("AppEnvData/SpeciesObservations/SOSflora.csv", header = TRUE)
spdat <- subset(spdat, Scientific == sp)
sites <- readOGR("AppEnvData/ManagmentSites/OEHManagmentSites.shp")

spdat$lat <- spdat[, "Latitude_G"]
spdat$long <- spdat[, "Longitude_"]
dat <- EnvExtract(spdat$lat, spdat$long)


require(cluster) # for gower similarity and pam
#require(Rtsne) # for t-SNE plot
#require(ggplot2) # for visualization
variablesUSE <- c("soil", "elev", "rain", "tmax", "NewRainVar")

EnvCluser <- function(Env, variablesUSE) {
  # get data that is needed
  dat <- Env
  #clean data for use
  datCluster <- dat[, variablesUSE]
  datCluster$soil <- factor(datCluster$soil)
  datCluster$NewRainVar <- factor(datCluster$NewRainVar)
  #clean data for use
  
  #define (dis)similarity between observations
  gower_dist <- daisy(datCluster,
                      metric = "gower")
  
  #determine suggested number of clusters
  sil_width <- c(NA)
  for (i in 2:10) {
    pam_fit <- pam(gower_dist,
                   diss = TRUE,
                   k = i)
    sil_width[i] <- pam_fit$silinfo$avg.width
  }
  sil_dat <- na.omit(data.frame(cbind(sil_width, 1:10)))
  C1 <- subset(sil_dat, sil_width == max(sil_dat$sil_width))$V2
  
  #cluster analysis
  pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
  dat$cluster <- pam_fit$clustering
  
  return(dat)
  
}
