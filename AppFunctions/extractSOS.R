#extracts the details of the  Save our Species polygon files for the desired locations (given as lat and long)


require(raster)
require(sp)
require(rgdal)


SOSextract<-function(lat,long){
  #prep point data
  P4S <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  coords <- data.frame(long, lat)
  coordinates(coords) <-c("long","lat")
  proj4string(coords)<-P4S
  
  SOS<-over(coords,shapefile("AppEnvData/SOS/OEHManagmentSites.shp",p4s = paste(P4S)),returnList = TRUE)
  return(SOS)
}

