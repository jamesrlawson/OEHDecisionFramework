# #function to extract environmental data and CAPAD protected areas
# sp<-"Astrotricha crassifolia"
# spdat<-read.csv("/Users/daisy/repos/OEHDecisionFramework/AppEnvData/SpeciesObservations/SOSflora.csv",header=TRUE)
# spdat<-subset(spdat,Scientific==sp)
# 
# lat<-spdat$Latitude_G
# long<-spdat$Longitude_


require(raster)
require(sp)
require(rgdal)


EnvExtract<-function(lat,long){
  #prep point data
  P4S <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  coords <- data.frame(long, lat)
  coordinates(coords) <-c("long","lat")
  proj4string(coords)<-P4S
  dat<-data.frame(cbind(long,lat))
  #extract point data for shapefiles
  dat$CAPAD<-over(coords,shapefile("AppEnvData/CAPADnsw/CAPAD2_NSW.shp",p4s = paste(P4S)))$Name
  dat$soil<-over(coords,shapefile("AppEnvData/soil/soilAtlas2M.shp",p4s = paste(P4S)))$Majr_Sl
  #extract point data for climate variables
  dat$elev<-extract(raster("AppEnvData/elev.asc"),coords)
  dat$rain<-extract(raster("AppEnvData/rain.asc"),coords)
  dat$tmax<-extract(raster("AppEnvData/tmax.asc"),coords)
  dat$rainVar<-extract(raster("AppEnvData/rainVar.asc"),coords)
  dat$NarClimffdigt50<-extract(raster("AppEnvData/NarClimffdigt50.asc"),coords)
  dat$NarClimfRain<-extract(raster("AppEnvData/NarClimRain.asc"),coords)
  dat$NarClimfTmax<-extract(raster("AppEnvData/NarClimTmax.asc"),coords)
  return(dat)
}


