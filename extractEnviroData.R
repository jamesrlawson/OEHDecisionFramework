#function to extract environmental data

dat<-read.csv("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/SpeciesObservations/BionetRecords/obsSmall.csv",header=TRUE)

lat<-dat$Latitude_G
long<-dat$Longitude_


require(raster)
require(sp)
require(rgdal)

P4S <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")#projection

EnvExtract<-function(lat,long){
  #prep point data
  P4S <- CRS("+proj=longlat +datum=WGS84")#projection
  coords <- data.frame(long, lat)
  coordinates(coords) <-c("long","lat")
  proj4string(coords)<-P4S
  
  #extract point data for shapefiles
  CAPAD<-over(coords,shapefile("AppEnvData/CAPADnsw/CAPAD2_NSW.shp",p4s = paste(P4S)))$Name
  soil<-over(coords,shapefile("AppEnvData/soil/soilAtlas2M.shp",p4s = paste(P4S)))$Majr_Sl
  SOS<-over(coords,shapefile("AppEnvData/SOS/OEHManagmentSites.shp",p4s = paste(P4S)),returnList = TRUE)

  #extract point data for climate variables
  elev<-extract(raster("AppEnvData/elev.asc"),coords)
  rain<-extract(raster("AppEnvData/rain.asc"),coords)
  tmax<-extract(raster("AppEnvData/tmax.asc"),coords)
  rainVar<-extract(raster("AppEnvData/rainVar.asc"),coords)
  NarClimffdigt50<-extract(raster("AppEnvData/NarClimffdigt50.asc"),coords)
  NarClimfRain<-extract(raster("AppEnvData/NarClimRain.asc"),coords)
  NarClimfTmax<-extract(raster("AppEnvData/NarClimRain.asc"),coords)
  
  
  
  
  
  
  
}

