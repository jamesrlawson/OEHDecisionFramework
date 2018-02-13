require(sp)
require(raster)
require(rgdal)
#require(maptools)
#require(rgeos)



#clip data to smaller area
e<-extent(135.581,156.275,-39.525,-23.975)

P4S <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")#projection

#use SOS data for extent object
SOS<-shapefile("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/OEHManagmentSites/data/SiteManagedSpecies03112017/SearchResults.shp")
SOS<-spTransform(SOS, paste(P4S))
writeOGR(SOS, ".", "AppEnvData/SOS/OEHManagmentSites", driver="ESRI Shapefile")

###############
#CAPAD
##############

CPD<-readOGR("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/CAPAD2/CAPAD_2016_tech_spec_terrestrial/CAPAD_2016_terrestrial.shp")
CPD_NSW<-CPD[CPD$STATE=="NSW" |CPD$STATE=="JBT",]
CPD_NSW<-spTransform(CPD_NSW, paste(P4S))
writeOGR(CPD_NSW, ".", "AppEnvData/CAPADnsw/CAPAD2_NSW", driver="ESRI Shapefile")


################
#current climate
###############
rain<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/EnvironmentalData/climate/anPrec1961-1990.txt",
            crs=paste(P4S))

rain<-crop(rain,e)
writeRaster(rain,"AppEnvData/rain.asc",overwrite=TRUE)

tmax<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/EnvironmentalData/climate/mTmax1961-1990.txt",
            crs=paste(P4S))

tmax<-crop(tmax,e)
writeRaster(tmax,"AppEnvData/tmax.asc",overwrite=TRUE)


rainVar<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/EnvironmentalData/climate/varPrec1900-2003.txt",
            crs=paste(P4S))
rainVar<-crop(rainVar,e)
writeRaster(rainVar,"AppEnvData/rainVar.asc",overwrite=TRUE)


##############
#elevation
#############
elev<-raster("/Users/daisy/GoogleDrive/PhD/Data/Spatial/Elevation/eMAST_ANUClimate_fx_elev_v1m0.nc")
elev<-crop(elev,e)
writeRaster(elev,"AppEnvData/elev.asc",overwrite=TRUE)


##############
#soil data
#############
soil<-shapefile("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/EnvironmentalData/Soils/soilAtlas2M/soilAtlas2M.shp")
Smajor<-read.csv("~/GoogleDrive/Projects/OEHProtectedSpecies/RawData/EnvironmentalData/Soils/SoilAtlas2M_ASC/asclut.txt", header=FALSE,col.names = c("MAP_UNIT","V2","Major_Soil_abr","Major_Soil"))#major soil classes
soil@data <- merge(soil@data,Smajor,by="MAP_UNIT", all.x=T, sort=F)
soil<-crop(soil,e)
soil<-spTransform(soil, paste(P4S))
writeOGR(soil, ".", "AppEnvData/soil/soilAtlas2M", driver="ESRI Shapefile")


#############
#future climte (Change)
#############
FRain<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/MultiModelMean_NARCLiM Domain_All Epochs_All Variables_Annual and Seasonal/6c3cb0199634_d02_multimodel_mean_chg_2060_2079_praccfl_percent_ann.asc")# percent change
writeRaster(FRain,"AppEnvData/NarClimRain.asc",overwrite=TRUE)

  
FTmax<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/MultiModelMean_NARCLiM Domain_All Epochs_All Variables_Annual and Seasonal/6c3cb0199634_d02_multimodel_mean_chg_2060_2079_tasmaxmean_K_ann.asc")
writeRaster(FTmax,"AppEnvData/NarClimTmax.asc",overwrite=TRUE)
  
FFire<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/RawData/MultiModelMean_NARCLiM Domain_All Epochs_All Variables_Annual and Seasonal/6c3cb0199634_d02_multimodel_mean_chg_2060_2079_ffdigt50_ndays_ann.asc")#change increase in ndays

writeRaster(FTmax,"AppEnvData/NarClimffdigt50.asc",overwrite=TRUE)


#################
#map of NSW
#################

#Map of NSW
AUS<-readOGR("/Users/daisy/GoogleDrive/PhD/Data/Spatial/AustraliaPolygon/STE11aAust.shp")
NSW<-AUS[AUS$STATE_NAME=="New South Wales",]
NSW <- gSimplify(NSW, tol=0.01, topologyPreserve=TRUE)



  
