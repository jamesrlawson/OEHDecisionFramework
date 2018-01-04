#clip data to NSW borders

P4S <- CRS("+proj=longlat +datum=WGS84")#projection

#use SOS data for extent object
SOS<-shapefile("AppEnvData/SOS/SearchResults.shp")
SOS<-spTransform(SOS, paste(P4S))


require("raster")



#CAPAD
CPD<-shapefile("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/Data/CAPAD2/CAPAD_2016_tech_spec_terrestrial/CAPAD_2016_terrestrial.shp")
CPD_NSW<-CPD[CPD$STATE=="NSW" |CPD$STATE=="JBT",]

#current climate
rain<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/Data/EnvironmentalData/climate/anPrec1961-1990.txt",
            crs=paste(P4S))
rain<-crop(rain,extent(SOS))

tmax<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/Data/EnvironmentalData/climate/anPrec1961-1990.txt",
            crs=paste(P4S))
rainVar<-raster("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/Data/EnvironmentalData/climate/varPrec1900-2003.txt",
            crs=paste(P4S))

#other variables
elev<-raster("/Users/daisy/GoogleDrive/PhD/Data/Spatial/Elevation/eMAST_ANUClimate_fx_elev_v1m0.nc")




GPP<-raster::extract(gpp,cbind(df1$Longitude,df1$Latitude))
LAI<-raster::extract(lai,cbind(df1$Longitude,df1$Latitude))
VPD<-raster::extract(vpd,cbind(df1$Longitude,df1$Latitude))
CAL<-raster::extract(cal,cbind(df1$Longitude,df1$Latitude))
