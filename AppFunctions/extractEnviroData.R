# 
# # #function to extract environmental data and CAPAD protected areas
# sp<-"Astrotricha crassifolia"
# spdat<-read.csv("/Users/daisy/repos/OEHDecisionFramework/AppEnvData/SpeciesObservations/SOSflora.csv",header=TRUE)
# spdat<-subset(spdat,Scientific==sp)
# 
# lat<-spdat$Latitude_G
# long<-spdat$Longitude_


require(raster)
require(sp)
require(rgdal)
require(igraph)
require(magrittr)

######## get 2km grid AOO, with optimised grid initiation point #########

getAOOraster <- function(spdat, Cell_size_AOO=1) {
  
  # reproject coordinates to AEA
  
  projAEA <- crs("+init=epsg:3577 +proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m")
  
  spdat %<>%
    dplyr::select(Longitude_, Latitude_G) %>%
    SpatialPoints(CRS("+init=epsg:4326")) %>%
    spTransform(projAEA) %>%
    `@`(coords) %>%
    data.frame %>%
    setNames(c("longitudeAea", "latitudeAea"))
  
  # calculate AOO, with 1000m grid optimisation
  
  Corners <- rbind(c(min(spdat[,1]), max(spdat[,1])), c(min(spdat[,2]), max(spdat[,2])))
  
  Occupied_cells <- c()
  decal <- c(0,1,2,3)
  out.list <- list()
  for (h in decal) {
    ext = extent(floor(Corners[1,1])-h*(Cell_size_AOO*1000/4)-2*Cell_size_AOO*1000, floor(Corners[1,2])+h*(Cell_size_AOO*1000/4)+2*Cell_size_AOO*1000, 
                 floor(Corners[2,1])-h*(Cell_size_AOO*1000/4)-2*Cell_size_AOO*1000, floor(Corners[2,2])+h*(Cell_size_AOO*1000/4)+2*Cell_size_AOO*1000)
    r = raster(ext, resolution=Cell_size_AOO*1000,crs=projAEA)
    r2_AOO <- rasterize(spdat, r)
    OCC <- length(which(!is.na(values(r2_AOO))))
    Occupied_cells <- c(Occupied_cells, OCC)
    out.list[[h+1]] <- r2_AOO
    ### If only one occupied cell, stop the production of raster
    if(OCC==1) break
  }
  h <- decal[which.min(Occupied_cells)]
  Occupied_cells <- min(Occupied_cells)
  
  return(out.list[[h+1]])
  
}

######## reproject raster data to species extent ########

env.reproj <- function(ras, AOOraster) {
  
  projAEA <- crs("+init=epsg:3577 +proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m")
  
  # and create a base raster of the same extent as the input AOOraster (use getAOOraster())
  r <- raster::raster(ext = extent(AOOraster), crs = projAEA)
  
  # reproject to r
  ras.reproj <- raster::projectRaster(ras, r)
  
  return(ras.reproj)
}


EnvExtract<-function(spdat){
  
  projAEA <- crs("+init=epsg:3577 +proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m")
  
  # get 1km gridcell AOO
  sp.AOO <- getAOOraster(spdat, 1) %>%
            clump(directions=8) # clump by cell adjacency (including diagonals) and set raster values as the clump ID
  
  ## get cell count for each clump
  # table(as.matrix(bla.ras.clumped))
  
  sp.AOO_poly<- rasterToPolygons(sp.AOO) # convert to polygons

  # dat <- sp.AOO_poly %>% as.data.frame(.)

  #prep point data
  P4S <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  # coords <- data.frame(long, lat)
  # coordinates(coords) <-c("long","lat")
  # proj4string(coords)<-P4S
  # dat<-data.frame(cbind(long,lat))
  
  #extract means of pixel data for shapefiles
  print('extracting CAPAD data')
  CAPAD <- shapefile("AppEnvData/CAPADnsw/CAPAD2_NSW.shp") %>% 
           spTransform(., projAEA) %>%
           over(sp.AOO_poly, .)
  sp.AOO_poly$CAPAD <- paste(CAPAD$NAME, CAPAD$TYPE) %>% gsub('NA NA', NA, .)
  print('extracting soil data')
  soil <- shapefile("AppEnvData/soil/soilAtlas2M.shp") %>%
          spTransform(., projAEA) %>%
          over(sp.AOO_poly, .)
  sp.AOO_poly$soil <- soil$Majr_Sl
    
  # #extract point data for shapefiles
  # dat$CAPAD<-over(coords,shapefile("AppEnvData/CAPADnsw/CAPAD2_NSW.shp",p4s = paste(P4S)))$Name
  # dat$soil<-over(coords,shapefile("AppEnvData/soil/soilAtlas2M.shp",p4s = paste(P4S)))$Majr_Sl

  #extract means of pixel data from rasters
  print('extracting raster data')
  
  sp.AOO_poly$elev <- env.reproj(raster("AppEnvData/elev.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
    extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  sp.AOO_poly$rain <- env.reproj(raster("AppEnvData/rain.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
    extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  sp.AOO_poly$tmax <- env.reproj(raster("AppEnvData/tmax.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
    extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  sp.AOO_poly$rainVar <- env.reproj(raster("AppEnvData/rainVar.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
    extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  sp.AOO_poly$NarClimffdigt50 <- env.reproj(raster("AppEnvData/NarClimffdigt50.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
    extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  sp.AOO_poly$NarClimfRain <- env.reproj(raster("AppEnvData/NarClimRain.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
    extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  sp.AOO_poly$NarClimfTmax <- env.reproj(raster("AppEnvData/NarClimTmax.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
    extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  # 
  # 
  # dat$elev <- env.reproj(raster("AppEnvData/elev.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
  #             extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  # dat$rain <- env.reproj(raster("AppEnvData/rain.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
  #   extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  # dat$tmax <- env.reproj(raster("AppEnvData/tmax.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
  #   extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  # dat$rainVar <- env.reproj(raster("AppEnvData/rainVar.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
  #   extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  # dat$NarClimffdigt50 <- env.reproj(raster("AppEnvData/NarClimffdigt50.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
  #   extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  # dat$NarClimfRain <- env.reproj(raster("AppEnvData/NarClimRain.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
  #   extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  # dat$NarClimfTmax <- env.reproj(raster("AppEnvData/NarClimTmax.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
  #   extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
  
  # get actual predictions for tmax and rainfall
  
  sp.AOO_poly$tmax_future <- sp.AOO_poly$tmax + sp.AOO_poly$NarClimfTmax
  sp.AOO_poly$rain_future <- sp.AOO_poly$rain * (1 + (sp.AOO_poly$NarClimfRain/100)) # NarClimfRain is in % change, this transforms it to % of current climate value
  
  # #extract point data for climate variables
  # dat$elev<-extract(raster("AppEnvData/elev.asc"),coords)
  # dat$rain<-extract(raster("AppEnvData/rain.asc"),coords)
  # dat$tmax<-extract(raster("AppEnvData/tmax.asc"),coords)
  # dat$rainVar<-extract(raster("AppEnvData/rainVar.asc"),coords)
  # dat$NarClimffdigt50<-extract(raster("AppEnvData/NarClimffdigt50.asc"),coords)
  # dat$NarClimfRain<-extract(raster("AppEnvData/NarClimRain.asc"),coords)
  # dat$NarClimfTmax<-extract(raster("AppEnvData/NarClimTmax.asc"),coords)
  
  # #reclassify rainfall variability - ranges defined on BOM website
  # dat$rainVar[dat$rainVar > 0 & dat$rainVar <= 0.5] <- "Low"
  # dat$rainVar[dat$rainVar > 0.5 & dat$rainVar <= 0.75] <- "Low to mod"
  # dat$rainVar[dat$rainVar > 0.75 & dat$rainVar <= 1] <- "Mod"
  # dat$rainVar[dat$rainVar > 1 & dat$rainVar <= 1.25] <- "Mod to high"
  # dat$rainVar[dat$rainVar > 1.25 & dat$rainVar <= 1.5] <- "High"
  # dat$rainVar[dat$rainVar > 1.5 & dat$rainVar <= 2] <- "Very high"
  # dat$rainVar[dat$rainVar > 2 ] <- "Extreme"
  # 
  
  # blo <- as.data.frame(sp.AOO_poly) %>%
  #        mutate(reserved = as.numeric(!is.na(CAPAD))) %>%
  #        group_by(clumps) %>%
  #        dplyr::summarise_at(vars(elev:reserved), mean, na.rm=TRUE)
  # 
  # blo$clump_count <- as.data.frame(sp.AOO_poly) %>% 
  #                    group_by(clumps) %>%
  #                    summarise(n = length(clumps)) %>%
  #                    .$n

  return(as.data.frame(sp.AOO_poly))
}