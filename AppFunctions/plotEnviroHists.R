#make histograms of environmental data
#TODO: CHECK that code works when there are no SOS sites, plotting still works

##Test data
# library(rgdal)
# source("AppFunctions/extractEnviroData.R", local = T)
# sp<-"Acacia acanthoclada"
# spdat<-read.csv("AppEnvData/SpeciesObservations/SOSflora.csv",header=TRUE)
# spdat<-subset(spdat,Scientific==sp)
# sites<-readOGR("AppEnvData/ManagmentSites/OEHManagmentSites.shp")
# 
# spdat$lat <- spdat[, "Latitude_G"]
# spdat$long <- spdat[, "Longitude_"]
# dat<-EnvExtract(spdat$lat,spdat$long)
# 
# #select site data
# coords <- dat[,c("long","lat")]
# coordinates(coords) <-c("long","lat")
# proj4string(coords)<-crs(sites)
# 
# managmentSite <- sites[sites$SciName == sp,]
# dat<-cbind(dat,over(coords,managmentSite,returnList = FALSE))
# 
# allSite<-dat
# sosSite<-subset(dat,!is.na(dat$SiteName))





source("AppFunctions/MultiplotLayout.R", local = T)



CurClimPlot<-function(allSite,sosSite){
  
  #add column to describe bins
  allSite$loc <- "all"
  sosSite$loc <- "sos"
  allDat <- rbind(allSite, sosSite)
  
  
  ###tmax  
  
  
  p1<-ggplot(allDat, aes(x=tmax, fill = loc))+
    geom_histogram(binwidth = .5, position="identity", colour="black")+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    scale_fill_manual( values = c("#999999", "#E69F00"))+
    ggtitle("Avg. annual Tmax")+
    labs(x=expression(~degree~C),y=  "Count")
  
  
  
  #### rainfall
  p2<-ggplot(allDat, aes(x=rain,y=..count.., fill = loc))+
    geom_histogram(binwidth=50, position="identity", colour="black")+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    scale_fill_manual( values=c("#999999", "#E69F00"))+
    ggtitle("Avg. annual rainfall")+
    labs(x="Rainfall (mm)", y = "Count")
  
  #rainfall variability
  
  # p3<-ggplot(allDat, aes( fill = loc))+
  #   geom_bar( aes(rainVar), colour=NA) +
  #   theme(panel.grid.major = element_blank(), 
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_blank(),
  #         legend.position="none",
  #         axis.text.x=element_text(angle=45,hjust=1,vjust=0.5),
  #         axis.line = element_line(colour = "black"))+
  #   scale_fill_manual(labels = c("All locations", "Mngm. sites"), values=c("#999999", "#E69F00"))+
  #   ggtitle("Avg. annual rainfall variability")+
  #   labs(x="", y="Count",fill="Locations")
  
  #rainfall variability
  p3<-ggplot(allDat, aes(x=rainVar, fill = loc))+
    geom_histogram(binwidth=0.5,  position="identity", colour=NA)+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    ggtitle("Avg. annual rainfall variability")+
    labs(x="", y="Count",fill="Locations")
    
  
  #elevation
  p4<-ggplot(allDat, aes(x=elev, fill = loc))+
    geom_histogram(binwidth=100,  position="identity", colour=NA)+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    ggtitle("Elevation")+
    labs(x="Elevation (m)", y="Count")
  
  
  
  #soil types
  
  p5<-ggplot(allDat, aes( fill = loc))+
    geom_bar(aes(soil),position = position_identity(), colour=NA) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=45,hjust=1,vjust=0.5),
          axis.line = element_line(colour = "black"))+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    ggtitle("Soil type")+
    labs(x="",y="Count")
  
  # #legend only
  # p6a<-ggplot(allDat, aes(x=rainVar, fill = loc))+
  #   #geom_density(alpha = 0.5)+
  #   geom_histogram(binwidth=.1, position="identity",breaks=c(0,.5,.75,1,1.25,1.5,2,3))+
  #   theme(panel.grid.major = element_blank(), 
  #         panel.grid.minor = element_blank(),
  #         panel.background = element_blank(), 
  #         axis.line = element_line(colour = "black"))+
  #   scale_fill_manual( labels = c("All locations", "Mngm. sites"), values=c("#999999", "#E69F00"))+
  #   ggtitle("Avg. annual rainfall variability")+
  #   labs(x="Index of variability")
  # legend<-g_legend(p6a)
  
  
  # facet
  
  
  #browser()
  
  allDat_long <- tidyr::gather(allDat, 'time', 'val', c('tmax', 'tmax_future'))
  allDat_long$cat <- paste(allDat_long$loc, allDat_long$time, sep = "-")
  
  tmax.ras <- raster::raster("AppEnvData/tmax.asc")
  
  p6<-ggplot(allDat_long, aes(fill=loc))+
    geom_histogram(aes(val), binwidth=.50, position="stack", colour = NA)+
    geom_rect(data= subset(allDat_long, time == 'tmax_future')[1,], aes(xmin=max(allDat$tmax), xmax=(round(max(allDat$tmax_future)/0.5)*0.5)+0.25, ymin=-Inf, ymax=Inf), fill='red', alpha = 0.4, inherit.aes = FALSE) +
    # geom_histogram(binwidth=.25, position="identity",colour="black")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = 'none')+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    ggtitle("Current and future (2069) daily max temp.")+
    labs(x=expression(~degree~C),y="Count") +
    # xlim(c(min(tmax.ras[], na.rm=TRUE), max(tmax.ras[], na.rm=TRUE))) +
    facet_grid(time ~ .)
  
  
  allDat_long <- tidyr::gather(allDat, 'time', 'val', c('rain', 'rain_future'))
  allDat_long$cat <- paste(allDat_long$loc, allDat_long$time, sep = "-")
  rain.ras <- raster("AppEnvData/rain.asc")
  
  p7<-ggplot(allDat_long, aes(fill=loc))+
    geom_histogram(aes(val), binwidth=50, position="stack", colour = NA)+
    geom_rect(data= subset(allDat_long, time == 'rain_future')[1,], aes(xmin=max(allDat$rain), xmax=(round(max(allDat$rain_future)/50)*50)+25, ymin=-Inf, ymax=Inf), fill='red', alpha = 0.4, inherit.aes = FALSE) +
    # geom_histogram(binwidth=.25, position="identity",colour="black")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = 'none')+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    ggtitle("Current and future (2069) annual rainfall")+
    labs(x="Rainfall (mm)", y = "Count") +
    # xlim(c(min(rain.ras[], na.rm=TRUE), max(rain.ras[], na.rm=TRUE))) +
    facet_grid(time ~ .) 
  
  # return(multiplot(p6, p4, p5, p7, p3,cols=2))
  
  gs <- c(p6,p7,p3,p4,p5)
  
  lay <- rbind(c(1,2),
               c(1,2),
               c(1,2),
               c(3,4),
               c(3,4),
               c(5,NA),
               c(5,NA))
  return(grid.arrange(p6,p7,p3,p4,p5, layout_matrix = lay))
  
}


###############################
#other variables


futClimPlot<-function(allSite,sosSite){
  
  #add column to describe bins
  allSite$loc <- "all"
  sosSite$loc <- "sos"
  allDat <- rbind(allSite, sosSite)
  
  p1<-ggplot(allDat, aes(x= NarClimfTmax, fill = loc))+
    geom_histogram(binwidth=.25, position="identity",colour="black")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    ggtitle("Change in Avg. annual Tmax")+
    labs(x=expression(~degree~C),y="Count")
  
  p2<-ggplot(allDat, aes(x=NarClimfRain, fill = loc))+
    geom_histogram(binwidth=5,  position="identity",colour="black")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    ggtitle("Change in Avg. annual rainfall (%)")+
    labs(x="%",y="Count")
  
  p3<-ggplot(allDat, aes(x=NarClimffdigt50, fill = loc))+
    geom_histogram(binwidth=1, position="identity",colour="black")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none")+
    scale_fill_manual(values=c("#999999", "#E69F00"))+
    ggtitle("Change # of days with FFDI > 50")+
    labs(x="Days", y="Count")
  
  return(multiplot(p1, p2, p3,cols=3))
  
}

