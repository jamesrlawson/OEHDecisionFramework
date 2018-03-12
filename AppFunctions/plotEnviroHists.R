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
  

  
  #### rainall
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
  
  p3<-ggplot(allDat, aes( fill = loc))+
    geom_bar( aes(rainVar), colour="black") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position="right",
          axis.text.x=element_text(angle=45,hjust=1,vjust=0.5),
          axis.line = element_line(colour = "black"))+
    scale_fill_manual(labels = c("All locations", "Mngm. sites"), values=c("#999999", "#E69F00"))+
    ggtitle("Avg. annual rainfall variability")+
    labs(x="", y="Count",fill="Locations")
  
  
  #elevation
  p4<-ggplot(allDat, aes(x=elev, fill = loc))+
    geom_histogram(binwidth=100,  position="identity", colour="black")+
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
        geom_bar(aes(soil),position = position_identity(), colour="black") +
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
  
  
  return(multiplot(p1, p4, p2, p5, p3,legend,cols=3))

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

