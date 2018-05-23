library(rgdal)

source("AppFunctions/extractEnviroData.R", local = T)

sp <- "Acacia baueri subsp. aspera"
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
variablesUSE <- c("soil", "elev", "rain", "tmax", "rainVar")
#clusters<-4

#select site data
coords <- dat[,c("long","lat")]
coordinates(coords) <-c("long","lat")
proj4string(coords)<-crs(sites)

sp <- 'Acacia baueri subsp. aspera'
managmentSite <- sites[sites$SciName == sp,]
dat <- cbind(dat, sp::over(coords,managmentSite,returnList = FALSE))

dat$tmax_future <- dat$tmax + dat$NarClimfTmax
dat$rain_future <- dat$rain * (1 + (dat$NarClimfRain/100))

Env <- dat

futClimPlot(Env,subset(Env,!is.na(Env$SiteName)))

Env_SoS <- subset(Env,!is.na(Env$SiteName))
Env_SoS$loc <- 'SoS'
Env_all <- subset(Env,is.na(Env$SiteName))
Env_all$loc <- 'all'
allDat <- rbind(Env_all, Env_SoS)
#allDat$loc <- as.factor(allDat$loc)


# facet

allDat_long <- tidyr::gather(allDat, 'time', 'val', c('tmax', 'tmax_future'))
allDat_long$cat <- paste(allDat_long$loc, allDat_long$time, sep = "-")


p5<-ggplot(allDat_long, aes(fill=loc))+
  geom_rect(data= subset(allDat_long, time == 'tmax_future')[1,], aes(xmin=max(allDat$tmax), xmax=(round(max(allDat$tmax_future)/0.5)*0.5)+0.25, ymin=-Inf, ymax=Inf), fill='red', alpha = 0.4, inherit.aes = FALSE) +
  geom_histogram(aes(val), binwidth=.50, position="stack", colour = 'black')+
  # geom_histogram(binwidth=.25, position="identity",colour="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=c("#999999", "#E69F00"))+
#  ggtitle("Change in Avg. annual Tmax")+
  # create 
  labs(x=expression(~degree~C),y="Count") +
  facet_grid(time ~ .) +
  xlim(min(raster("AppEnvData/tmax.asc")[], na.rm=TRUE), max(raster("AppEnvData/tmax.asc")[], na.rm=TRUE))

p5


allDat_long <- tidyr::gather(allDat, 'time', 'val', c('rain', 'rain_future'))
allDat_long$cat <- paste(allDat_long$loc, allDat_long$time, sep = "-")


p6<-ggplot(allDat_long, aes(fill=loc))+
  geom_rect(data= subset(allDat_long, time == 'rain_future')[1,], aes(xmin=max(allDat$rain), xmax=(round(max(allDat$rain_future)/50)*50)+25, ymin=-Inf, ymax=Inf), fill='red', alpha = 0.4, inherit.aes = FALSE) +
  geom_histogram(aes(val), binwidth=50, position="stack", colour = 'black')+
  # geom_histogram(binwidth=.25, position="identity",colour="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  #  ggtitle("Change in Avg. annual Tmax")+
  # create 
  labs(x="Rainfall (mm)", y = "Count") +
  facet_grid(time ~ .) +
  xlim(min(raster("AppEnvData/rain.asc")[], na.rm=TRUE), max(raster("AppEnvData/rain.asc")[]), na.rm=TRUE)
p6
