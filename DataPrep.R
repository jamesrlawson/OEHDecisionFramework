
library(rgdal)
library(rgeos)
library(sf)
library(foreign)

############################
#observations
############################
dat.dir<-"/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/Data"

#list of species
sp<-read.csv(paste0(dat.dir,"/SpeciesProfiles/SOSSpeciesList.csv"))
veg<-unique(sp$Species.type)
veg<-veg[c(1:3,5:7,10,13,15,16)]
florasp<-sp[sp$Species.type %in% veg,]

#fauna<-readOGR("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/Data/SpeciesObservations/Bionet Records 2/bionet_threatened_fauna.shp")
flora <- read.dbf("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/Data/SpeciesObservations/Bionet Records/flora/bionet_threatened_flora.dbf")
flora<-unique(flora[flora$Scientific %in% sp$Scientific.name,])

flora2 <- read.table("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/InitialDocuments/DataFromStu/nsw_atlas/Atlas_records_e2f5fbw14y5vmpcyxztsofpu20150505-090120.txt",
                             sep = '\t', header = TRUE, skip = 4, quote = "", comment.char = "",
                             stringsAsFactors = FALSE, na.strings = "\\N")
flora2<-flora2[flora2$ScientificName %in% sp$Scientific.name,]

#check species counts are the same

a<-subset(flora, Scientific == as.character(florasp$Scientific.name)[10])
b<-subset(flora2, ScientificName == as.character(florasp$Scientific.name)[10])





fauna <- read.dbf("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/InitialDocuments/DataFromStu/nsw_atlas/NSWAtlas_Thr_Fauna.dbf")

spObs<-flora[flora$Scientific=="Calystegia affinis",]


#SOS sites
SOS<-readOGR("/Users/daisy/GoogleDrive/Projects/OEHProtectedSpecies/Data/OEHManagmentSites/data/SiteManagedSpecies03112017/SearchResults.shp")
#get a list of all the species there are shapes for
spSOS<-SOS[SOS$SciName=="Calystegia affinis",]

#Map of NSW
AUS<-readOGR("/Users/daisy/GoogleDrive/PhD/Data/Spatial/AustraliaPolygon/STE11aAust.shp")
NSW<-AUS[AUS$STATE_NAME=="New South Wales",]
NSW <- gSimplify(NSW, tol=0.01, topologyPreserve=TRUE)
points(a$Longitude_,a$Latitude_G,col="red", pch=20)
