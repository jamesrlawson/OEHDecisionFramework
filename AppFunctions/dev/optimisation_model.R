require(magrittr)

acabau_env <- readr::read_csv('acabau.csv')
acabau_env$ID <- 1:nrow(acabau_env)
acabau_env$soil <- as.factor(acabau_env$soil)

acabau <- readr::read_csv('acabau_clumps.csv')
acabau$ID <- 1:nrow(acabau)

acabau$protected <- as.numeric(!is.na(acabau$CAPAD))
acabau$suitability <- (acabau$tmax_outsideNiche/max(acabau$tmax_outsideNiche) + acabau$rain_outsideNiche/max(acabau$rain_outsideNiche))/2

acabau.agg <- dplyr::group_by(acabau, clumps) %>% dplyr::summarise(clumpSize = length(clumps),
                                                            suitability = mean(suitability)) %>% as.data.frame()


# get all possible combinations of potential locations

#acabau.agg <- acabau.agg[order(acabau.agg$suitability),]
acabau.agg <- acabau.agg[with(acabau.agg, order(suitability, -clumpSize)),] # order by suitability, choosing largest clumpsize for clumps with identical suitability

# suitables <- length(acabau.agg$suitability[acabau.agg$suitability <= quantile(acabau.agg$suitability, 0.10)])
suitables <- nrow(acabau.agg[acabau.agg$suitability <= quantile(acabau.agg$suitability, 0.33) & acabau.agg$clumpSize > quantile(acabau.agg$clumpSize, 0.5),])
suitables <- ifelse(suitables < 10, 10, suitables)
suitables <- ifelse(suitables > 30, 30, suitables)

# combinations <- as.data.frame(t(combn(acabau.agg[1:10,]$clumps, 5))) 
# combinations <- as.data.frame(t(combn(acabau.agg[1:15,]$clumps, 5))) 

# combinations <- as.data.frame(t(combn(acabau.agg[1:suitables,]$clumps, 5))) # for use with loop
combinations <- combn(acabau.agg[1:suitables,]$clumps, 5, simplify=FALSE) #for use with future_lapply


# remove duplicate sets (with any row order)
#combinations <- combinations[!duplicated(t(apply(combinations,1,sort))),] 

get_setSuitability <- function(x) {
  acabau_ <- acabau %>% dplyr::group_by(clumps) %>% dplyr::summarise(suitability = mean(suitability, na.rm=TRUE))
  acabau_[acabau_$clumps %in% x,]$suitability %>% mean(.,na.rm=TRUE)
}

get_setStability <- function(x) {
  
  acabau_ <- acabau %>% dplyr::group_by(clumps) %>% dplyr::summarise(protected = mean(protected, na.rm=TRUE),
                                                              clump_size = length(clumps))
  set.stability <- acabau_[acabau_$clumps %in% x,] %>%
                    dplyr::mutate(stability = (protected + clump_size/max(acabau_$clump_size))/2)
                    # mutate(stability = protected)
  sum(set.stability$stability, na.rm=TRUE)
}

get_setGowdis <- function(x) {
  
   set.envdat <- acabau_env[acabau_env$clumps %in% x,c(3:7)]
   # set.envdat <- acabau_env[acabau_env$clumps %in% x,c(4:7)]
  
  set.gowdis <- cluster::daisy(set.envdat, metric='gower', stand=TRUE) # 3:7 are the environmental variables we're currently using
  
  groups <- factor(c(rep(1,nrow(as.matrix(set.gowdis))))) # need this for next calculation
  disp <- vegan::betadisper(set.gowdis, group = groups, bias.adjust=TRUE) # calculate distances from multivariate centroid
 # mean(disp$distances) # calculate mean distance to centroid
  # or calculate representativeness across bins between 0-1
  # bin disp$distance into 10 bins, calculate fraction of bins represented
  # length(unique(cut(disp$distances, breaks=seq(0,1,by=0.05), right = FALSE)))/20
  
  # or both!
  # mean(disp$distances) * length(unique(cut(disp$distances, breaks=seq(0,1,by=0.05), right = FALSE)))/20
  
  # or distance range?
  #max(disp$distances)-min(disp$distances)
  # or var(disp$distances)
  var(disp$distances)
  
}

containsProtected <- function(x) {
  acabau_ <- acabau %>% dplyr::group_by(clumps) %>% dplyr::summarise(protected = mean(protected, na.rm=TRUE))
  acabau_ <- acabau_[acabau_$clumps %in% x,] 
  mean(acabau_$protected)
}

clumpSize <- function(x) {
  acabau_ <- acabau %>% dplyr::group_by(clumps) %>% dplyr::summarise(clumpSize = length(clumps))
  acabau_ <- acabau_[acabau_$clumps %in% x,] 
  mean(acabau_$clumpSize)
}


# blax.list <- vector("list", length = nrow(combinations))
# # blax <- data.frame(setID = rep(NA, nrow(combinations)))
# for(i in 1:nrow(combinations)) {
#   blax <- data.frame(setID = rep(NA, 1))
#   print(round(i/length(blax.list),5))
#   # blax$setID[i] <- i
#   # blax$set.suitability[i] <- get_setSuitability(combinations[i,])
#   # blax$set.stability[i] <- get_setStability(combinations[i,])
#   # blax$set.gowdis[i] <- get_setGowdis(combinations[i,])
#   # blax$frac_protected[i] <- containsProtected(combinations[i,])
#   # blax$mean_clumpSize[i] <- clumpSize(combinations[i,])
#   blax$setID <- i
#   blax$set.suitability <- get_setSuitability(combinations[i,])
#   blax$set.stability <- get_setStability(combinations[i,])
#   blax$set.gowdis <- get_setGowdis(combinations[i,])
#   #blax$frac_protected <- containsProtected(combinations[i,])
#   #blax$mean_clumpSize <- clumpSize(combinations[i,])
#   
#   blax.list[[i]] <- blax
# }
# 
# blax <- do.call('rbind', blax.list)


blax <- data.frame(matrix(NA, length(combinations)))
blax$setID <- 1:length(combinations)
blax$set.suitability <- unlist(future.apply::future_lapply(t(combinations), get_setSuitability))
blax$set.stability <- unlist(future.apply::future_lapply(t(combinations), get_setStability))
blax$set.gowdis <- unlist(future.apply::future_lapply(t(combinations), get_setGowdis))



# 
# blax.list <- as.list(data.frame(t(combinations)))
# 
# purrr::map(t(combinations), get_setStability)
# 
# blax.list[1:100]

blax$set.suitability_ <- 1 - (blax$set.suitability/max(blax$set.suitability)) %>% round(5)
blax$set.suitability_[blax$set.suitability_ %in% NaN] <- 1
blax$set.suitability_ <- blax$set.suitability_ / max(blax$set.suitability_)
blax$set.stability_ <- blax$set.stability/max(blax$set.stability)
blax$set.gowdis_ <- blax$set.gowdis/max(blax$set.gowdis)

blax$final <- blax$set.suitability_ * blax$set.stability_ * (blax$set.gowdis_)

blax$final_ <- blax$final / max(blax$final)


# dplyr::top_n(blax, 10, set.stability_) %>% dplyr::arrange(desc(final))
# 
# 
# x = seq(0.1,1,0.1)
# y = 1/(x + 1)

# plot(x,y)

# z <- combinations[19,] 
# z <- combinations[9,] 
# z <- combinations[129,] 
# z <- combinations[4,] 
# z <- combinations[169,] 

set.top <- blax[blax$final_ ==  max(blax$final_),]$setID
#set.top <- set.top - 1
# set.top = 2070
# set.top = 4029
#set.top = 6031

z <- combinations[set.top,]

a <- acabau[acabau$clumps %in% z,]
b <- acabau_env[acabau_env$clumps %in% z,]

####### plot maps and histograms #######

sp<-"Syzygium paniculatum"
spdat<-readr::read_csv("AppEnvData/SpeciesObservations/SOSflora.csv") %>%
  filter(Scientific %in% sp)

source('AppFunctions/plotEnviroHists.R')
source('AppFunctions/extractEnviroData.R')


sp.AOO <- getAOOraster(spdat, 1) %>%
  clump(directions=8)

# get cell count for each clump
# table(as.matrix(bla.ras.clumped))

# convert raster to polygons
sp.AOO_poly<- rasterToPolygons(sp.AOO)

#######

plot(env.reproj(raster("AppEnvData/elev.asc", crs=CRS("+init=epsg:4326")), sp.AOO))
plot(sp.AOO_poly, col='darkgrey', border='darkgrey', add=TRUE)

nrow(acabau.agg[acabau.agg$suitability <= quantile(acabau.agg$suitability, 0.33) & acabau.agg$clumpSize > quantile(acabau.agg$clumpSize, 0.5),])

 acabau_allsuitable <- acabau.agg[acabau.agg$suitability %in% c(min(acabau.agg[1:suitables,]$suitability):max(acabau.agg[1:suitables,]$suitability)),]
#acabau_allsuitable <- acabau.agg[acabau.agg$suitability %in% acabau.agg[1:64,]$suitability,]


combinations_allsuitable <- combn(acabau_allsuitable$clumps, 5)
y <- unique(reshape2::melt(combinations_allsuitable)$value) 
  
plot(sp.AOO_poly[sp.AOO_poly$clumps %in% y,], col='blue', border='blue', add=TRUE)
plot(sp.AOO_poly[sp.AOO_poly$clumps %in% z,], col='red', border='red', add=TRUE) # selected sites


# acabau_allsuitable <- acabau.agg[acabau.agg$suitability %in% c(min(acabau.agg$suitability):max(acabau.agg$suitability)),]
# y <- unique(reshape2::melt(combinations_allsuitable)$value) 
# plot(sp.AOO_poly, fill='blue', border='blue')
# plot(sp.AOO_poly[sp.AOO_poly$clumps %in% y,], fill='orange', border='orange', add=TRUE)

z <- combinations[set.top,]

a <- acabau[acabau$clumps %in% z,]
b <- acabau_env[acabau_env$clumps %in% z,]

allSite <- acabau_env
sosSite <- b

CurClimPlot(allSite,sosSite)




sp<-"Acacia gordonii"
spdat<-readr::read_csv("AppEnvData/SpeciesObservations/SOSflora.csv") %>%
       filter(Scientific %in% sp)

require(dplyr)
bla.ras <- getAOOraster(spdat)
bla.ras.clumped <- clump(bla.ras, directions=8)

# get cell count for each clump
# table(as.matrix(bla.ras.clumped))

# convert raster to polygons
sp.AOO_poly<- rasterToPolygons(bla.ras.clumped)

# dat$elev <- env.reproj(raster("AppEnvData/elev.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
#   extract(., sp.AOO_poly, fun=mean) %>% as.vector(.)
# 
# sp.AOO_poly <- env.reproj(raster("AppEnvData/elev.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
#   extract(., sp.AOO_poly, fun=mean, sp=TRUE)
# 
# sp.AOO_poly <- env.reproj(raster("AppEnvData/rain.asc", crs=CRS("+init=epsg:4326")), sp.AOO) %>%
#   extract(., sp.AOO_poly, fun=mean, sp=TRUE)
# sp.AOO_poly
# 
# dat <- sp.AOO_poly %>% as.data.frame(.)



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


















blaz <- data.frame(gowdis=rep(NA,ncol(bla)))

for(i in 1:ncol(bla)) {
  
  blaz$gowdis <- get_gowdis(bla[,i])
  
}





# Make Matrix  & Data frame 
My.Matrix <- matrix(data=rep(c(1,2,3),each=3), ncol=3)
My.DF <- data.frame(My.Matrix)

# Example 1 ~  Using a sum() function 
# data argument .x is My.Matrix: can feed in by pipe %>%
# Incorrect – data structure; returns elements
My.Matrix %>% map(.f=sum)

# Correct map returns list (default)
My.Matrix %>% as.data.frame() %>% map(.f=sum)
# Correct map returns dbl
My.DF %>% map_dbl(.f=sum)







bla <- cluster::daisy(acabau_env[acabau_env$ID %in% c(1:3),c(3:7)], metric="gower")

mean(as.vector(bla))



1:10 %>% accumulate(~ .x)
1:10 %>% accumulate(~ .y)


#https://stackoverflow.com/questions/5671149/permute-all-unique-enumerations-of-a-vector-in-r

uniqueperm2 <- function(d) {
  dat <- factor(d)
  N <- length(dat)
  n <- tabulate(dat)
  ng <- length(n)
  if(ng==1) return(d)
  a <- N-c(0,cumsum(n))[-(ng+1)]
  foo <- lapply(1:ng, function(i) matrix(combn(a[i],n[i]),nrow=n[i]))
  out <- matrix(NA, nrow=N, ncol=prod(sapply(foo, ncol)))
  xxx <- c(0,cumsum(sapply(foo, nrow)))
  xxx <- cbind(xxx[-length(xxx)]+1, xxx[-1])
  miss <- matrix(1:N,ncol=1)
  for(i in seq_len(length(foo)-1)) {
    l1 <- foo[[i]]
    nn <- ncol(miss)
    miss <- matrix(rep(miss, ncol(l1)), nrow=nrow(miss))
    k <- (rep(0:(ncol(miss)-1), each=nrow(l1)))*nrow(miss) + 
      l1[,rep(1:ncol(l1), each=nn)]
    out[xxx[i,1]:xxx[i,2],] <- matrix(miss[k], ncol=ncol(miss))
    miss <- matrix(miss[-k], ncol=ncol(miss))
  }
  k <- length(foo)
  out[xxx[k,1]:xxx[k,2],] <- miss
  out <- out[rank(as.numeric(dat), ties="first"),]
  foo <- cbind(as.vector(out), as.vector(col(out)))
  out[foo] <- d
  t(out)
}







dat <- 1:5
install.packages('iterpc')
library(iterpc)
h <- as.data.frame(getall(iterpc(table(dat), order=TRUE)))
h$ID <- sum(apply(h, 1, prod))


combinations <- unique(expand.grid(1:3, 1:3, 1:3))
combinations <- unique(expand.grid(i:(i+2), i:(i+2), i:(i+2)))

combinations[,!duplicated(combinations)]
