##Sydney Harbour Bathymetry- FOR CATLIN SEAVEIW SURVEY
rm(list = ls())
library(rgdal)
library(raster)
library(maptools)
library(gstat)
library(fields)
library(rgl)
library(rasterVis)



#set working directory


#read in spatial data
sound_rms<- readOGR ("Data", "sh_soundings_not_spc_mga") ##read in the Shapefile
sound_spc<-read.csv("Data/Sydney_Harbour_SPC_5m_grid.csv", header=F)
colnames(sound_spc)<-c("x","y","z")


#append the RMS data to the Sy Port Corp Data via a dataframe merge
rms.df<-data.frame(x=sound_rms$coords.x1,y=sound_rms$coords.x2,z=sound_rms$DEPTH)
sound<-rbind(rms.df,sound_spc)
sound$z<-sound$z * -1 #turn positive to negetives

sound<-SpatialPointsDataFrame(coords=sound[1:2],data=sound[3])
proj4string(sound)<-proj4string(sound_rms)


##just deal with the Outer Port Jackson Estuary
est<- readOGR("Data","Sydney_harbour_estuaries_catchments_4")
est<-spTransform(est,CRS(proj4string(sound)))
est.pj<-est[est$NAMETYPE=="PORT JACKSON ESTUARY",]
inside.pj <- !is.na(over(sound, as(est.pj, "SpatialPolygons")))
sound.pj<-sound[inside.pj, ]
summary(sound.pj)

###Create a raster of sounding data
#set extents of the raster
rast <- raster() 
extent(rast) <- extent(sound.pj)
ncol(rast) <- 1000
nrow(rast) <- 1000

rast.test<-point.rast(rast.test,sound.pj, 1000,1000)

#rasterise the depth data (mean depth in each of the 1000x1000 pixels of the raster)
rast2 <- rasterize(sound.pj, rast.test, sound.pj$z, fun=mean)
pj.rast <- crop(rast2, extent(est.pj))
pj.rast <- mask(pj.rast, est.pj) #mask raster so that only pixels within the PJ polygon are included
plot(pj.rast) #plot the raw data
plot(est.pj, add=T)


####Create an interpolation for depth
ra <- aggregate(pj.rast, 9) ###need to aggregate the data to smaller, more managable size. aggregates every 4 adjoining pixels together
plot(ra)
xy <- data.frame(xyFromCell(ra, 1:ncell(ra)))
v <- getValues(ra)
tps <- Tps(xy, v)

###create a new raster based on the  pj.rast- will be filled by the interpolation
p <- raster(sound.pj)
extent(p)<-extent(sound.pj)
ncol(p)<-1000
nrow(p)<-1000

p <- interpolate(p, tps)  ###interpolate p using the tps model developed above
p <- mask(p, est.pj) ###mask to just the PJ estuary


##plot the results in a one by two format
par(adj=0,cex.main=0.8,  oma=c(0,0,0,0), mfrow=c(2,1), omi=c(0,0,0,0))
plot(p, axes=FALSE, col=colorRampPalette(c("blue", "white", "green"))(255))
title(main="Depths Sydney Harbour (RMS data only)- Thin Spline Interpolation")
plot(est.pj, add=T)


breakpoints <- c(0,4,max(sound.pj$DEPTH))
colors <- c("red","green")
plot(p, axes=FALSE, breaks=breakpoints, col=colors)
title(main="Depth Limitations- 4 m", )
plot(est.pj, add=T)
dev.off()



####CALCULATING THE HABITAT DEPTH SUITABILITY

##Rocky Reef

reef<- readShapePoly("NSW_DPI_Estuarine_Reef")
proj4string(reef)<-proj4string(sound)
summary(reef)

reef.rast <- raster() ##Creat Same raster as depth profile
extent(reef.rast) <- extent(sound.pj)
ncol(reef.rast) <- 1000
nrow(reef.rast) <- 1000

reef.rs <- rasterize(reef, reef.rast, reef$Substrate) ##rasterise DPI polygons to same raster as Depth

##mask the raster just to Port Jackson
pj.reef.rs <- mask(reef.rs, est.pj)
plot(pj.reef.rs, legend=TRUE)
plot(est.pj,add=T)
plot(p, axes=FALSE, breaks=breakpoints, col=colors)

##raster arithmetic to work out the depth>4m and rocky reef present.
rd<-reef.rs==1 & p<= -4
rd[rd!=1] <- NA
plot(rd, legend=F)
plot(est.pj, add=T)
title(main="Dive Site Suitability (>4 m Depth with Rocky Reef Present)")

#polygonise the raster for possible plotting in deducer spatial
rd.poly<-rasterToPolygons(rd, fun=function(rd){rd==1})
plot(rd.poly)

pdf(file="Sydney_Harbour_Bathymetry.pdf", width=16, height=11)
par(adj=0,cex.main=0.8,  oma=c(0,0,0,0), omi=c(0,0,0,0))
plot(p, axes=FALSE, col=colorRampPalette(c("blue", "white", "green"))(255))
title(main="Depths Sydney Harbour (RMS data only)- Thin Spline Interpolation")
plot(est.pj, add=T)
dev.off()

pdf(file="Sydney_Harbour_Depth_dive_suitability.pdf", width=16, height=11)
plot(rd, legend=F)
plot(est.pj, add=T)
title(main="Dive Site Suitability (>4 m Depth with Rocky Reef Present)")
dev.off()


##Macrophytes

macro<- readShapePoly("NSW_DPI_Estuarine_Macrophytes")
proj4string(macro)<-proj4string(sound)
macro<-macro[which(macro$HABITAT=="Seagrass"),] ###just want seagrass cover
writeOGR(macro, dsn="Bathymetry",layer="SH_Seagrass_extent", driver="ESRI Shapefile") ##just want seagrass data
macro.1<-readShapePoly("SH_Seagrass_extent") #just want seagrass cover

macro.rast <- raster() ##Creat Same raster as depth profile
extent(macro.rast) <- extent(sound.pj)
ncol(macro.rast) <- 1000
nrow(macro.rast) <- 1000

macro.rs<-rasterize(macro.1, macro.rast, macro$HABITAT)
pj.macro.rs<-mask(macro.rs,est.pj)
plot(pj.macro.rs)
sea.dep<-pj.macro.rs==3 & p>4
plot(sea.dep)
plot(est.pj, add=T)

plot3D(p, col=colorRampPalette(c("blue", "white", "green"))(255))


######################

gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}
gcd.slc(long1=315824.1, lat1=6250185.1,long2=342731.2,lat2=6250185.1)

rdist.earth ( matrix(c(315824.1,6250185.1),ncols=2)), matrix(c(342731.2,6250185.1),ncols=2)), miles = FALSE, R = NULL)
